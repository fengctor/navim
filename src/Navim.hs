{-# LANGUAGE OverloadedStrings #-}

module Navim where

import System.Directory
import System.Exit

import Data.Bool
import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Traversable

import Control.Monad
import Control.Monad.IO.Class

import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Util
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Core

import Cursor.Simple.List.NonEmpty

import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events

-- Main
navim :: IO ()
navim = do
    initState <- buildState Nothing
    endState  <- defaultMain navimApp initState
    let navPath = join $ intersperse "/" $ reverse $ navHistory endState
    putStrLn navPath

-- TODO: - add "Mode" sum type and record
--       - draw dialog and handle input based on current Mode
data NavimState = NavimState
    { navimStatePaths :: NonEmptyCursor DirContent
    , navHistory :: [FilePath]
    , mode :: Mode
    } deriving (Show, Eq)

data Mode
    = Navigation -- normal file navigation
    | Colon      -- colon commands: ends when input is empty or enter pressed
        { colonInputReversed :: String }
    | Input      -- waiting for user input with a given prompt
        { prompt :: String        -- TODO: maybe sum type for prompt?
        , responseInput :: String -- TODO: different data structure for fast snoc?
        }
    deriving (Show, Eq)

data ResourceName
    = ResourceName
    deriving (Show, Eq, Ord)

data DirContent
    = File FilePath
    | Directory FilePath
    deriving (Show, Eq, Ord)

getPath :: DirContent -> FilePath
getPath (File fp)      = fp
getPath (Directory fp) = fp

nonEmptyCursorReset :: NonEmptyCursor a -> NonEmptyCursor a
nonEmptyCursorReset = makeNonEmptyCursor . rebuildNonEmptyCursor

-- TUI App Components
navimApp :: App NavimState e ResourceName
navimApp = App
    { appDraw = drawNavim
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleEvent
    , appStartEvent = pure
    , appAttrMap = const $
          attrMap
              mempty
              [ ("file", fg white)
              , ("dir", fg brightCyan)
              , ("file" <> "selected", withStyle currentAttr underline)
              , ("dir" <> "selected", withStyle (bg green) underline)
              , ("header", fg white)
              ]
    }

-- State Transformer
buildState :: Maybe NavimState -> IO NavimState
buildState prevState = do
    curdir      <- getCurrentDirectory
    rawContents <- getDirectoryContents curdir
    contents    <- for rawContents $ \fp ->
                       bool (Directory fp) (File fp) <$> doesFileExist fp
    case NE.nonEmpty contents of
        Nothing -> die "Should never happen (current directory \".\" always here)"
        Just ne ->
            return $
                case prevState of
                    Nothing -> NavimState
                        { navimStatePaths = makeNonEmptyCursor ne
                        , navHistory = []
                        , mode = Navigation
                        }
                    Just ps -> ps
                        { navimStatePaths =
                              adjustCursor
                                  (navimStatePaths ps)
                                  (makeNonEmptyCursor ne)
                        }
  where
    adjustCursor oldNec = moveNextBy (length $ nonEmptyCursorPrev oldNec)
    moveNextBy 0 newNec = newNec
    moveNextBy n newNec = case nonEmptyCursorSelectNext newNec of
                               Nothing   -> newNec
                               Just nec' -> moveNextBy (n - 1) nec'

-- UI Drawer
drawNavim :: NavimState -> [Widget ResourceName]
drawNavim ns =
    [
      border (padRight Max $
         withAttr "header" (str "navim - a (WIP) file manager written in Haskell by Gary Feng"))
      <=>
      padBottom Max pathsWidget
      <=>
      (padRight Max $
          case mode ns of
              Navigation ->
                  str "-- NAVIGATION --"
              Colon reversedInput ->
                  showCursor
                      ResourceName
                      (Location (textWidth reversedInput, 0)) -- end of colon command string
                      (str . reverse $ reversedInput)
              Input prompt input ->
                  str "TODOTODOTODO"
      )
    ]
  where
    pathsCursor = navimStatePaths ns
    pathsWidget =
        padRight Max
        . viewport ResourceName Vertical
        . vBox
        . mconcat
        $ [ drawFilePath False <$> reverse (nonEmptyCursorPrev pathsCursor)
          , [ visible
              . drawFilePath True
              $ nonEmptyCursorCurrent pathsCursor
            ]
          , drawFilePath False <$> nonEmptyCursorNext pathsCursor
          ]

drawFilePath :: Bool -> DirContent -> Widget n
drawFilePath selected dc = decorate . str . getPath $ dc
-- TODO: how to mix attributes
  where
    decorate =
        withAttr
        . bool id (<> "selected") selected
        $ case dc of
              File      _ -> "file"
              Directory _ -> "dir"

-- Event Handler
handleEvent :: NavimState -> BrickEvent n e -> EventM n (Next NavimState)
handleEvent s e =
    case e of
        VtyEvent vtye ->
            case vtye of
                EvKey key@(KChar ':') [] ->
                    colonModeOr (continue s { mode = Colon ":" }) key
                EvKey key@(KChar 'j') [] ->
                    colonModeOr (moveCursorWith nonEmptyCursorSelectNext s) key
                EvKey key@(KChar 'k') [] ->
                    colonModeOr (moveCursorWith nonEmptyCursorSelectPrev s) key
                EvKey key@(KChar _)   [] ->
                    colonModeOr (continue s) key

                EvKey KDown [] -> moveCursorWith nonEmptyCursorSelectNext s
                EvKey KUp   [] -> moveCursorWith nonEmptyCursorSelectPrev s

                EvKey KBS    [] -> colonModeOr (continue s) KBS
                EvKey KEnter [] -> colonModeOr (performNavigate s) KEnter
                EvKey KEsc   [] -> continue s { mode = Navigation }
                _ -> continue s
        _ -> continue s
  where
    colonModeOr :: EventM n (Next NavimState) -> Key -> EventM n (Next NavimState)
    colonModeOr elseAction key =
        case (mode s, key) of
            (Navigation, _) -> elseAction

            -- TODO: lens please...
            (Colon input, KChar c) ->
                continue s { mode = Colon { colonInputReversed = c:input } }
            (Colon [c], KBS) ->
                continue s { mode = Navigation }
            (Colon (c:cs), KBS) ->
                continue s { mode = Colon { colonInputReversed = cs } } -- Yikes unsafe...
            (Colon cs, KEnter) -> colonCommand s . reverse $ cs

            (Input _ _, _) -> continue s -- TODO!!!!!!!!

            (_, _)        -> continue s

{- BEGIN Event Handler Helpers -}

moveCursorWith :: (NonEmptyCursor DirContent -> Maybe (NonEmptyCursor DirContent))
               -> NavimState
               -> EventM n (Next NavimState)
moveCursorWith move state =
    continue $
        case move (navimStatePaths state) of
            Nothing     -> state
            Just newNec -> state {navimStatePaths = newNec}

performNavigate :: NavimState -> EventM n (Next NavimState)
performNavigate s = case nonEmptyCursorCurrent $ navimStatePaths s of
                        File      _  -> continue s
                        Directory fp -> do
                            liftIO $ setCurrentDirectory fp
                            let newHistory = case (navHistory s, fp) of
                                                 (ps  , "." )    -> ps
                                                 ([]  , "..")    -> [".."]
                                                 ("..":ps, "..") -> "..":"..":ps
                                                 (p:ps, "..")    -> ps
                                                 (ps  , next)    -> next:ps
                            s' <- liftIO . buildState $
                                      Just s { navimStatePaths =
                                                   nonEmptyCursorReset (navimStatePaths s) }
                            continue s' { navHistory = newHistory }

-- TODO: move this to a safe directory ops module?
createDirectorySafe :: FilePath -> IO Bool
createDirectorySafe fp = do
    curdir      <- getCurrentDirectory
    contents    <- getDirectoryContents curdir
    if fp `elem` contents
        then return False
        else True <$ createDirectory fp

colonCommand :: NavimState -> String -> EventM n (Next NavimState)
colonCommand s input =
    case input of
        ":q" -> halt s
        ':':'d':'i':'r':' ':dirName -> do
            success <- liftIO . createDirectorySafe $ dirName
            -- TODO: handle failure
            s'      <- liftIO . buildState $ Just s
            continue s' { mode = Navigation }
        _ -> continue s { mode = Navigation }

{- END Event Handler Helpers -}
