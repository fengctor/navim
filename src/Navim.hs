{-# LANGUAGE OverloadedStrings #-}

module Navim where

import System.Directory
import System.Exit

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
import Brick.Widgets.Core

import Cursor.Simple.List.NonEmpty

import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events

-- Main
navim :: IO ()
navim = do initState <- buildState Nothing
           endState  <- defaultMain navimApp initState
           let navPath = mconcat $ intersperse "/" $ reverse $ navHistory endState
           putStrLn navPath

-- TODO: - add "Mode" sum type and record
--       - draw dialog and handle input based on current Mode
data NavimState =
  NavimState
    { navimStatePaths :: NonEmptyCursor DirContent
    , navHistory :: [FilePath]
    , commandRev :: String
    } deriving (Show, Eq)

data ResourceName = ResourceName deriving (Show, Eq, Ord)

data DirContent = File FilePath | Directory FilePath deriving (Show, Eq, Ord)

getPath :: DirContent -> FilePath
getPath (File fp)      = fp
getPath (Directory fp) = fp

nonEmptyCursorReset :: NonEmptyCursor a -> NonEmptyCursor a
nonEmptyCursorReset = makeNonEmptyCursor . rebuildNonEmptyCursor

-- TUI App Components
navimApp :: App NavimState e ResourceName
navimApp =
  App
    { appDraw = drawNavim
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleEvent
    , appStartEvent = pure
    , appAttrMap = const $ attrMap mempty [ ("selected", withStyle (fg green) underline)
                                          , ("file", fg white)
                                          , ("dir", fg brightCyan)
                                          , ("header", fg green)
                                          ]
    }


-- State Transformer
buildState :: Maybe NavimState -> IO NavimState
buildState prevState =
  do curdir      <- getCurrentDirectory
     rawContents <- getDirectoryContents curdir
     contents    <- for rawContents $ \fp ->
                      do isFile <- doesFileExist fp
                         return $ if isFile then File fp
                                            else Directory fp
     case NE.nonEmpty contents of
          Nothing -> die "Should never happen (current directory \".\" always here)"
          Just ne -> return $
                       case prevState of
                            Nothing -> NavimState
                                         { navimStatePaths = makeNonEmptyCursor ne
                                         , navHistory = []
                                         , commandRev = ""
                                         }
                            Just ps -> ps {navimStatePaths = adjustCursor
                                                               (navimStatePaths ps)
                                                               (makeNonEmptyCursor ne)}
  where adjustCursor oldNec = moveNextBy (length $ nonEmptyCursorPrev oldNec)
        moveNextBy 0 newNec = newNec
        moveNextBy n newNec = case nonEmptyCursorSelectNext newNec of
                                   Nothing   -> newNec
                                   Just nec' -> moveNextBy (n - 1) nec'

-- UI Drawer
drawNavim :: NavimState -> [Widget ResourceName]
drawNavim ns = [
                 border (padRight Max $ withAttr "header" (str "navim - a (WIP) file manager written in Haskell by Gary Feng"))
                 <=>
                 border (padBottom Max pathsWidget)
                 <=>
                 str (reverse $ commandRev ns)
               ]
  where pathsCursor = navimStatePaths ns
        pathsWidget = padRight Max $ vBox $ mconcat
                         [ drawFilePath False <$> reverse (nonEmptyCursorPrev pathsCursor)
                         , [drawFilePath True $ nonEmptyCursorCurrent pathsCursor]
                         , drawFilePath False <$> nonEmptyCursorNext pathsCursor
                         ]

drawFilePath :: Bool -> DirContent -> Widget n
drawFilePath selected dc = decorate $ str $ getPath dc
-- TODO: how to mix attributes
  where decorate =
          case dc of
               File      _ -> withAttr $ (if selected then ("selected" <>) else id) "file"
               Directory _ -> withAttr $ (if selected then ("selected" <>) else id) "dir"

-- BEGIN Event Handler Helpers

moveCursorWith :: (NonEmptyCursor DirContent -> Maybe (NonEmptyCursor DirContent)) ->
                  NavimState ->
                  EventM n (Next NavimState)
moveCursorWith move state = continue $
  case move (navimStatePaths state) of
       Nothing     -> state
       Just newNec -> state {navimStatePaths = newNec}

performNavigate :: NavimState -> EventM n (Next NavimState)
performNavigate s = case nonEmptyCursorCurrent $ navimStatePaths s of
                         File      _  -> continue s
                         Directory fp ->
                           do liftIO $ setCurrentDirectory fp
                              let newHistory = case (navHistory s, fp) of
                                                    (ps  , "." ) -> ps
                                                    ([]  , "..") -> [".."]
                                                    ("..":ps, "..") -> "..":"..":ps
                                                    (p:ps, "..") -> ps
                                                    (ps  , next) -> next:ps
                              s' <- liftIO $ buildState $
                                      Just s {navimStatePaths = nonEmptyCursorReset (navimStatePaths s)}
                              continue $ s' {navHistory = newHistory}

colonCommand :: NavimState -> EventM n (Next NavimState)
colonCommand s = case reverse $ commandRev s of
                      ":q" -> halt s
                      ':':'d':'i':'r':' ':dirName -> do liftIO $ createDirectory dirName
                                                        s' <- liftIO $ buildState $ Just s
                                                        continue s' {commandRev = []}
                      _    -> continue s {commandRev = []}

-- END Event Handler Helpers

-- Event Handler
handleEvent :: NavimState -> BrickEvent n e -> EventM n (Next NavimState)
handleEvent s e =
  case e of
       VtyEvent vtye ->
         case vtye of
              EvKey (KChar ':') [] -> continue s {commandRev = ':':commandRev s}

              EvKey key@(KChar 'j') [] -> colonModeOr (moveCursorWith nonEmptyCursorSelectNext s) key
              EvKey key@(KChar 'k') [] -> colonModeOr (moveCursorWith nonEmptyCursorSelectPrev s) key
              EvKey key@(KChar _)   [] -> colonModeOr (continue s) key

              EvKey KDown [] -> moveCursorWith nonEmptyCursorSelectNext s
              EvKey KUp   [] -> moveCursorWith nonEmptyCursorSelectPrev s

              EvKey KBS    [] -> colonModeOr (continue s) KBS
              EvKey KEnter [] -> colonModeOr (performNavigate s) KEnter
              EvKey KEsc   [] -> continue s {commandRev = []}
              _ -> continue s
       _ -> continue s
  where
    colonModeOr :: EventM n (Next NavimState) -> Key -> EventM n (Next NavimState)
    colonModeOr elseAction key =
      case (commandRev s, key) of
           ([], _)       -> elseAction
           (c:cs, KBS)   -> continue s {commandRev = cs}
           (cs, KEnter)  -> colonCommand s
           (cs, KChar c) -> do s' <- liftIO $ buildState $ Just s
                               continue s' {commandRev = c:cs}
           (_, _)        -> continue s
