{-# LANGUAGE OverloadedStrings #-}

module Navim where

import System.Directory
import System.Exit

import Data.Bool
import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Traversable

import Control.Lens
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

import Navim.DirContent

-- Main
navim :: IO ()
navim = do
    initState <- buildState Nothing
    endState  <- defaultMain navimApp initState
    let navPath = mconcat . intersperse "/" . reverse $ endState ^. navimHistory
    putStrLn navPath

data NavimState = NavimState
    { _navimStatePaths :: NonEmptyCursor DirContent
    , _navimHistory :: [FilePath]
    , _navimMode :: Mode
    } deriving (Show, Eq)

navimStatePaths :: Lens' NavimState (NonEmptyCursor DirContent)
navimStatePaths = lens _navimStatePaths (\ns p -> ns { _navimStatePaths = p })

navimHistory :: Lens' NavimState [FilePath]
navimHistory = lens _navimHistory (\ns h -> ns { _navimHistory = h })

navimMode :: Lens' NavimState Mode
navimMode = lens _navimMode (\ns m -> ns { _navimMode = m })

-- TODO: something about prisms for this
data Mode
    = Navigation -- normal file navigation
    | Colon      -- colon commands: ends when input is empty or enter pressed
        { _colonInput :: String }
    | Input      -- waiting for user input with a given prompt
        { _prompt :: Prompt        -- TODO: maybe sum type for prompt?
        , _inputResponse :: String -- TODO: different data structure for fast snoc?
        }
    deriving (Show, Eq)

data Prompt
    = CreateFile
    | CreateDirectory
    | Remove
    | Rename
    deriving (Show, Eq)

data ResourceName
    = ResourceName
    deriving (Show, Eq, Ord)


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

-- State Transformer (and I don't mean the monad ;))
buildState :: Maybe NavimState -> IO NavimState
buildState prevState = do
    contents <- getCurrentDirContents
    case NE.nonEmpty contents of
        Nothing -> die "Should never happen (current directory \".\" always here)"
        Just ne ->
            case prevState of
                Nothing -> do
                    curDir <- getCurrentDirectory
                    return NavimState
                        { _navimStatePaths = makeNonEmptyCursor ne
                        , _navimHistory = reverse . splitOn '/' $ curDir
                        , _navimMode = Navigation
                        }
                Just ps -> return $
                               ps & navimStatePaths
                                  %~ adjustCursor (makeNonEmptyCursor ne)
  where
    adjustCursor newNec oldNec = moveNextBy (length $ nonEmptyCursorPrev oldNec) newNec
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
      statusBar
      <=>
      inputBar
    ]
  where
    pathsCursor = ns ^. navimStatePaths

    pathsWidget =
        padRight Max
        . viewport ResourceName Vertical
        . vBox
        . mconcat
        $ [ drawDirContent False <$> reverse (nonEmptyCursorPrev pathsCursor)
          , [ visible
              . drawDirContent True
              $ nonEmptyCursorCurrent pathsCursor
            ]
          , drawDirContent False <$> nonEmptyCursorNext pathsCursor
          ]

    statusBar = str $
        case ns ^. navimMode of
            Input CreateFile _ ->
                "Enter the name of the file to be created"
            Input CreateDirectory _ ->
                "Enter the name of the directory to be created"
            Input Remove _ ->
                case nonEmptyCursorCurrent pathsCursor of
                    File name ->
                        mconcat
                            [ "Are you sure you want to remove the file "
                            , name
                            , "?"
                            ]
                    Directory "." ->
                        "You may not remove the current directory from within."
                    Directory ".." ->
                        "You may not remove the parent directory from within."
                    Directory name ->
                        mconcat
                            [ "Are you sure you want to remove the directory "
                            , name
                            , "?"
                            ]
            Input Rename _ ->
                case nonEmptyCursorCurrent pathsCursor of
                    File name ->
                        mconcat
                            [ "Enter the new name for the file "
                            , name
                            , "."
                            ]
                    Directory "." ->
                        "You may not rename the current directory from within."
                    Directory ".." ->
                        "You may not rename the parent directory from within."
                    Directory name ->
                        mconcat
                            [ "Enter the new name for the directory "
                            , name
                            , "."
                            ]

            _ -> ('/':) . mconcat . intersperse "/" . reverse $ ns ^. navimHistory

    inputBar =
        padRight Max $
            case ns ^. navimMode  of
                Navigation ->
                    str "-- NAVIGATION --"
                Colon reversedInput ->
                    withBottomCursor reversedInput
                Input CreateFile reversedInput ->
                    withBottomCursor $ reversedInput ++ reverse "File name: "
                Input CreateDirectory reversedInput ->
                    withBottomCursor $ reversedInput ++ reverse "Directory name: "
                Input Remove reversedInput ->
                    withBottomCursor $ reversedInput ++ reverse "Confirm (y/n): "
                Input Rename reversedInput ->
                    withBottomCursor $ reversedInput ++ reverse "New name: "

    withBottomCursor reversedInput =
        showCursor
            ResourceName
            (Location (textWidth reversedInput, 0))
            (str . reverse $ reversedInput)

drawDirContent :: Bool -> DirContent -> Widget n
drawDirContent selected dc = decorate . str . getPath $ dc
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
                    bottomInputOr (continue $ s & navimMode .~ Colon ":") key
                EvKey key@(KChar 'j') [] ->
                    bottomInputOr (moveCursorWith nonEmptyCursorSelectNext s) key
                EvKey key@(KChar 'k') [] ->
                    bottomInputOr (moveCursorWith nonEmptyCursorSelectPrev s) key
                EvKey key@(KChar 'g') [] ->
                    bottomInputOr (moveCursorWith (Just . nonEmptyCursorSelectFirst) s) key
                EvKey key@(KChar 'G') [] ->
                    bottomInputOr (moveCursorWith (Just . nonEmptyCursorSelectLast) s) key
                EvKey key@(KChar 'n') [] ->
                    bottomInputOr (continue $ s & navimMode .~ Input CreateFile "") key
                EvKey key@(KChar 'n') [MMeta] ->
                    bottomInputOr (continue $ s & navimMode .~ Input CreateDirectory "") key
                EvKey key@(KChar 'd') [] ->
                    bottomInputOr (continue $ s & navimMode .~ Input Remove "") key
                EvKey key@(KChar 'r') [] ->
                    bottomInputOr (continue $ s & navimMode .~ Input Rename "") key
                EvKey key@(KChar _)   [] ->
                    bottomInputOr (continue s) key

                EvKey KDown [] -> moveCursorWith nonEmptyCursorSelectNext s
                EvKey KUp   [] -> moveCursorWith nonEmptyCursorSelectPrev s

                EvKey KBS    [] -> bottomInputOr (continue s) KBS
                EvKey KEnter [] -> bottomInputOr (performNavigate s) KEnter
                EvKey KEsc   [] -> continue $ s & navimMode .~ Navigation
                _ -> continue s
        _ -> continue s
  where
    bottomInputOr :: EventM n (Next NavimState) -> Key -> EventM n (Next NavimState)
    bottomInputOr navAction key =
        case (s ^. navimMode, key) of
            (Navigation, _) -> navAction

            -- TODO: lens please...
            (Colon input, KChar c) ->
                continue $ s & navimMode .~ Colon { _colonInput = c:input }
            (Colon [c], KBS) ->
                continue $ s & navimMode .~ Navigation
            (Colon (c:cs), KBS) ->
                continue $ s & navimMode .~ Colon { _colonInput = cs }
            (Colon cs, KEnter) ->
                colonCommand s . reverse $ cs

            (inMode@(Input _ resp), KChar c) ->
                continue $ s & navimMode .~ inMode { _inputResponse = c:resp }
            (inMode@(Input _ (c:cs)), KBS) ->
                continue $ s & navimMode .~ inMode { _inputResponse = cs }
            (inMode@(Input _ ""), KBS) ->
                continue $ s & navimMode .~ inMode { _inputResponse = "" }

            (Input Remove inp, KEnter) ->
                case nonEmptyCursorCurrent $ s ^. navimStatePaths of
                    Directory "."  -> continue $ s & navimMode .~ Navigation -- TODO: no silent failure pls
                    Directory ".." -> continue $ s & navimMode .~ Navigation -- TODO: no silent failure pls
                    _              -> inputCommand s Remove . reverse $ inp
            (Input Rename inp, KEnter) ->
                case nonEmptyCursorCurrent $ s ^. navimStatePaths of
                    Directory "."  -> continue $ s & navimMode .~ Navigation -- TODO: no silent failure pls
                    Directory ".." -> continue $ s & navimMode .~ Navigation -- TODO: no silent failure pls
                    _              -> inputCommand s Rename . reverse $ inp

            (Input pr inp, KEnter) ->
                inputCommand s pr . reverse $ inp

            (Input _ _, _) -> continue s -- TODO!!!!!!!!

            (_, _)        -> continue s

{- BEGIN Event Handler Helpers -}

moveCursorWith :: (NonEmptyCursor DirContent -> Maybe (NonEmptyCursor DirContent))
               -> NavimState
               -> EventM n (Next NavimState)
moveCursorWith move ns =
    continue $
        case move $ ns ^. navimStatePaths of
            Nothing     -> ns
            Just newNec -> ns & navimStatePaths .~ newNec

-- TODO: move cursor to parent directory when navigating on ".."
performNavigate :: NavimState -> EventM n (Next NavimState)
performNavigate ns =
    case nonEmptyCursorCurrent $ ns ^. navimStatePaths of
        File      _  -> continue ns
        Directory fp -> do
            liftIO $ setCurrentDirectory fp
            let (newHistory,
                 nextFocus) = case (ns ^. navimHistory, fp) of
                                  (ps, ".")    -> (ps, ".")
                                  ([], "..")   -> ([], ".")
                                  (p:ps, "..") -> (ps, p)
                                  (ps, next)   -> (next:ps, ".")
            ns' <- liftIO . buildState $
                       Just $ ns & navimStatePaths
                                 %~ nonEmptyCursorReset
            continue $
                ns' & navimHistory .~ newHistory
                    & navimStatePaths %~ \newPaths ->
                          fromMaybe (nonEmptyCursorReset newPaths) $
                              nonEmptyCursorSearch
                                  ((== nextFocus) . getPath)
                                  newPaths

colonCommand :: NavimState -> String -> EventM n (Next NavimState)
colonCommand s input =
    case input of
        ":q" -> halt s
        -- TODO: other meta commands
        _ -> continue $
                 s & navimMode .~ Navigation

inputCommand :: NavimState -> Prompt -> String -> EventM n (Next NavimState)
inputCommand ns CreateFile name = do
    success <- liftIO . createDirContentSafe $ File name
    ns'     <- liftIO . buildState $ Just ns
    continue $ ns' & navimMode .~ Navigation -- TODO: Navigation mode includes a Maybe Error field
inputCommand ns CreateDirectory name = do
    success <- liftIO . createDirContentSafe $ Directory name
    ns'     <- liftIO . buildState $ Just ns
    continue $ ns' & navimMode .~ Navigation -- TODO: Navigation mode includes a Maybe Error field
inputCommand ns Remove "y" = do
    liftIO . removeDirContent . nonEmptyCursorCurrent $ ns ^. navimStatePaths
    ns' <- liftIO . buildState $ Just ns
    continue $ ns' & navimMode .~ Navigation
inputCommand ns Remove _ =
    continue $ ns & navimMode .~ Navigation
inputCommand ns Rename newPath = do
    success <- liftIO $ renameDirContentSafe
                   (nonEmptyCursorCurrent $ ns ^. navimStatePaths)
                   newPath
    ns'     <- liftIO . buildState $ Just ns
    continue $ ns' & navimMode .~ Navigation

{- END Event Handler Helpers -}
