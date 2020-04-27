{-# LANGUAGE OverloadedStrings #-}

module Navim where

import System.Directory
import System.Exit
import System.Process

import Data.Bool
import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Traversable
import Data.Tuple

import Control.Lens
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

import Text.Wrap

import Navim.DirContent
import Navim.NavimState

-- Main
navim :: IO ()
navim = do
    initState <- buildState Nothing
    endState  <- defaultMain navimApp initState
    let DirHistory _ cur _ = endState ^. navimHistory
    putStrLn cur

data ResourceName
    = PathsWidget
    | InputBar
    deriving (Show, Eq, Ord)

nonEmptyCursorReset :: NonEmptyCursor a -> NonEmptyCursor a
nonEmptyCursorReset = makeNonEmptyCursor . rebuildNonEmptyCursor

strWrapDefault :: String -> Widget n
strWrapDefault = strWrapWith (WrapSettings False True)

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
              , ("success", bg green)
              , ("error", bg red)
              ]
    }

-- State Transformer (and I don't mean the monad ;))
buildState :: Maybe NavimState -> IO NavimState
buildState prevState = do
    curDir   <- getCurrentDirectory
    contents <- getDirContents curDir
    case NE.nonEmpty contents of
        Nothing -> die "Should never happen (current directory \".\" always here)"
        Just ne ->
            case prevState of
                Nothing ->
                    pure NavimState
                        { _navimStatePaths = makeNonEmptyCursor ne
                        , _navimHistory = DirHistory [] curDir []
                        , _navimMode = NavigationMode $ Navigation Indicate
                        , _navimClipboard = Nothing
                        , _navimWidth = 1
                        }
                Just ps ->
                    pure $
                        ps & navimStatePaths
                           %~ adjustCursor (makeNonEmptyCursor ne)
  where
    adjustCursor newNec oldNec =
        moveNextBy (length $ nonEmptyCursorPrev oldNec) newNec

    moveNextBy 0 newNec = newNec
    moveNextBy n newNec =
        case nonEmptyCursorSelectNext newNec of
            Nothing   -> newNec
            Just nec' -> moveNextBy (n - 1) nec'

-- UI Drawer
drawNavim :: NavimState -> [Widget ResourceName]
drawNavim ns =
    [
      hBorder
      <=>
      header
      <=>
      hBorder
      <=>
      padBottom Max pathsWidget
      <=>
      hBorder
      <=>
      statusBar
      <=>
      reportExtent InputBar inputBar
    ]
  where
    header =
        padRight Max
        . withAttr "header"
        . strWrapDefault
        $ unlines
            [ " ____ ____ ____ ____ ____ "
            , "||n |||a |||v |||i |||m ||"
            , "||__|||__|||__|||__|||__||"
            , "|/__\\|/__\\|/__\\|/__\\|/__\\|"
            , "A (WIP) file manager written in Haskell by Gary Feng"
            ]

    pathsWidget =
        padRight Max
        . viewport PathsWidget Vertical
        . vBox
        . mconcat
        $ [ drawDirContent False <$> reverse (nonEmptyCursorPrev pathsCursor)
          , [ visible
              . drawDirContent True
              $ nonEmptyCursorCurrent pathsCursor
            ]
          , drawDirContent False <$> nonEmptyCursorNext pathsCursor
          ]

    pathsCursor = ns ^. navimStatePaths

    statusBar = strWrapDefault $
        case ns ^. navimMode of
            InputMode input ->
                case input ^. command of
                    CreateFile ->
                        "Enter the name of the file to be created"
                    CreateDirectory ->
                        "Enter the name of the directory to be created"
                    Remove ->
                        case nonEmptyCursorCurrent pathsCursor of
                            File name ->
                                mconcat
                                    [ "Are you sure you want to remove the file "
                                    , name
                                    , "?"
                                    ]
                            Directory name ->
                                mconcat
                                    [ "Are you sure you want to remove the directory "
                                    , name
                                    , "?"
                                    ]
                    Rename ->
                        case nonEmptyCursorCurrent pathsCursor of
                            File name ->
                                mconcat
                                    [ "Enter the new name for the file "
                                    , name
                                    , "."
                                    ]
                            Directory name ->
                                mconcat
                                    [ "Enter the new name for the directory "
                                    , name
                                    , "."
                                    ]
                    Paste ->
                        case ns ^. navimClipboard of
                            Nothing ->
                                "Clipboard is empty"
                            Just (Directory _) ->
                                "Directory copy/paste is currently unsupported"
                            Just (File name) ->
                                mconcat
                                    [ "Are you sure you want to copy "
                                    , name
                                    , " into the current directory?"
                                    ]

            _ -> ns ^. navimHistory
                     . currentDirectory

    inputBar =
        padRight Max $
            case ns ^. navimMode of
                NavigationMode navigation ->
                    navigation ^. displayMessage
                                . to (str . messageString)
                                . to (case navigation ^. displayMessage of
                                         Indicate    -> id
                                         Success _   -> withAttr "success"
                                         Error _ _   -> withAttr "error")
                ColonMode colon ->
                    colon ^. colonInput
                           . to (++ ['_'])
                           . to withBottomCursor
                InputMode input ->
                        input ^. inputResponse
                               . to (++ ['_'])
                               . to (inputCommandText (input ^. command) ++)
                               . to withBottomCursor

    inputCommandText CreateFile      = "File name: "
    inputCommandText CreateDirectory = "Directory name: "
    inputCommandText Remove          = "Confirm (y/n): "
    inputCommandText Rename          = "New name: "
    inputCommandText Copy            = "Should never use this... TODO"
    inputCommandText Paste           = "Confirm (y/n): "

    -- input has '_' appended to the end...
    -- this is a hack
    -- TODO: figure out how to get cursor to take up the next line
    --       when input is at the end of a line
    withBottomCursor input =
        showCursor
            InputBar
            (Location . swap $ (textWidth input - 1) `divMod` (ns ^. navimWidth))
            (strWrapDefault input)

drawDirContent :: Bool -> DirContent -> Widget n
drawDirContent selected dc =
    decorate . strWrapDefault . getPath $ dc
  where
    decorate =
        withAttr . bool id (<> "selected") selected $
            case dc of
                File      _ -> "file"
                Directory _ -> "dir"

-- Event Handler
handleEvent :: NavimState
            -> BrickEvent ResourceName e
            -> EventM ResourceName (Next NavimState)
handleEvent s e = do
    mExtent <- lookupExtent InputBar
    let ns = case mExtent of
                 Nothing -> s
                 Just (Extent _ _ (width, _) _) ->
                     s & navimWidth
                       .~ width
    case e of
        VtyEvent vtye ->
            case vtye of
                EvKey key@(KChar ':') [] ->
                    givenCommandOrInput ns key $
                        continue $ ns & navimMode
                                      .~ ColonMode (Colon ":")
                EvKey key@(KChar 'j') [] ->
                    givenCommandOrInput ns key $
                        moveCursorWith nonEmptyCursorSelectNext ns
                EvKey key@(KChar 'k') [] ->
                    givenCommandOrInput ns key $
                        moveCursorWith nonEmptyCursorSelectPrev ns
                EvKey key@(KChar 'g') [] ->
                    givenCommandOrInput ns key $
                        moveCursorWith (Just . nonEmptyCursorSelectFirst) ns
                EvKey key@(KChar 'G') [] ->
                    givenCommandOrInput ns key $
                        moveCursorWith (Just . nonEmptyCursorSelectLast) ns
                EvKey key@(KChar 'n') [] ->
                    givenCommandOrInput ns key $
                        continue $ toInputMode CreateFile ns
                EvKey key@(KChar 'N') [] ->
                    givenCommandOrInput ns key $
                        continue $ toInputMode CreateDirectory ns
                EvKey key@(KChar 'd') [] ->
                    givenCommandOrInput ns key $
                        continue $
                            case ns ^. navimStatePaths
                                     . to nonEmptyCursorCurrent of
                                Directory "." ->
                                    ns & navimMode
                                       . _NavigationMode . displayMessage
                                       .~ Error Remove (InvalidName ".")
                                Directory ".." ->
                                    ns & navimMode
                                       . _NavigationMode . displayMessage
                                       .~ Error Remove (InvalidName "..")
                                _ -> toInputMode Remove ns
                EvKey key@(KChar 'r') [] ->
                    givenCommandOrInput ns key $
                        continue $
                            case ns ^. navimStatePaths
                                     . to nonEmptyCursorCurrent of
                                Directory "." ->
                                    ns & navimMode
                                       . _NavigationMode . displayMessage
                                       .~ Error Rename (InvalidName ".")
                                Directory ".." ->
                                    ns & navimMode
                                       . _NavigationMode . displayMessage
                                       .~ Error Rename (InvalidName ".")
                                _ -> toInputMode Rename ns
                EvKey key@(KChar 'y') [] -> -- TODO: do nothing on Directory for now
                    givenCommandOrInput ns key $
                        continue $
                            case ns ^. navimStatePaths
                                     . to nonEmptyCursorCurrent of
                                Directory name ->
                                    ns & navimMode
                                       . _NavigationMode . displayMessage
                                       .~ Error Copy (InvalidName name)
                                File name ->
                                    ns & navimMode
                                       . _NavigationMode . displayMessage
                                       .~ Success Copy
                                       & navimClipboard
                                       .~ (ns ^. navimHistory
                                               . currentDirectory
                                               . to (Just
                                                    . File
                                                    . (++ '/':name)))
                EvKey key@(KChar 'p') [] ->
                    givenCommandOrInput ns key $
                        continue $
                            case ns ^. navimClipboard of
                                Nothing -> ns
                                _       -> toInputMode Paste ns
                EvKey key@(KChar 'v') [] ->
                    givenCommandOrInput ns key $
                        suspendAndResume $
                            ns <$
                            callProcess
                                "vim"
                                 [ns ^. navimStatePaths
                                      . to (getPath . nonEmptyCursorCurrent)]
                EvKey key@(KChar 'u') [] ->
                    givenCommandOrInput ns key $
                        liftIO (changeDirHistoryWith undoDirHistory ns)
                            >>= continue
                EvKey key@(KChar 'r') [MCtrl] ->
                    givenCommandOrInput ns key $
                        liftIO (changeDirHistoryWith redoDirHistory ns)
                            >>= continue
                EvKey key@(KChar _) [] ->
                    givenCommandOrInput ns key $ continue ns

                EvKey (KChar 'd') [MCtrl] ->
                    halt ns

                EvKey KDown [] -> moveCursorWith nonEmptyCursorSelectNext ns
                EvKey KUp   [] -> moveCursorWith nonEmptyCursorSelectPrev ns

                EvKey KBS    [] -> givenCommandOrInput ns KBS (continue ns)
                EvKey KEnter [] -> givenCommandOrInput ns KEnter (previewOrNavigate ns)
                EvKey KEsc   [] -> continue $ ns & navimMode .~ NavigationMode (Navigation Indicate)
                _ -> continue ns
        _ -> continue ns
  where
    safeInit [] = []
    safeInit xs = init xs

    toInputMode cmd = navimMode .~ InputMode (Input cmd "")

    givenCommandOrInput :: NavimState -> Key -> EventM n (Next NavimState) -> EventM n (Next NavimState)
    givenCommandOrInput ns key navAction =
        case (ns ^. navimMode, key) of
            (NavigationMode _, _) -> navAction

            (ColonMode _, KChar c) ->
                continue $
                    ns & navimMode . _ColonMode . colonInput
                       %~ (++ [c])
            (ColonMode colon, KBS) ->
                case colon ^. colonInput of
                    [] ->
                        error "Programmer error: colon input should never be empty"
                    [_] ->
                        continue $
                            ns & navimMode
                               .~ NavigationMode (Navigation Indicate)
                    cs ->
                        continue $
                            ns & navimMode . _ColonMode . colonInput
                               %~ safeInit
            (ColonMode colon, KEnter) ->
                colon ^. colonInput .to (colonCommand ns)

            (InputMode _, KChar c) ->
                continue $
                    ns & navimMode . _InputMode . inputResponse
                       %~ (++ [c])
            (InputMode _, KBS) ->
                continue $
                    ns & navimMode . _InputMode . inputResponse
                       %~ safeInit
            (InputMode input, KEnter) ->
                input ^. command
                       . to (performInputCommand ns)
            (InputMode _, _) ->
                continue ns -- TODO!!!!!!!!

            (_, _) ->
                continue ns

    performInputCommand ns cmd = do
        dcResult <- liftIO . inputCommand $ ns
        ns'      <- liftIO . buildState $ Just ns
        continue $
            ns' & navimMode
                .~ NavigationMode
                       (Navigation $ case dcResult of
                            DCSuccess -> Success cmd
                            DCError e -> Error cmd e)

{- BEGIN Event Handler Helpers -}

-- Note: clears the error message too
moveCursorWith :: (NonEmptyCursor DirContent -> Maybe (NonEmptyCursor DirContent))
               -> NavimState
               -> EventM n (Next NavimState)
moveCursorWith move ns =
    continue $
        case move $ ns ^. navimStatePaths of
            Nothing     -> ns & navimMode . _NavigationMode . displayMessage
                              .~ Indicate
            Just newNec -> ns & navimStatePaths
                              .~ newNec
                              & navimMode . _NavigationMode . displayMessage
                              .~ Indicate

previewOrNavigate :: NavimState -> EventM n (Next NavimState)
previewOrNavigate ns =
    case ns ^. navimStatePaths
             . to nonEmptyCursorCurrent of
        File      fp ->
            suspendAndResume $
                ns <$
                callProcess
                    "less"
                    [ns ^. navimStatePaths
                         . to (getPath . nonEmptyCursorCurrent)]
        Directory fp -> do
            let (curDir, _) = ns ^. navimHistory
                                  . currentDirectory
                                  . to nameAndDirectory
            let nextFocus = if fp == ".." then curDir else "."
            liftIO $ setCurrentDirectory fp
            newCurDir <- liftIO getCurrentDirectory
            ns'       <- liftIO . buildState . Just $
                             ns & navimStatePaths
                                %~ nonEmptyCursorReset
            continue $
                ns' & navimMode . _NavigationMode . displayMessage
                    .~ Indicate
                    & navimHistory
                    %~ withNewCurrentDir newCurDir
                    & navimStatePaths
                    %~ \newPaths ->
                          fromMaybe (nonEmptyCursorReset newPaths) $
                              nonEmptyCursorSearch
                                  ((== nextFocus) . getPath)
                                  newPaths

colonCommand :: NavimState -> String -> EventM n (Next NavimState)
colonCommand ns input =
    case input of
        ":q" -> halt ns
        -- TODO: other meta commands
        ':':'r':'u':'n':' ':cmd ->
            suspendAndResume $
                callCommand cmd
                >> (buildState . Just $
                       ns & navimMode
                          .~ NavigationMode (Navigation Indicate))

        _ -> continue $
                 ns & navimMode
                    .~ NavigationMode (Navigation Indicate)

-- TODO: maybe having Command as a param is a better idea
inputCommand :: NavimState -> IO DirContentActionResult
inputCommand ns =
    case ns ^. navimMode of
        InputMode input ->
            let entered = input ^. inputResponse in
            case input ^. command of
                CreateFile ->
                    createDirContentSafe $ File entered
                CreateDirectory ->
                    createDirContentSafe $ Directory entered
                Remove ->
                    case entered of
                        "y" -> onSelected removeDirContentSafe
                        _   -> pure $ DCError Cancelled
                Rename ->
                    onSelected $ renameDirContentSafe entered
                Paste ->
                    case (entered, ns ^. navimClipboard) of
                        ("y", Just clip) ->
                            let (clipName, _) = nameAndDirectory . getPath $ clip in
                            copyDirContentSafe
                                (ns ^. navimHistory
                                     . currentDirectory
                                     . to (++ '/':clipName))
                                clip
                        _ ->
                            pure $ DCError Cancelled
        _ -> pure $ DCError Cancelled -- TODO: kind of a silent error
 where
    onSelected f = ns ^. navimStatePaths
                       . to (f . nonEmptyCursorCurrent)

changeDirHistoryWith :: (DirHistory -> DirHistory) -> NavimState -> IO NavimState
changeDirHistoryWith changeFn ns = do
    let newHistory = ns ^. navimHistory
                         . to changeFn
    let newCurDir = newHistory ^. currentDirectory
    validDir <- doesDirectoryExist newCurDir
    if validDir
        then
            setCurrentDirectory newCurDir *>
            ((navimHistory .~ newHistory) <$> buildState (Just ns))
        else
            pure $       -- todo: handle differently
                ns & navimHistory
                   . undoDirectories
                   .~ []
                   & navimHistory
                   . redoDirectories
                   .~ []

{- END Event Handler Helpers -}
