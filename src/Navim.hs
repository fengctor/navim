{-# LANGUAGE OverloadedStrings #-}

module Navim where

import System.Directory
import System.Exit
import System.Process

import Data.Bool
import Data.Char
import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.HashMap (Map)
import qualified Data.HashMap as Map
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

import qualified Cursor.List.NonEmpty as NEC (NonEmptyCursor(..))
import Cursor.Simple.List.NonEmpty

import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events

import Text.Wrap

import Navim.DirContent
import Navim.Instances.Hashable
import Navim.NavimState

type NavimStateRN = NavimState ResourceName

defaultCommandMap :: Map
    (Key, [Modifier])
    (NavimStateRN -> EventM ResourceName (Next NavimStateRN))
defaultCommandMap = Map.fromList
    [ ( (KChar 'j', [])
      , moveCursorWith nonEmptyCursorSelectNext
      )
    , ( (KChar 'k', [])
      , moveCursorWith nonEmptyCursorSelectPrev
      )
    , ( (KChar 'g', [])
      , moveCursorWith (Just . nonEmptyCursorSelectFirst)
      )
    , ( (KChar 'G', [])
      , moveCursorWith (Just . nonEmptyCursorSelectLast)
      )
    , ( (KChar 'n', [])
      , continue . (toInputMode CreateFile)
      )
    , ( (KChar 'N', [])
      , continue . (toInputMode CreateDirectory)
      )
    , ( (KChar 'd', [])
      , attemptModifySelectedWith Remove
      )
    , ( (KChar 'r', [])
      , attemptModifySelectedWith Rename
      )
    , ( (KChar 'y', [])
      , copySelected
      )
    , ( (KChar 'p', [])
      , pasteClipboard
      )
    , ( (KChar 'v', [])
      , vimOnSelected
      )
    , ( (KChar 'u', [])
      , (>>= continue) . liftIO . (changeDirHistoryWith undoDirHistory)
      )
    , ( (KChar 'r', [MCtrl])
      , (>>= continue) . liftIO . (changeDirHistoryWith redoDirHistory)
      )
    , ( (KChar 'd', [MCtrl])
      , halt
      )
    , ( (KChar 'f', [])
      , performSearch
      )
    ]
  where
    toInputMode cmd = navimMode .~ InputMode (Input cmd "")

    attemptModifySelectedWith cmd ns =
        continue $
            case ns ^. navimStatePaths
                     . to nonEmptyCursorCurrent of
                Directory "." ->
                    ns & navimMode
                       . _NavigationMode . displayMessage
                       .~ Error cmd (InvalidName ".")
                Directory ".." ->
                    ns & navimMode
                       . _NavigationMode . displayMessage
                       .~ Error cmd (InvalidName "..")
                _ -> toInputMode cmd ns

    copySelected ns =
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

    pasteClipboard ns =
        continue $
            case ns ^. navimClipboard of
                Nothing -> ns
                _       -> toInputMode Paste ns

    vimOnSelected ns =
        suspendAndResume $
            ns <$
            callProcess
                "vim"
                 [ns ^. navimStatePaths
                      . to (getPath . nonEmptyCursorCurrent)]

    performSearch ns =
        let nsSearch = ns ^. navimSearch
                           . to (toLower <$>) in
        continue $
            ns & navimMode
               . _NavigationMode . displayMessage
               .~ case nsSearch of
                      "" -> Indicate
                      _  -> Neutral ('/' : nsSearch)
               & navimStatePaths
               %~ \paths ->
                   case nsSearch of
                       "" -> paths
                       savedQuery ->
                           fromMaybe paths $
                               nonEmptyCursorCircularSearch
                                   ((savedQuery `isPrefixOf`)
                                    . (toLower <$>)
                                    . getPath)
                                   paths


-- Main
navim :: NavimConfig ResourceName -> IO ()
navim cfg = do
    initState <- buildState Nothing
    endState  <- defaultMain navimApp $
                     initState & navimConfig
                               .~ cfg
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
navimApp :: App NavimStateRN e ResourceName
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
buildState :: Maybe NavimStateRN -> IO NavimStateRN
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
                        , _navimSearch = ""
                        , _navimWidth = 1
                        , _navimConfig = NavimConfig $ Map.empty
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
drawNavim :: NavimStateRN -> [Widget ResourceName]
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
                                         Neutral _   -> id
                                         Success _   -> withAttr "success"
                                         Error _ _   -> withAttr "error")
                MetaMode meta ->
                    meta ^. metaInput
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
handleEvent :: NavimStateRN
            -> BrickEvent ResourceName e
            -> EventM ResourceName (Next NavimStateRN)
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
                EvKey KEnter [] -> dispatchKey ns KEnter $
                                       previewOrNavigate ns
                EvKey KUp    [] -> dispatchKey ns KEnter $
                                       moveCursorWith nonEmptyCursorSelectPrev ns
                EvKey KDown  [] -> dispatchKey ns KEnter $
                                       moveCursorWith nonEmptyCursorSelectNext ns

                EvKey key@(KChar ':') [] ->
                    dispatchKey ns key $
                        continue $
                            ns & navimMode
                               .~ MetaMode (Meta ":")

                EvKey key@(KChar '/') [] ->
                    dispatchKey ns key $
                        continue $
                            ns & navimMode
                               .~ MetaMode (Meta "/")

                EvKey KEsc [] ->
                    continue $
                        ns & navimMode
                           .~ NavigationMode (Navigation Indicate)

                EvKey key modifier ->
                    dispatchKey ns key $
                        fromMaybe
                            continue
                            (ns ^. navimConfig . commandMap
                                 . to (Map.lookup (key, modifier)))
                        ns

                _ -> continue ns
        _ -> continue ns
  where
    safeInit [] = []
    safeInit xs = init xs

    toInputMode cmd = navimMode .~ InputMode (Input cmd "")

    dispatchKey :: NavimStateRN
                -> Key
                -> EventM n (Next NavimStateRN)
                -> EventM n (Next NavimStateRN)
    dispatchKey ns key navAction =
        case (ns ^. navimMode, key) of
            (NavigationMode _, _) -> navAction

            (MetaMode _, KChar c) ->
                continue $
                    ns & navimMode . _MetaMode . metaInput
                       %~ (++ [c])
            (MetaMode meta, KBS) ->
                case meta ^. metaInput of
                    [] ->
                        error "Programmer error: meta input should never be empty"
                    [_] ->
                        continue $
                            ns & navimMode
                               .~ NavigationMode (Navigation Indicate)
                    cs ->
                        continue $
                            ns & navimMode . _MetaMode . metaInput
                               %~ safeInit
            (MetaMode meta, KEnter) ->
                meta ^. metaInput .to (metaCommand ns)

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
               -> NavimStateRN
               -> EventM n (Next NavimStateRN)
moveCursorWith move ns =
    continue $
        case move $ ns ^. navimStatePaths of
            Nothing     -> ns & navimMode . _NavigationMode . displayMessage
                              .~ Indicate
            Just newNec -> ns & navimStatePaths
                              .~ newNec
                              & navimMode . _NavigationMode . displayMessage
                              .~ Indicate

previewOrNavigate :: NavimStateRN -> EventM n (Next NavimStateRN)
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
                    %~ \paths ->
                        fromMaybe (nonEmptyCursorReset paths) $
                            nonEmptyCursorSearch
                                ((== nextFocus) . getPath)
                                paths

metaCommand :: NavimStateRN -> String -> EventM n (Next NavimStateRN)
metaCommand ns input =
    case input of
        ":q" -> halt ns
        -- TODO: other meta commands
        ':':'r':'u':'n':' ':cmd ->
            suspendAndResume $
                callCommand cmd
                >> (buildState . Just $
                       ns & navimMode
                          .~ NavigationMode (Navigation Indicate))
        ['/'] ->
            continue $
                ns & navimMode
                   .~ NavigationMode (Navigation Indicate)
                   & navimSearch
                   .~ ""
        '/':cs ->
            let searchQuery = toLower <$> cs in
            continue $
                ns & navimMode
                   .~ NavigationMode (Navigation Indicate)
                   & navimSearch
                   .~ searchQuery
                   & navimStatePaths
                   %~ \paths ->
                       fromMaybe paths $
                           nonEmptyCursorCircularSearch
                               ((searchQuery `isPrefixOf`)
                                . (toLower <$>)
                                . getPath)
                               paths
        _ -> continue $
                 ns & navimMode
                    .~ NavigationMode (Navigation Indicate)

-- TODO: maybe having Command as a param is a better idea
inputCommand :: NavimStateRN -> IO DirContentActionResult
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

changeDirHistoryWith :: (DirHistory -> DirHistory) -> NavimStateRN -> IO NavimStateRN
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

nonEmptyCursorCircularSearch :: (a -> Bool)
                             -> NonEmptyCursor a
                             -> Maybe (NonEmptyCursor a)
nonEmptyCursorCircularSearch p nec@(NEC.NonEmptyCursor prev cur next) =
    searchWithGas
        (length prev + length next)
        p
        (nonEmptyCursorSelectNextOrCycle nec)
  where
    nonEmptyCursorSelectNextOrCycle nec =
        fromMaybe (nonEmptyCursorSelectFirst nec) (nonEmptyCursorSelectNext nec)

    searchWithGas 0 _ nec = Nothing
    searchWithGas n p nec
        | p (nonEmptyCursorCurrent nec)
            = Just nec
        | otherwise
            = searchWithGas
                  (n - 1)
                  p
                  (nonEmptyCursorSelectNextOrCycle nec)

{- END Event Handler Helpers -}
