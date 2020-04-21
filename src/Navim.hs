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

import Text.Wrap

import Navim.DirContent
import Navim.NavimState

-- Main
navim :: IO ()
navim = do
    initState <- buildState Nothing
    endState  <- defaultMain navimApp initState
    let navPath = endState ^. navimHistory
                            . to (mconcat . intersperse "/" . reverse)
    putStrLn navPath

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
              , ("error", bg red)
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
                        , _navimMode = NavigationMode $ Navigation Nothing
                        }
                Just ps -> return $
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
      header
      <=>
      padBottom Max pathsWidget
      <=>
      statusBar
      <=>
      inputBar
    ]
  where
    header =
        border
        . padRight Max
        . withAttr "header"
        . strWrapDefault
        $ "navim - a (WIP) file manager written in Haskell by Gary Feng"

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

    statusBar = str $
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
                    Rename ->
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
            _ -> ns ^. navimHistory
                     . to (('/':) . mconcat . intersperse "/" . reverse)

    inputBar =
        padRight Max $
            case ns ^. navimMode of
                -- TODO: if NavigationMode Navigation with an error, display error
                NavigationMode navigation ->
                    case navigation ^. errored of
                            Nothing  -> str "-- NAVIGATION --"
                            Just err -> withAttr "error" . str . errorMessage $ err
                ColonMode colon ->
                    colon ^. colonInput
                           . to withBottomCursor
                InputMode input ->
                        input ^. inputResponse
                               . to (inputCommandText (input ^. command) ++)
                               . to withBottomCursor

    inputCommandText CreateFile      = "File name: "
    inputCommandText CreateDirectory = "Directory name: "
    inputCommandText Remove          = "Confirm (y/n): "
    inputCommandText Rename          = "New name: "

    withBottomCursor input =
        showCursor
            InputBar
            (Location (textWidth input, 0)) -- TODO: save screen width in NavimState and mod this by it
            (strWrapDefault input)

drawDirContent :: Bool -> DirContent -> Widget n
drawDirContent selected dc =
    decorate
    . strWrapDefault
    . getPath $ dc
  where
    decorate =
        withAttr
        . bool id (<> "selected") selected
        $ case dc of
              File      _ -> "file"
              Directory _ -> "dir"

-- Event Handler
handleEvent :: NavimState -> BrickEvent n e -> EventM n (Next NavimState)
handleEvent ns e =
    case e of
        VtyEvent vtye ->
            case vtye of
                EvKey key@(KChar ':') [] ->
                    bottomInputOr
                        (continue $ ns & navimMode .~ ColonMode (Colon ":"))
                        key
                EvKey key@(KChar 'j') [] ->
                    bottomInputOr
                        (moveCursorWith nonEmptyCursorSelectNext ns)
                        key
                EvKey key@(KChar 'k') [] ->
                    bottomInputOr
                        (moveCursorWith nonEmptyCursorSelectPrev ns)
                        key
                EvKey key@(KChar 'g') [] ->
                    bottomInputOr
                        (moveCursorWith (Just . nonEmptyCursorSelectFirst) ns)
                        key
                EvKey key@(KChar 'G') [] ->
                    bottomInputOr
                        (moveCursorWith (Just . nonEmptyCursorSelectLast) ns)
                        key
                EvKey key@(KChar 'n') [] ->
                    bottomInputOr
                        (continue $ toInputMode CreateFile ns)
                        key
                EvKey key@(KChar 'n') [MMeta] ->
                    bottomInputOr
                        (continue $ toInputMode CreateDirectory ns)
                        key
                EvKey key@(KChar 'd') [] ->
                    bottomInputOr
                        (continue $ toInputMode Remove ns)
                        key
                EvKey key@(KChar 'r') [] ->
                    bottomInputOr
                        (continue $ toInputMode Rename ns)
                        key
                EvKey key@(KChar _)   [] ->
                    bottomInputOr (continue ns) key

                EvKey KDown [] -> moveCursorWith nonEmptyCursorSelectNext ns
                EvKey KUp   [] -> moveCursorWith nonEmptyCursorSelectPrev ns

                EvKey KBS    [] -> bottomInputOr (continue ns) KBS
                EvKey KEnter [] -> bottomInputOr (performNavigate ns) KEnter
                EvKey KEsc   [] -> continue $ ns & navimMode .~ NavigationMode (Navigation Nothing)
                _ -> continue ns
        _ -> continue ns
  where
    safeInit [] = []
    safeInit xs = init xs

    toInputMode cmd = navimMode .~ InputMode (Input cmd "")

    bottomInputOr :: EventM n (Next NavimState) -> Key -> EventM n (Next NavimState)
    bottomInputOr navAction key =
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
                               .~ NavigationMode (Navigation Nothing)
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
                case input ^. command of
                    Remove ->
                        case ns ^. navimStatePaths
                                 . to nonEmptyCursorCurrent of
                            -- TODO: better error message pls
                            Directory "." ->
                                continue $
                                    ns & navimMode
                                       .~ NavigationMode
                                              (Navigation $ Just Remove)
                            Directory ".." ->
                                continue $
                                    ns & navimMode
                                       .~ NavigationMode
                                              (Navigation $ Just Remove)
                            _ ->
                                performInputCommand Remove
                    Rename ->
                        case ns ^. navimStatePaths
                                 . to nonEmptyCursorCurrent of
                            -- TODO: better error message pls
                            Directory "." ->
                                continue $
                                    ns & navimMode
                                       .~ NavigationMode
                                              (Navigation $ Just Rename)
                            Directory ".." ->
                                continue $
                                    ns & navimMode
                                       .~ NavigationMode
                                              (Navigation $ Just Rename)
                            _ ->
                                performInputCommand Rename
                    otherCommand ->
                        performInputCommand otherCommand
            (InputMode _, _) ->
                continue ns -- TODO!!!!!!!!

            (_, _) ->
                continue ns

    performInputCommand cmd = do
        success <- liftIO . inputCommand $ ns
        ns'     <- liftIO . buildState $ Just ns
        continue $
            ns' & navimMode
                .~ NavigationMode
                       (Navigation $
                           if success
                              then Nothing
                              else Just cmd)


{- BEGIN Event Handler Helpers -}

-- Note: clears the error message too
moveCursorWith :: (NonEmptyCursor DirContent -> Maybe (NonEmptyCursor DirContent))
               -> NavimState
               -> EventM n (Next NavimState)
moveCursorWith move ns =
    continue $
        case move $ ns ^. navimStatePaths of
            Nothing     -> ns & navimMode . _NavigationMode . errored
                              .~ Nothing
            Just newNec -> ns & navimStatePaths
                              .~ newNec
                              & navimMode . _NavigationMode . errored
                              .~ Nothing

performNavigate :: NavimState -> EventM n (Next NavimState)
performNavigate ns =
    case ns ^. navimStatePaths
             . to nonEmptyCursorCurrent of
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
colonCommand ns input =
    case input of
        ":q" -> halt ns
        -- TODO: other meta commands
        _ -> continue $
                 ns & navimMode
                    .~ NavigationMode (Navigation Nothing)

-- TODO: maybe having Command as a param is a better idea
inputCommand :: NavimState -> IO Bool
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
                        "y" -> (True <$)                 -- TODO: might error based on permissions
                               $ onSelected removeDirContent
                        _   -> return True
                Rename ->
                    onSelected $ renameDirContentSafe entered
        _ -> return False -- TODO: kind of a silent error
 where
    onSelected = ($ nonEmptyCursorCurrent (ns ^. navimStatePaths))

{- END Event Handler Helpers -}
