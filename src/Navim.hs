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

import Data.NavimState
import Navim.DirContent

-- Main
navim :: IO ()
navim = do
    initState <- buildState Nothing
    endState  <- defaultMain navimApp initState
    let navPath = mconcat . intersperse "/" . reverse $ endState ^. navimHistory
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
                        , _navimMode = NavigationMode Navigation
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
                case input ^. prompt of
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
            _ -> ('/':) . mconcat . intersperse "/" . reverse $ ns ^. navimHistory

    inputBar =
        padRight Max $
            case ns ^. navimMode  of
                NavigationMode Navigation ->
                    str "-- NAVIGATION --"
                ColonMode colon ->
                    withBottomCursor $ colon ^. colonInput
                InputMode input ->
                    case input ^. prompt of
                        CreateFile ->
                            withBottomCursor $
                                "File name: " ++ input ^. inputResponse
                        CreateDirectory ->
                            withBottomCursor $
                                "Directory name: " ++ input ^. inputResponse
                        Remove ->
                            withBottomCursor $
                                "Confirm (y/n): " ++ input ^. inputResponse
                        Rename ->
                            withBottomCursor $
                                "New name: " ++ input ^. inputResponse

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
handleEvent s e =
    case e of
        VtyEvent vtye ->
            case vtye of
                EvKey key@(KChar ':') [] ->
                    bottomInputOr
                        (continue $ s & navimMode .~ ColonMode (Colon ":"))
                        key
                EvKey key@(KChar 'j') [] ->
                    bottomInputOr
                        (moveCursorWith nonEmptyCursorSelectNext s)
                        key
                EvKey key@(KChar 'k') [] ->
                    bottomInputOr
                        (moveCursorWith nonEmptyCursorSelectPrev s)
                        key
                EvKey key@(KChar 'g') [] ->
                    bottomInputOr
                        (moveCursorWith (Just . nonEmptyCursorSelectFirst) s)
                        key
                EvKey key@(KChar 'G') [] ->
                    bottomInputOr
                        (moveCursorWith (Just . nonEmptyCursorSelectLast) s)
                        key
                EvKey key@(KChar 'n') [] ->
                    bottomInputOr
                        (continue $ s & navimMode .~ InputMode (Input CreateFile ""))
                        key
                EvKey key@(KChar 'n') [MMeta] ->
                    bottomInputOr
                        (continue $ s & navimMode .~ InputMode (Input CreateDirectory ""))
                        key
                EvKey key@(KChar 'd') [] ->
                    bottomInputOr
                        (continue $ s & navimMode .~ InputMode (Input Remove ""))
                        key
                EvKey key@(KChar 'r') [] ->
                    bottomInputOr
                        (continue $ s & navimMode .~ InputMode (Input Rename ""))
                        key
                EvKey key@(KChar _)   [] ->
                    bottomInputOr (continue s) key

                EvKey KDown [] -> moveCursorWith nonEmptyCursorSelectNext s
                EvKey KUp   [] -> moveCursorWith nonEmptyCursorSelectPrev s

                EvKey KBS    [] -> bottomInputOr (continue s) KBS
                EvKey KEnter [] -> bottomInputOr (performNavigate s) KEnter
                EvKey KEsc   [] -> continue $ s & navimMode .~ NavigationMode Navigation
                _ -> continue s
        _ -> continue s
  where
    safeInit [] = []
    safeInit xs = init xs

    bottomInputOr :: EventM n (Next NavimState) -> Key -> EventM n (Next NavimState)
    bottomInputOr navAction key =
        case (s ^. navimMode, key) of
            (NavigationMode _, _) -> navAction

            -- TODO: lens please...
            (ColonMode _, KChar c) ->
                continue $
                    s & navimMode . _ColonMode . colonInput
                      %~ (++ [c])
            (ColonMode colon, KBS) ->
                case colon ^. colonInput of
                    []   -> error "Programmer error: colon input should never be empty"
                    [_]  -> continue $
                                s & navimMode
                                  .~ NavigationMode Navigation
                    cs -> continue $
                              s & navimMode . _ColonMode . colonInput
                                %~ safeInit
            (ColonMode colon, KEnter) ->
                colonCommand s $ colon ^. colonInput

            (InputMode _, KChar c) ->
                continue $
                    s & navimMode . _InputMode . inputResponse
                      %~ (++ [c])
            (InputMode _, KBS) ->
                continue $
                    s & navimMode . _InputMode . inputResponse
                      %~ safeInit
            (InputMode input, KEnter) ->
                case input ^. prompt of
                    Remove ->
                        case nonEmptyCursorCurrent $ s ^. navimStatePaths of
                            -- TODO: no silent failure pls
                            Directory "."  -> continue $
                                                  s & navimMode
                                                    .~ NavigationMode Navigation
                            Directory ".." -> continue $
                                                  s & navimMode
                                                     .~ NavigationMode Navigation
                            _              -> inputCommand s Remove $ input ^. inputResponse
                    Rename ->
                        case nonEmptyCursorCurrent $ s ^. navimStatePaths of
                            -- TODO: no silent failure pls
                            Directory "."  -> continue $
                                                  s & navimMode
                                                    .~ NavigationMode Navigation
                            Directory ".." -> continue $
                                                  s & navimMode
                                                    .~ NavigationMode Navigation
                            _              -> inputCommand s Rename $ input ^. inputResponse
                    otherPrompt ->
                        inputCommand s otherPrompt $ input ^. inputResponse
            (InputMode _, _) -> continue s -- TODO!!!!!!!!

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
                 s & navimMode
                   .~ NavigationMode Navigation

-- TODO: get prompt from ns
inputCommand :: NavimState -> Prompt -> String -> EventM n (Next NavimState)
inputCommand ns CreateFile name = do
    success <- liftIO . createDirContentSafe $ File name
    ns'     <- liftIO . buildState $ Just ns
    continue $
        ns' & navimMode
            .~ NavigationMode Navigation -- TODO: Navigation mode includes a Maybe Error field
inputCommand ns CreateDirectory name = do
    success <- liftIO . createDirContentSafe $ Directory name
    ns'     <- liftIO . buildState $ Just ns
    continue $
        ns' & navimMode
            .~ NavigationMode Navigation -- TODO: Navigation mode includes a Maybe Error field
inputCommand ns Remove "y" = do
    liftIO . removeDirContent . nonEmptyCursorCurrent $ ns ^. navimStatePaths
    ns' <- liftIO . buildState $ Just ns
    continue $
        ns' & navimMode
            .~ NavigationMode Navigation
inputCommand ns Remove _ =
    continue $
        ns & navimMode
           .~ NavigationMode Navigation
inputCommand ns Rename newPath = do
    success <- liftIO $ renameDirContentSafe
                   (nonEmptyCursorCurrent $ ns ^. navimStatePaths)
                   newPath
    ns'     <- liftIO . buildState $ Just ns
    continue $
        ns' & navimMode
            .~ NavigationMode Navigation

{- END Event Handler Helpers -}
