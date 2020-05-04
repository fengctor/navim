{-# LANGUAGE OverloadedStrings #-}

module Navim where

import           Data.Bool
import           Data.HashMap                (Map)
import qualified Data.HashMap                as Map
import           Data.Maybe
import           Data.Tuple

import           Control.Lens
import           Control.Monad.IO.Class

import           Brick.AttrMap
import           Brick.Main
import           Brick.Types
import           Brick.Util
import           Brick.Widgets.Border
import           Brick.Widgets.Core

import           Cursor.Simple.List.NonEmpty

import           Graphics.Vty.Attributes
import           Graphics.Vty.Input.Events

import           Text.Wrap

import           Navim.DirContent
import           Navim.Instances.Hashable
import           Navim.NavimCommand
import           Navim.NavimState

defaultCommandMap :: Map
    (Key, [Modifier])
    NavimCommand
defaultCommandMap = Map.fromList
    [ ( (KChar 'j', [])
      , Internal . NoInput $ MoveCursor CursorDown
      )
    , ( (KChar 'k', [])
      , Internal . NoInput $ MoveCursor CursorUp
      )
    , ( (KChar 'J', [])
      , Internal $ Sequence (replicate 3 (MoveCursor CursorDown)) Nothing
      )
    , ( (KChar 'K', [])
      , Internal $ Sequence (replicate 3 (MoveCursor CursorUp)) Nothing
      )
    , ( (KChar 'g', [])
      , Internal . NoInput $ MoveCursor CursorTop
      )
    , ( (KChar 'G', [])
      , Internal . NoInput $ MoveCursor CursorBottom
      )
    , ( (KChar 'n', [])
      , Internal . WithInput $ CreateContent File
      )
    , ( (KChar 'N', [])
      , Internal . WithInput $ CreateContent Directory
      )
    , ( (KChar 'd', [])
      , Internal . WithInput $ ModifySelected Remove
      )
    , ( (KChar 'r', [])
      , Internal . WithInput $ ModifySelected Rename
      )
    , ( (KChar 'y', [])
      , Internal . NoInput $ SelectedToClipboard Replicate
      )
    , ( (KChar 'x', [])
      , Internal . NoInput $ SelectedToClipboard Move
      )
    , ( (KChar 'p', [])
      , Internal . WithInput $ PasteClipboard
      )
    , ( (KChar 'v', [])
      , External $ BashCommandOnSelected "vim"
      )
    , ( (KChar 'u', [])
      , Internal . NoInput $ ChangeDirHistory Undo
      )
    , ( (KChar 'r', [MCtrl])
      , Internal . NoInput $ ChangeDirHistory Redo
      )
    , ( (KChar 'f', [])
      , Internal . NoInput $ PerformSearch
      )
    ]

-- Main
navim :: NavimConfig NavimCommand -> IO ()
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


strWrapDefault :: String -> Widget n
strWrapDefault = strWrapWith (WrapSettings False True)

-- TUI App Components
navimApp :: App (NavimState NavimCommand) e ResourceName
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

-- UI Drawer
drawNavim :: NavimState NavimCommand -> [Widget ResourceName]
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
                            DirContent File name ->
                                mconcat
                                    [ "Are you sure you want to remove the file "
                                    , name
                                    , "?"
                                    ]
                            DirContent Directory name ->
                                mconcat
                                    [ "Are you sure you want to remove the directory "
                                    , name
                                    , "?"
                                    ]
                    Rename ->
                        case nonEmptyCursorCurrent pathsCursor of
                            DirContent File name ->
                                mconcat
                                    [ "Enter the new name for the file "
                                    , name
                                    , "."
                                    ]
                            DirContent Directory name ->
                                mconcat
                                    [ "Enter the new name for the directory "
                                    , name
                                    , "."
                                    ]
                    Paste ->
                        case ns ^. navimClipboard . clipboardContent of
                            Nothing ->
                                "Clipboard is empty"
                            Just (DirContent Directory _) ->
                                "Directory copy/paste is currently unsupported"
                            Just (DirContent File name) ->
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
                                         Indicate  -> id
                                         Neutral _ -> id
                                         Success _ -> withAttr "success"
                                         Error _ _ -> withAttr "error")
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
            case contentType dc of
                File      -> "file"
                Directory -> "dir"

-- Event Handler
handleEvent :: NavimState NavimCommand
            -> BrickEvent ResourceName e
            -> EventM ResourceName (Next (NavimState NavimCommand))
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
                                       continue $ moveCursorWith nonEmptyCursorSelectPrev ns
                EvKey KDown  [] -> dispatchKey ns KEnter $
                                       continue $ moveCursorWith nonEmptyCursorSelectNext ns

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

                EvKey key@(KChar 'd') [MCtrl] ->
                    halt ns

                EvKey key modifier ->
                    dispatchKey ns key $
                        maybe
                            keyNotBound
                            commandFunction
                            (ns ^. navimConfig . commandMap
                                 . to (Map.lookup (key, modifier))
                            )
                        ns

                _ -> continue ns
        _ -> continue ns
  where
    toInputMode cmd = navimMode .~ InputMode (Input cmd "")

    keyNotBound = continue
                  . (navimMode . _NavigationMode . displayMessage
                     .~ Neutral "KEY NOT BOUND")

dispatchKey :: NavimState NavimCommand
            -> Key
            -> EventM n (Next (NavimState NavimCommand))
            -> EventM n (Next (NavimState NavimCommand))
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
            meta ^. metaInput . to (metaCommand ns)

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
  where
    safeInit [] = []
    safeInit xs = init xs

    performInputCommand ns cmd = do
        dcResult <- liftIO . inputCommand $ ns
        ns'      <- liftIO . buildState $ Just ns
        continue $
            ns' & navimMode
                .~ NavigationMode
                       (Navigation $ case dcResult of
                            DCSuccess -> Success cmd
                            DCError e -> Error cmd e
                       )

