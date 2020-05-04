{-# LANGUAGE TemplateHaskell #-}

module Navim.NavimState where

import           Control.Lens

import           Data.HashMap                (Map)

import           Brick.Types

import           Cursor.Simple.List.NonEmpty

import           Graphics.Vty.Input.Events

import           Navim.DirContent

data Command
    = CreateFile
    | CreateDirectory
    | Remove
    | Rename
    | Copy
    | Paste
    deriving (Show, Eq)

messageString :: Message -> String
messageString Indicate = "--NAVIGATION--"

messageString (Neutral msg) = msg

messageString (Success CreateFile)      = "File creation succeeded"
messageString (Success CreateDirectory) = "Directory creation succeeded"
messageString (Success Remove)          = "Deletion succeeded"
messageString (Success Rename)          = "Renaming succeeded"
messageString (Success Copy)            = "Copied content to clipboard"
messageString (Success Paste)           = "Clipboard content pasted"

messageString (Error command reason) =
    mconcat
        [ failedCommandMsg
        , ": "
        , failedReasonMsg
        ]
  where
    failedCommandMsg =
        case command of
            CreateFile      -> "File creation failed"
            CreateDirectory -> "Directory creation failed"
            Remove          -> "Deletion failed"
            Rename          -> "Renaming failed"
            Copy            -> "Cannot copy content to clipboard"
            Paste           -> "Cannot paste clipboard content"

    failedReasonMsg =
        case reason of
            AlreadyExists name dir ->
                mconcat
                    [ name
                    , " already exists in directory "
                    , dir
                    ]
            DoesNotExist name ->
                mconcat
                    [ name
                    , " does not exist"
                    ]
            InsufficientPermissions name ->
                mconcat
                    [ "insufficient permissions for"
                    , name
                    ]
            InvalidName name ->
                mconcat
                    [ name
                    , " is invalid"
                    ]
            Cancelled ->
                "command was cancelled"

data Message
    = Indicate
    | Neutral String
    | Success Command
    | Error Command DirContentActionError
    deriving (Show, Eq)

newtype Navigation
    = Navigation
        { _displayMessage :: Message }
    deriving (Show, Eq)
makeLenses ''Navigation

newtype Meta
    = Meta
        { _metaInput :: String }
    deriving (Show, Eq)
makeLenses ''Meta

data Input
    = Input
        { _command       :: Command
        , _inputResponse :: String
        }
    deriving (Show, Eq)
makeLenses ''Input

data Mode
    = NavigationMode Navigation
    | MetaMode Meta
    | InputMode Input
    deriving (Show, Eq)
makePrisms ''Mode

-- Undo stack, current directory, and redo stack
data DirHistory
    = DirHistory
        { _undoDirectories  :: [FilePath]
        , _currentDirectory :: FilePath
        , _redoDirectories  :: [FilePath]
        }
    deriving (Show, Eq)
makeLenses ''DirHistory

undoDirHistory :: DirHistory -> DirHistory
undoDirHistory dh =
    case dh of
        (DirHistory [] _ _)      -> dh
        (DirHistory (u:us) c rs) -> DirHistory us u (c:rs)

redoDirHistory :: DirHistory -> DirHistory
redoDirHistory dh =
    case dh of
        (DirHistory _ _ [])      -> dh
        (DirHistory us c (r:rs)) -> DirHistory (c:us) r rs

withNewCurrentDir :: FilePath -> DirHistory -> DirHistory
withNewCurrentDir fp (DirHistory us c rs) = DirHistory (c:us) fp []

data ClipType
    = Replicate
    | Move
    deriving (Show, Eq)

data NavimClipboard
    = NavimClipboard
        { _clipboardContent :: Maybe DirContent
        , _clipType         :: ClipType
        }
    deriving (Show, Eq)
makeLenses ''NavimClipboard

-- TODO: other fields
data NavimConfig commandType
    = NavimConfig
        { _commandMap :: Map (Key, [Modifier]) commandType
        }
    deriving (Show, Eq)

data NavimState n
    = NavimState
        { _navimStatePaths :: NonEmptyCursor DirContent
        , _navimHistory    :: DirHistory
        , _navimMode       :: Mode
        , _navimClipboard  :: NavimClipboard
        , _navimSearch     :: String
        , _navimWidth      :: Int
        , _navimConfig     :: NavimConfig n
        }
    deriving (Show, Eq)

makeLenses ''NavimConfig
makeLenses ''NavimState
