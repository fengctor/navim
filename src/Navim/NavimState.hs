{-# LANGUAGE TemplateHaskell #-}

module Navim.NavimState where

import Cursor.Simple.List.NonEmpty

import Control.Lens

import Navim.DirContent

data Command
    = CreateFile
    | CreateDirectory
    | Remove
    | Rename
    | Copy
    | Paste
    deriving (Show, Eq)

messageString :: Message -> String
messageString Indicate                   = "--NAVIGATION--"

messageString (Success CreateFile)       = "File creation succeeded"
messageString (Success CreateDirectory)  = "Directory creation succeeded"
messageString (Success Remove)           = "Deletion succeeded"
messageString (Success Rename)           = "Renaming succeeded"
messageString (Success Copy)             = "Copied content to clipboard"
messageString (Success Paste)            = "Clipboard content pasted"

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
    | Success Command
    | Error Command DirContentActionError
    deriving (Show, Eq)

-- TODO: have field be a message to display, which can be an error or a regular message
newtype Navigation
    = Navigation
        { _displayMessage :: Message }
    deriving (Show, Eq)
makeLenses ''Navigation

newtype Colon
    = Colon
        { _colonInput :: String }
    deriving (Show, Eq)
makeLenses ''Colon

data Input
    = Input
        { _command :: Command
        , _inputResponse :: String
        }
    deriving (Show, Eq)
makeLenses ''Input

-- TODO: something about prisms for this
data Mode
    = NavigationMode Navigation -- normal file navigation
    | ColonMode Colon           -- colon commands: ends when input is empty or enter pressed
    | InputMode Input           -- waiting for user input with a given prompt
    deriving (Show, Eq)
makePrisms ''Mode

data NavimState = NavimState
    { _navimStatePaths :: NonEmptyCursor DirContent
    , _navimHistory :: [FilePath]
    , _navimMode :: Mode
    , _navimClipboard :: Maybe DirContent
    } deriving (Show, Eq)
makeLenses ''NavimState
