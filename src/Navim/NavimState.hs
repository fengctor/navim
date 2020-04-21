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
messageString (Error CreateFile)         = "File creation failed"
messageString (Error CreateDirectory)    = "Directory creation failed"
messageString (Error Remove)             = "Deletion failed"
messageString (Error Rename)             = "Renaming failed"
messageString (Error Copy)               = "Cannot copy content to clipboard"
messageString (Error Paste)              = "CAnnot paste clipboard content"

data Message
    = Indicate
    | Success Command
    | Error Command
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
