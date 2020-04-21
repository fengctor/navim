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

errorMessage :: Command -> String
errorMessage CreateFile      = "Could not create file"
errorMessage CreateDirectory = "Could not create directory"
errorMessage Remove          = "Could not delete selection"
errorMessage Rename          = "Could not perform renaming"
errorMessage Copy            = "Could not copy selection"
errorMessage Paste           = "Could not perform paste"

-- TODO: have field be a message to display, which can be an error or a regular message
newtype Navigation
    = Navigation
        { _errored :: Maybe Command }
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
