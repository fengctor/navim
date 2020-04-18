{-# LANGUAGE TemplateHaskell #-}

module Data.NavimState where

import Cursor.Simple.List.NonEmpty

import Control.Lens

import Navim.DirContent

data Prompt
    = CreateFile
    | CreateDirectory
    | Remove
    | Rename
    deriving (Show, Eq)

data Navigation
    = Navigation
    deriving (Show, Eq)
makeLenses ''Navigation

data Colon
    = Colon { _colonInput :: String }
    deriving (Show, Eq)
makeLenses ''Colon

data Input
    = Input
        { _prompt :: Prompt        -- TODO: maybe sum type for prompt?
        , _inputResponse :: String -- TODO: different data structure for fast snoc?
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
    } deriving (Show, Eq)
makeLenses ''NavimState
