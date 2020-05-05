{-# LANGUAGE TemplateHaskell #-}

module Navim.NavimConfig where

import           Control.Lens

import           Data.HashMap              (Map)

import           Graphics.Vty.Input.Events

-- TODO: other fields
data NavimConfig commandType
    = NavimConfig
        { _commandMap :: Map (Key, [Modifier]) commandType
        }
    deriving (Show, Eq)
makeLenses ''NavimConfig
