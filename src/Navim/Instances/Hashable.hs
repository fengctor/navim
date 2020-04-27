{-# LANGUAGE DeriveGeneric #-}

module Navim.Instances.Hashable where

import Data.Hashable

import Graphics.Vty.Input.Events

instance Hashable Key

instance Hashable Modifier
