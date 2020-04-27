module Navim.Instances.Hashable where

import Data.Hashable

import Graphics.Vty.Input.Events

instance Hashable Key where
    hashWithSalt n (KChar c) = n + fromEnum c
    hashWithSalt n _ = n + 10

instance Hashable Modifier where
    hashWithSalt n MShift = n
    hashWithSalt n MCtrl  = n + 1
    hashWithSalt n MMeta  = n + 2
    hashWithSalt n MAlt   = n + 3
