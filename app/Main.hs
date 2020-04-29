module Main where

import Navim
import Navim.NavimState

main :: IO ()
main = navim (NavimConfig defaultCommandMap)
