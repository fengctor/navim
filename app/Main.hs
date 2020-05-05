module Main where

import           Navim
import           Navim.NavimConfig
import           Navim.NavimConfig.Parser
import           Navim.NavimState

main :: IO ()
main = navim (NavimConfig defaultCommandMap)
