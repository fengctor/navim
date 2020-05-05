module Main where

import           System.Environment

import           Navim
import           Navim.NavimConfig
import           Navim.NavimConfig.Parser
import           Navim.NavimState

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] ->
            navim (NavimConfig defaultCommandMap)
        ["-c", fileName] -> do
            confText <- readFile fileName
            case runParser parseNavimConfigWithNavimCommand confText of
                Just ("", config) -> navim config
                _ -> putStrLn $ mconcat ["failed to parse ", fileName]
