module Navim.DirContent where

import System.Directory

import Data.Traversable
import Data.Bool

data DirContent
    = File FilePath
    | Directory FilePath
    deriving (Show, Eq, Ord)

getPath :: DirContent -> FilePath
getPath (File fp)      = fp
getPath (Directory fp) = fp

createDirContentSafe :: DirContent -> IO Bool
createDirContentSafe dc = do
    curdir      <- getCurrentDirectory
    contents    <- getDirectoryContents curdir
    if getPath dc `elem` contents
        then return False
        else True
             <$ case dc of
                    File      name -> writeFile name ""
                    Directory name -> createDirectory name

renameDirContentSafe :: DirContent -> String -> IO Bool
renameDirContentSafe dc newPath = do
    curdir      <- getCurrentDirectory
    contents    <- getDirectoryContents curdir
    if newPath `elem` contents
        then return False
        else True <$ renamePath (getPath dc) newPath

removeDirContent :: DirContent -> IO ()
removeDirContent (File name)      = removeFile name
removeDirContent (Directory name) = removeDirectoryRecursive name

getCurrentDirContents :: IO [DirContent]
getCurrentDirContents = do
    curdir      <- getCurrentDirectory
    rawContents <- getDirectoryContents curdir
    for rawContents $ \fp ->
        bool (Directory fp) (File fp) <$> doesFileExist fp
