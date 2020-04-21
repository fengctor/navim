module Navim.DirContent where

import System.Directory

import Data.Bifunctor
import Data.Bool
import Data.List
import Data.Traversable

-- TODO: instead of Bool, use a Success/Failure type
-- ie. boolean blindness bad

data DirContent
    = File FilePath
    | Directory FilePath
    deriving (Show, Eq, Ord)

-- TODO: move to a different module?
splitOn _ [] = []
splitOn c (s:ss)
  | c == s = splitOn c ss
  | otherwise = let (left, right) = break (== c) (s:ss)
                    in left : splitOn c right

getPath :: DirContent -> FilePath
getPath (File fp)      = fp
getPath (Directory fp) = fp

fromRelativePath :: FilePath -> FilePath -> FilePath
fromRelativePath current rel =
    ('/':)
    . mconcat
    . intersperse "/"
    . reverse
    $ go (reverse . dirList $ current) (dirList rel)
  where
    go as     []        = as
    go as     (".":rs)  = go as rs
    go []     ("..":rs) = go [] rs
    go (a:as) ("..":rs) = go as rs
    go as     (r:rs)    = go (r:as) rs
    dirList = splitOn '/'

nameAndDirectory :: FilePath -> (FilePath, FilePath)
nameAndDirectory = bimap reverse reverse . break (== '/') . reverse

createDirContentSafe :: DirContent -> IO Bool
createDirContentSafe dc = do
    let path = getPath dc
    curDir <- getCurrentDirectory
    let (dcName, dcDir) = nameAndDirectory $ fromRelativePath curDir path
    dcDirContents <- getDirectoryContents dcDir
    if null dcName || dcName `elem` dcDirContents
        then return False
        else True
             <$ case dc of
                    File      name -> writeFile name ""
                    Directory name -> createDirectory name

-- newPath: relative path from current directory which dc will be renamed to
renameDirContentSafe :: String -> DirContent -> IO Bool
renameDirContentSafe newPath dc = do
    curDir <- getCurrentDirectory
    let (newPathName, newPathDir) = nameAndDirectory $ fromRelativePath curDir newPath
    newPathDirContents <- getDirectoryContents newPathDir
    if null newPathName || newPathName `elem` newPathDirContents
        then return False
        else True <$ renamePath (getPath dc) newPath

-- dest: absolute path to destination
copyDirContentSafe :: String -> DirContent -> IO Bool
copyDirContentSafe dest (Directory _) = return False -- Todo: handle directories
copyDirContentSafe dest (File name)   = do
    curDir <- getCurrentDirectory
    let (destName, destDir) = nameAndDirectory dest
    -- TODO: check destDir is a directory
    destDirContents <- getDirectoryContents destDir
    if null destName || destName `elem` destDirContents
       then return False
       else True <$ copyFile name dest

-- TODO: may fail based on permissions
removeDirContentSafe :: DirContent -> IO Bool
removeDirContentSafe (File name)      = True <$ removeFile name
removeDirContentSafe (Directory name) = True <$ removeDirectoryRecursive name

getCurrentDirContents :: IO [DirContent]
getCurrentDirContents = do
    curdir      <- getCurrentDirectory
    rawContents <- getDirectoryContents curdir
    for rawContents $ \fp ->
        bool (Directory fp) (File fp) <$> doesFileExist fp

getDirContents :: FilePath -> IO [DirContent]
getDirContents dir = do
    rawContents <- getDirectoryContents dir
    for rawContents $ \fp ->
        bool (Directory fp) (File fp) <$> doesFileExist fp
