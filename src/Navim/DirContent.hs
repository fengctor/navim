module Navim.DirContent where

import           System.Directory

import           Data.Bifunctor
import           Data.Bool
import           Data.List
import           Data.Traversable

data DirContentActionError
    = AlreadyExists FilePath FilePath
    | DoesNotExist FilePath
    | InsufficientPermissions FilePath
    | InvalidName FilePath
    | Cancelled
    deriving (Show, Eq, Ord)

data DirContentActionResult
    = DCSuccess
    | DCError DirContentActionError
    deriving (Show, Eq, Ord)

data ContentType
    = File
    | Directory
    deriving (Show, Eq, Ord)

data DirContent
    = DirContent
        { contentType :: ContentType
        , getPath     :: FilePath
        }
    deriving (Show, Eq, Ord)

-- TODO: move to a different module?
splitOn _ [] = []
splitOn c (s:ss)
  | c == s = splitOn c ss
  | otherwise = let (left, right) = break (== c) (s:ss)
                    in left : splitOn c right

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

createDirContentSafe :: DirContent -> IO DirContentActionResult
createDirContentSafe dc = do
    let path = getPath dc
    curDir <- getCurrentDirectory
    let (dcName, dcDir) = nameAndDirectory $ fromRelativePath curDir path
    dcDirContents <- getDirectoryContents dcDir
    if null dcName
        then pure $ DCError (InvalidName dcName)
        else if dcName `elem` dcDirContents
            then pure $ DCError (AlreadyExists dcName dcDir)
            else DCSuccess
                 <$ case dc of
                        (DirContent File name)      -> writeFile name ""
                        (DirContent Directory name) -> createDirectory name

-- newPath: relative path from current directory which dc will be renamed to
renameDirContentSafe :: String -> DirContent -> IO DirContentActionResult
renameDirContentSafe newPath dc = do
    curDir <- getCurrentDirectory
    let (newPathName, newPathDir) = nameAndDirectory $ fromRelativePath curDir newPath
    newPathDirContents <- getDirectoryContents newPathDir
    if null newPathName
        then pure $ DCError (InvalidName newPathName)
        else if newPathName `elem` newPathDirContents
            then pure $ DCError (AlreadyExists newPathName newPathDir)
            else DCSuccess <$ renamePath (getPath dc) newPath

-- dest: absolute path to destination
-- Todo: handle directories
copyDirContentSafe :: String -> DirContent -> IO DirContentActionResult
copyDirContentSafe dest (DirContent Directory _) = error "not yet implemented"
copyDirContentSafe dest (DirContent File name) = do
    curDir <- getCurrentDirectory
    let (destName, destDir) = nameAndDirectory dest
    -- TODO: check destDir is a directory
    destDirContents <- getDirectoryContents destDir
    if null destName
        then pure $ DCError (InvalidName destName)
        else if destName `elem` destDirContents
            then pure $ DCError (AlreadyExists destName destDir)
            else DCSuccess <$ copyFile name dest

-- TODO: may fail based on permissions
removeDirContentSafe :: DirContent -> IO DirContentActionResult
removeDirContentSafe (DirContent File name) =
    DCSuccess <$ removeFile name
removeDirContentSafe (DirContent Directory name) =
    DCSuccess <$ removeDirectoryRecursive name

getDirContents :: FilePath -> IO [DirContent]
getDirContents dir = do
    rawContents <- getDirectoryContents dir
    for rawContents $ \fp ->
        bool (DirContent Directory fp) (DirContent File fp) <$>
        doesFileExist fp
