{-# LANGUAGE TemplateHaskell #-}

module Navim.NavimState where


import           Brick.Types

import           Control.Lens

import           Cursor.Simple.List.NonEmpty

import qualified Data.HashMap                as Map
import           Data.List
import qualified Data.List.NonEmpty          as NE

import           Navim.DirContent
import           Navim.NavimConfig

import           System.Directory
import           System.Exit

data InputCommand
    = CreateFile
    | CreateDirectory
    | Remove
    | Rename
    | Copy
    | Paste
    deriving (Show, Eq)

messageString :: Message -> String
messageString Indicate = "--NAVIGATION--"

messageString (Neutral msg) = msg

messageString (Success CreateFile)      = "File creation succeeded"
messageString (Success CreateDirectory) = "Directory creation succeeded"
messageString (Success Remove)          = "Deletion succeeded"
messageString (Success Rename)          = "Renaming succeeded"
messageString (Success Copy)            = "Copied content to clipboard"
messageString (Success Paste)           = "Clipboard content pasted"

messageString (Error command reason) =
    mconcat
        [ failedCommandMsg
        , ": "
        , failedReasonMsg
        ]
  where
    failedCommandMsg =
        case command of
            CreateFile      -> "File creation failed"
            CreateDirectory -> "Directory creation failed"
            Remove          -> "Deletion failed"
            Rename          -> "Renaming failed"
            Copy            -> "Cannot copy content to clipboard"
            Paste           -> "Cannot paste clipboard content"

    failedReasonMsg =
        case reason of
            AlreadyExists name dir ->
                mconcat
                    [ name
                    , " already exists in directory "
                    , dir
                    ]
            DoesNotExist name ->
                mconcat
                    [ name
                    , " does not exist"
                    ]
            InsufficientPermissions name ->
                mconcat
                    [ "insufficient permissions for"
                    , name
                    ]
            InvalidName name ->
                mconcat
                    [ name
                    , " is invalid"
                    ]
            Cancelled ->
                "command was cancelled"

data Message
    = Indicate
    | Neutral String
    | Success InputCommand
    | Error InputCommand DirContentActionError
    deriving (Show, Eq)

newtype Navigation
    = Navigation
        { _displayMessage :: Message }
    deriving (Show, Eq)
makeLenses ''Navigation

newtype Meta
    = Meta
        { _metaInput :: String }
    deriving (Show, Eq)
makeLenses ''Meta

data Input
    = Input
        { _inputCommand  :: InputCommand
        , _inputResponse :: String
        }
    deriving (Show, Eq)
makeLenses ''Input

data Mode
    = NavigationMode Navigation
    | MetaMode Meta
    | InputMode Input
    deriving (Show, Eq)
makePrisms ''Mode

-- Undo stack, current directory, and redo stack
data DirHistory
    = DirHistory
        { _undoDirectories  :: [FilePath]
        , _currentDirectory :: FilePath
        , _redoDirectories  :: [FilePath]
        }
    deriving (Show, Eq)
makeLenses ''DirHistory

undoDirHistory :: DirHistory -> DirHistory
undoDirHistory dh =
    case dh of
        (DirHistory [] _ _)      -> dh
        (DirHistory (u:us) c rs) -> DirHistory us u (c:rs)

redoDirHistory :: DirHistory -> DirHistory
redoDirHistory dh =
    case dh of
        (DirHistory _ _ [])      -> dh
        (DirHistory us c (r:rs)) -> DirHistory (c:us) r rs

withNewCurrentDir :: FilePath -> DirHistory -> DirHistory
withNewCurrentDir fp (DirHistory us c rs) = DirHistory (c:us) fp []

data ClipType
    = Replicate
    | Move
    deriving (Show, Eq)

data NavimClipboard
    = NavimClipboard
        { _clipboardContent :: Maybe DirContent
        , _clipType         :: ClipType
        }
    deriving (Show, Eq)
makeLenses ''NavimClipboard

data NavimState n
    = NavimState
        { _navimStatePaths :: NonEmptyCursor DirContent
        , _navimHistory    :: DirHistory
        , _navimMode       :: Mode
        , _navimClipboard  :: NavimClipboard
        , _navimSearch     :: String
        , _navimWidth      :: Int
        , _navimConfig     :: NavimConfig n
        }
    deriving (Show, Eq)

makeLenses ''NavimState

-- State Transformer (and I don't mean the monad ;))
buildState :: Maybe (NavimState n) -> IO (NavimState n)
buildState prevState = do
    curDir   <- getCurrentDirectory
    contents <- getDirContents curDir
    let sortedContents = uncurry (++) $
                             partition
                                 ((== Directory) . (contentType))
                                 contents
    case NE.nonEmpty sortedContents of
        Nothing -> die "Should never happen (current directory \".\" always here)"
        Just ne ->
            case prevState of
                Nothing ->
                    pure NavimState
                        { _navimStatePaths = makeNonEmptyCursor ne
                        , _navimHistory = DirHistory [] curDir []
                        , _navimMode = NavigationMode $ Navigation Indicate
                        , _navimClipboard = NavimClipboard Nothing Replicate
                        , _navimSearch = ""
                        , _navimWidth = 1
                        , _navimConfig = NavimConfig Map.empty
                        }
                Just ps ->
                    pure $
                        ps & navimStatePaths
                           %~ adjustCursor (makeNonEmptyCursor ne)
  where
    adjustCursor newNec oldNec =
        moveNextBy (length $ nonEmptyCursorPrev oldNec) newNec

    moveNextBy 0 newNec = newNec
    moveNextBy n newNec =
        case nonEmptyCursorSelectNext newNec of
            Nothing   -> newNec
            Just nec' -> moveNextBy (n - 1) nec'

