{-# LANGUAGE OverloadedStrings #-}

module Navim where

import System.Directory
import System.Exit

import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE

import Control.Monad
import Control.Monad.IO.Class

import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Util
import Brick.Widgets.Core

import Cursor.Simple.List.NonEmpty

import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events

navim :: IO ()
navim = do initState <- buildInitialState
           endState  <- defaultMain navimApp initState
           let navPath = mconcat $ intersperse "/" $ reverse $ navHistory endState
           putStrLn navPath

data NavimState =
  NavimState
    { navimStatePaths :: NonEmptyCursor DirContent
    , navHistory :: [FilePath]
    } deriving (Show, Eq)

data ResourceName = ResourceName deriving (Show, Eq, Ord)

data DirContent = File FilePath | Directory FilePath deriving (Show, Eq, Ord)

getPath :: DirContent -> FilePath
getPath (File fp)      = fp
getPath (Directory fp) = fp

navimApp :: App NavimState e ResourceName
navimApp =
  App
    { appDraw = drawNavim
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleEvent
    , appStartEvent = pure
    , appAttrMap = const $ attrMap mempty [ ("selected", bg red)
                                          , ("file", fg white)
                                          , ("dir", fg green)
                                          ]
    }

buildInitialState :: IO NavimState
buildInitialState = do curdir      <- getCurrentDirectory
                       rawContents <- getDirectoryContents curdir
                       contents    <- traverse
                                        (\fp -> doesFileExist fp >>=
                                           \isFile -> return $ if isFile then File fp else Directory fp)
                                        rawContents
                       case NE.nonEmpty contents of
                            Nothing -> die "Should never happen (current directory \".\" always here)"
                            Just ne -> return $
                                         NavimState
                                           { navimStatePaths = makeNonEmptyCursor ne
                                           , navHistory = []
                                           }

drawNavim :: NavimState -> [Widget ResourceName]
drawNavim ns = [padBottom Max pathsWidget
                <=>
                str "Gary Feng"
               ]
  where pathsCursor = navimStatePaths ns
        pathsWidget = vBox $ mconcat
                         [ drawFilePath False <$> reverse (nonEmptyCursorPrev pathsCursor)
                         , [drawFilePath True $ nonEmptyCursorCurrent pathsCursor]
                         , drawFilePath False <$> nonEmptyCursorNext pathsCursor
                         ]

-- FilePath is a type synonym for String
drawFilePath :: Bool -> DirContent -> Widget n
drawFilePath selected dc = decorate $ str $ getPath dc
-- TODO: how to mix attributes
  where decorate = case dc of
                        File _      -> withAttr $ (if selected then ("selected" <>) else id) "file"
                        Directory _ -> withAttr $ (if selected then ("selected" <>) else id) "dir"

moveCursorWith :: (NonEmptyCursor DirContent -> Maybe (NonEmptyCursor DirContent)) ->
                  NavimState ->
                  EventM n (Next NavimState)
moveCursorWith moveFn state = continue $
  case moveFn (navimStatePaths state) of
       Nothing     -> state
       Just newNec -> state {navimStatePaths = newNec}

handleEvent :: NavimState -> BrickEvent n e -> EventM n (Next NavimState)
handleEvent s e =
  case e of
       VtyEvent vtye ->
         case vtye of
              --EvKey (KChar ':') [] -> do cmd <- liftIO getLine -- TODO: how do
                                         --if cmd == "q" then halt s else continue s
              EvKey (KChar 'q') [] -> halt s
              EvKey (KChar 'j') [] -> moveCursorWith nonEmptyCursorSelectNext s
              EvKey (KChar 'k') [] -> moveCursorWith nonEmptyCursorSelectPrev s
              EvKey (KChar 'r') [] -> do liftIO $ createDirectory "yeet"
                                         s' <- liftIO buildInitialState
                                         continue s'

              EvKey KDown [] -> moveCursorWith nonEmptyCursorSelectNext s
              EvKey KUp []   -> moveCursorWith nonEmptyCursorSelectPrev s

              EvKey KEnter [] -> case nonEmptyCursorCurrent $ navimStatePaths s of
                                      File _       -> continue s
                                      Directory fp -> do liftIO $ setCurrentDirectory fp
                                                         let newHistory = case (navHistory s, fp) of
                                                                               (ps  , "." ) -> ps
                                                                               ([]  , "..") -> [".."]
                                                                               ("..":ps, "..") -> "..":"..":ps
                                                                               (p:ps, "..") -> ps
                                                                               (ps  , next) -> next:ps
                                                         s' <- liftIO buildInitialState
                                                         continue $ s' {navHistory = newHistory}
              _ -> continue s
       _ -> continue s
  where
