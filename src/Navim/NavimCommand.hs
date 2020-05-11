{-# LANGUAGE Rank2Types #-}

module Navim.NavimCommand where

import           System.Directory
import           System.Process

import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class

import           Data.Char
import           Data.List
import           Data.Maybe

import           Brick.Main
import           Brick.Types

import qualified Cursor.List.NonEmpty        as NEC (NonEmptyCursor (..))
import           Cursor.Simple.List.NonEmpty

import           Navim.DirContent
import           Navim.NavimConfig
import           Navim.NavimState

data CursorMovement
    = CursorUp
    | CursorDown
    | CursorTop
    | CursorBottom
    deriving (Show, Eq)

data DirHistoryModifier
    = Undo
    | Redo
    deriving (Show, Eq)

data NoInputCommand
    = MoveCursor CursorMovement
    | SelectedToClipboard ClipType
    | ChangeDirHistory DirHistoryModifier
    | PerformSearch
    | NavigateSelected
    deriving (Show, Eq)

data WithInputCommand
    = CreateContent ContentType
    | ModifySelected InputCommand
    | PasteClipboard
    deriving (Show, Eq)

data InternalCommand
    = NoInput NoInputCommand
    | WithInput WithInputCommand
    | Sequence [NoInputCommand] (Maybe WithInputCommand)
    deriving (Show, Eq)

data ExternalCommand
    = BashCommandOnSelected String
    deriving (Show, Eq)

data NavimCommand
    = Internal InternalCommand
    | External ExternalCommand
    deriving (Show, Eq)

noInputFunction :: NoInputCommand
                -> NavimState NavimCommand
                -> IO (NavimState NavimCommand)
noInputFunction (MoveCursor cursorMvmt) =
    return . moveCursorWith (case cursorMvmt of
        CursorUp     -> nonEmptyCursorSelectPrev
        CursorDown   -> nonEmptyCursorSelectNext
        CursorTop    -> Just . nonEmptyCursorSelectFirst
        CursorBottom -> Just . nonEmptyCursorSelectLast
    )
noInputFunction (SelectedToClipboard cliptype) =
    return . selectedToClipboard cliptype
noInputFunction (ChangeDirHistory dhm) =
    changeDirHistoryWith $ case dhm of
        Undo -> undoDirHistory
        Redo -> redoDirHistory
noInputFunction PerformSearch =
    return . performSearch
noInputFunction NavigateSelected =
    navigateSelected

withInputFunction :: WithInputCommand
                  -> NavimState NavimCommand
                  -> IO (NavimState NavimCommand)
withInputFunction (CreateContent ctype) =
    return . toInputMode (case ctype of
        File      -> CreateFile
        Directory -> CreateDirectory
    )
withInputFunction (ModifySelected cmd) =
    return . modifySelectedWith cmd
withInputFunction PasteClipboard =
    return . pasteClipboard

internalFunction :: InternalCommand
                 -> NavimState NavimCommand
                 -> IO (NavimState NavimCommand)
internalFunction (NoInput nic)   = noInputFunction nic
internalFunction (WithInput wic) = withInputFunction wic
internalFunction (Sequence nics mwic) =
    foldr
        ((>=>) . noInputFunction)
        (maybe return withInputFunction mwic)
        nics

externalFunction :: ExternalCommand
                 -> NavimState NavimCommand
                 -> IO (NavimState NavimCommand)
externalFunction (BashCommandOnSelected cmdStr) =
    bashCommandOnSelected cmdStr

commandFunction :: NavimCommand
                -> NavimState NavimCommand
                -> EventM n (Next (NavimState NavimCommand))
commandFunction (Internal i) = continue <=< (liftIO . internalFunction i)
commandFunction (External e) = suspendAndResume . externalFunction e

toInputMode :: InputCommand
            -> NavimState NavimCommand
            -> NavimState NavimCommand
toInputMode cmd = navimMode .~ InputMode (Input cmd "")

modifySelectedWith :: InputCommand
                   -> NavimState NavimCommand
                   -> NavimState NavimCommand
modifySelectedWith cmd ns =
    case ns ^. navimPaths
             . to nonEmptyCursorCurrent of
        DirContent Directory "." ->
            ns & navimMode
               . _NavigationMode . displayMessage
               .~ Error cmd (InvalidName ".")
        DirContent Directory ".." ->
            ns & navimMode
               . _NavigationMode . displayMessage
               .~ Error cmd (InvalidName "..")
        _ -> toInputMode cmd ns

selectedToClipboard :: ClipType -> NavimState NavimCommand -> NavimState NavimCommand
selectedToClipboard ct ns =
    case ns ^. navimPaths
             . to nonEmptyCursorCurrent of
        DirContent Directory name ->
            ns & navimMode
               . _NavigationMode . displayMessage
               .~ Error Copy (InvalidName name)
        DirContent File name ->
            ns & navimMode
               . _NavigationMode . displayMessage
               .~ Success Copy
               & navimClipboard . clipboardContent
               .~ (ns ^. navimHistory
                       . currentDirectory
                       . to (Just
                            . DirContent File
                            . (++ '/':name)))
               & navimClipboard . clipType
               .~ ct

pasteClipboard :: NavimState NavimCommand -> NavimState NavimCommand
pasteClipboard ns =
    case ns ^. navimClipboard . clipboardContent of
        Nothing -> ns
        _       -> toInputMode Paste ns

bashCommandOnSelected :: String
                      -> NavimState NavimCommand
                      -> IO (NavimState NavimCommand)
bashCommandOnSelected cmdStr ns =
    ns <$
    callProcess
        cmdStr
        [ns ^. navimPaths
             . to (getPath . nonEmptyCursorCurrent)]

performSearch :: NavimState NavimCommand -> NavimState NavimCommand
performSearch ns =
    ns & navimMode
       . _NavigationMode . displayMessage
       .~ case nsSearch of
              "" -> Indicate
              _  -> Neutral ('/' : nsSearch)
       & navimPaths
       %~ \paths ->
           case nsSearch of
               "" -> paths
               savedQuery ->
                   fromMaybe paths $
                       nonEmptyCursorCircularSearch
                           ((savedQuery `isPrefixOf`)
                            . (toLower <$>)
                            . getPath)
                           paths
  where
    nsSearch = ns ^. navimSearch
                   . to (toLower <$>)

-- Note: clears the error message too
moveCursorWith :: (forall a . NonEmptyCursor a -> Maybe (NonEmptyCursor a))
               -> NavimState NavimCommand
               -> NavimState NavimCommand
moveCursorWith move ns =
    case (move $ ns ^. navimPaths, move $ ns ^. navimFileSizes) of
        (Just pnec, Just snec) ->
            ns & navimPaths
               .~ pnec
               & navimFileSizes
               .~ snec
               & navimMode . _NavigationMode . displayMessage
               .~ Indicate
        _ ->
            ns & navimMode . _NavigationMode . displayMessage
               .~ Indicate

navigateSelected :: NavimState NavimCommand -> IO (NavimState NavimCommand)
navigateSelected ns =
    case ns ^. navimPaths
             . to nonEmptyCursorCurrent of
        DirContent File fp ->
            return $
                ns & navimMode . _NavigationMode . displayMessage
                   .~ Indicate -- TODO: proper error message

        DirContent Directory fp -> do
            let (curDir, _) = ns ^. navimHistory
                                  . currentDirectory
                                  . to nameAndDirectory
            let nextFocus = if fp == ".." then curDir else "."
            liftIO $ setCurrentDirectory fp
            newCurDir <- getCurrentDirectory
            ns'       <- buildState . Just $
                             ns & navimPaths
                                %~ nonEmptyCursorReset
            return $
                let newPaths = getNewPaths nextFocus (ns' ^. navimPaths) in
                ns' & navimMode . _NavigationMode . displayMessage
                    .~ Indicate
                    & navimHistory
                    %~ withNewCurrentDir newCurDir
                    & navimPaths
                    .~ newPaths
                    & navimFileSizes
                    %~ (syncCursorWith newPaths . nonEmptyCursorReset)
  where
    getNewPaths nextFocus paths =
        fromMaybe (nonEmptyCursorReset paths) $
            nonEmptyCursorSearch
                ((== nextFocus) . getPath)
                paths

    syncCursorWith goal toSync =
        case ( nonEmptyCursorSelectPrev goal
             , nonEmptyCursorSelectNext toSync
             ) of
            (Just goalPrev, Just toSyncNext) ->
                syncCursorWith goalPrev toSyncNext
            _ ->
                toSync

-- TODO: use navigate
previewOrNavigate :: NavimState NavimCommand -> EventM n (Next (NavimState NavimCommand))
previewOrNavigate ns =
    case ns ^. navimPaths
             . to nonEmptyCursorCurrent of
        DirContent File fp ->
            suspendAndResume $
                ns <$
                callProcess
                    "less"
                    [ns ^. navimPaths
                         . to (getPath . nonEmptyCursorCurrent)]
        DirContent Directory fp ->
            (liftIO . navigateSelected >=> continue) ns

runMetaCommand :: NavimState NavimCommand
               -> String
               -> EventM n (Next (NavimState NavimCommand))
runMetaCommand ns input =
    case input of
        ":q" -> halt ns
        -- TODO: other meta commands
        ':':'r':'u':'n':' ':cmd ->
            suspendAndResume $
                callCommand cmd
                >> (buildState . Just $
                       ns & navimMode
                          .~ NavigationMode (Navigation Indicate))
        ":clipboard" ->
            continue $
                ns & navimMode
                   .~ NavigationMode (Navigation . Neutral $
                          case ns ^. navimClipboard of
                              NavimClipboard Nothing _ -> "Clipboard is empty"

                              NavimClipboard (Just clip) Replicate ->
                                  getPath clip ++ " [COPIED]"
                              NavimClipboard (Just clip) Move ->
                                  getPath clip ++ " [CUT]")
        ['/'] ->
            continue $
                ns & navimMode
                   .~ NavigationMode (Navigation Indicate)
                   & navimSearch
                   .~ ""
        '/':cs ->
            let searchQuery = toLower <$> cs in
            continue $
                ns & navimMode
                   .~ NavigationMode (Navigation Indicate)
                   & navimSearch
                   .~ searchQuery
                   & navimPaths
                   %~ \paths ->
                       fromMaybe paths $
                           nonEmptyCursorCircularSearch
                               ((searchQuery `isPrefixOf`)
                                . (toLower <$>)
                                . getPath)
                               paths
        _ -> continue $
                 ns & navimMode
                    .~ NavigationMode (Navigation Indicate)

-- TODO: maybe having Command as a param is a better idea
runInputCommand :: NavimState n -> IO DirContentActionResult
runInputCommand ns =
    case ns ^. navimMode of
        InputMode input ->
            let entered = input ^. inputResponse in
            case input ^. inputCommand of
                CreateFile ->
                    createDirContentSafe $ DirContent File entered
                CreateDirectory ->
                    createDirContentSafe $ DirContent Directory entered
                Remove ->
                    case entered of
                        "y" -> onSelected removeDirContentSafe
                        _   -> pure $ DCError Cancelled
                Rename ->
                    onSelected $ renameDirContentSafe entered
                Paste ->
                    case (entered, ns ^. navimClipboard . clipboardContent) of
                        ("y", Just clip) -> do
                            let (clipName, _) = nameAndDirectory . getPath $ clip
                            result <- copyDirContentSafe
                                          (ns ^. navimHistory
                                               . currentDirectory
                                               . to (++ '/':clipName))
                                          clip
                            case ns ^. navimClipboard . clipType of
                                 Move -> removeDirContentSafe clip
                                 _    -> pure result
                        _ ->
                            pure $ DCError Cancelled
        _ -> pure $ DCError Cancelled -- TODO: kind of a silent error
 where
    onSelected f = ns ^. navimPaths
                       . to (f . nonEmptyCursorCurrent)

changeDirHistoryWith :: (DirHistory -> DirHistory) -> NavimState n -> IO (NavimState n)
changeDirHistoryWith changeFn ns = do
    let newHistory = ns ^. navimHistory
                         . to changeFn
    let newCurDir = newHistory ^. currentDirectory
    validDir <- doesDirectoryExist newCurDir
    if validDir
        then
            setCurrentDirectory newCurDir *>
            ((navimHistory .~ newHistory) <$> buildState (Just ns))
        else
            pure $       -- todo: handle differently
                ns & navimHistory
                   . undoDirectories
                   .~ []
                   & navimHistory
                   . redoDirectories
                   .~ []

nonEmptyCursorReset :: NonEmptyCursor a -> NonEmptyCursor a
nonEmptyCursorReset = makeNonEmptyCursor . rebuildNonEmptyCursor

nonEmptyCursorCircularSearch :: (a -> Bool)
                             -> NonEmptyCursor a
                             -> Maybe (NonEmptyCursor a)
nonEmptyCursorCircularSearch p nec@(NEC.NonEmptyCursor prev cur next) =
    searchWithGas
        (length prev + length next)
        p
        (nonEmptyCursorSelectNextOrCycle nec)
  where
    nonEmptyCursorSelectNextOrCycle nec =
        fromMaybe (nonEmptyCursorSelectFirst nec) (nonEmptyCursorSelectNext nec)

    searchWithGas 0 _ nec = Nothing
    searchWithGas n p nec
        | p (nonEmptyCursorCurrent nec)
            = Just nec
        | otherwise
            = searchWithGas
                  (n - 1)
                  p
                  (nonEmptyCursorSelectNextOrCycle nec)

{- END Event Handler Helpers -}
