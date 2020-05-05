{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}

module Navim.NavimConfig.Parser where

import           Control.Applicative

import           Data.Bool
import           Data.Char
import           Data.HashMap              (Map)
import qualified Data.HashMap              as Map

import           Graphics.Vty.Input.Events

import           Navim.NavimCommand
import           Navim.NavimConfig

import           Navim.DirContent          (ContentType (..))
import           Navim.Instances.Hashable
import           Navim.NavimState          (ClipType (..), InputCommand (..))

newtype Parser a
    = Parser
        { runParser :: String -> Maybe (String, a)
        }

instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap f (Parser p) =
        Parser $ ((fmap . fmap) f) . p

instance Applicative Parser where
    -- pure :: a -> Parser a
    pure v = Parser $ Just . (, v)
    -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    Parser pf <*> Parser pa =
        Parser $ \inp ->
            do (inp' , f) <- pf inp
               (inp'', a) <- pa inp'
               pure (inp'', f a)

instance Alternative Parser where
    -- empty :: Parser a
    empty = Parser $ const empty
    -- (<|>) :: Parser a -> Parser a -> Parser a
    Parser p1 <|> Parser p2 =
        Parser $ (<|>) <$> p1 <*> p2


-- Combinator primitives

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \case
    (c:cs) | p c -> Just (cs, c)
    _            -> Nothing

item :: Parser Char
item = satisfy (const True)

char :: Char -> Parser Char
char x = satisfy (== x)

string :: String -> Parser String
string = traverse char

oneOf :: [(a, String)] -> Parser a
oneOf =
    foldr
        (\(res, s) r ->
            res <$ string s
            <|>
            r
        )
        empty

space :: Parser Char
space = satisfy isSpace

spaces :: Parser ()
spaces = () <$ many space

spaces1 :: Parser ()
spaces1 = () <$ some space

newline :: Parser ()
newline = () <$ satisfy (== '\n')

newlines :: Parser ()
newlines = () <$ many newline

newlines1 :: Parser ()
newlines1 = () <$ some newline

listWithSep :: Parser b -> Parser a -> Parser [a]
listWithSep psep p = (:) <$> p <*> many (psep *> p)

-- Key parser

modifiers :: [(Modifier, String)]
modifiers =
    [ (MCtrl, "ctrl")
    , (MMeta, "alt")
    ]

parseModifier :: Parser Modifier
parseModifier = oneOf modifiers

-- TODO: allow for non-alphanumeric keys
parseKey :: Parser Key
parseKey = KChar <$> satisfy isAlphaNum

-- eg: ctrl-a, alt-b, c
-- TODO: handle modifier ordering
parseKeyWithModifier :: Parser (Key, [Modifier])
parseKeyWithModifier =
    flip (,) <$> (many $ parseModifier <* char '-') <*> parseKey


-- Command Parser

cursorMovements :: [(CursorMovement, String)]
cursorMovements =
    [ (CursorUp, "up")
    , (CursorDown, "down")
    , (CursorTop, "top")
    , (CursorBottom, "bottom")
    ]

parseCursorMovement :: Parser CursorMovement
parseCursorMovement  = oneOf cursorMovements

clipTypes :: [(ClipType, String)]
clipTypes =
    [ (Replicate, "copy")
    , (Move, "cut")
    ]

parseClipType :: Parser ClipType
parseClipType = oneOf clipTypes

dirHistoryModifiers :: [(DirHistoryModifier, String)]
dirHistoryModifiers =
    [ (Undo, "undo")
    , (Redo, "redo")
    ]

parseDirHistoryModifier :: Parser DirHistoryModifier
parseDirHistoryModifier = oneOf dirHistoryModifiers

parseNoInputCommand :: Parser NoInputCommand
parseNoInputCommand =
    MoveCursor <$> (string "moveCursor" *> spaces1 *> parseCursorMovement)
    <|>
    SelectedToClipboard <$> (string "toClipboard" *> spaces1 *> parseClipType)
    <|>
    ChangeDirHistory <$> parseDirHistoryModifier
    <|>
    PerformSearch <$ string "search"
    <|>
    NavigateSelected <$ string "navigate"

contentTypes :: [(ContentType, String)]
contentTypes =
    [ (File, "file")
    , (Directory, "directory")
    ]

parseContentType :: Parser ContentType
parseContentType = oneOf contentTypes

selectionModifiyingInputCommands :: [(InputCommand, String)]
selectionModifiyingInputCommands =
    [ (Remove, "remove")
    , (Rename, "rename")
    ]

parseSelectionModifiyingInputCommand :: Parser InputCommand
parseSelectionModifiyingInputCommand = oneOf selectionModifiyingInputCommands

parseWithInputCommand :: Parser WithInputCommand
parseWithInputCommand =
    CreateContent <$> (string "create" *> spaces1 *> parseContentType)
    <|>
    ModifySelected <$> parseSelectionModifiyingInputCommand
    <|>
    PasteClipboard <$ string "pasteClipboard"

-- TODO: handle sequence optional withInputCommand
parseInternalCommand =
    NoInput <$> parseNoInputCommand
    <|>
    WithInput <$> parseWithInputCommand
    <|>
    Sequence <$>
        (char '[' *>
            spaces *>
            listWithSep
                (spaces *> char ',' *> spaces)
                parseNoInputCommand
            <*
            spaces <*
            char ']'
        ) <*>
        ((spaces *> (Just <$> parseWithInputCommand)) <|> pure Nothing)

escapeCharacters :: [(Char, String)]
escapeCharacters =
    [ ('\'', "\\'")
    , ('\"', "\\\"")
    , ('\n', "\\n")
    , ('\r', "\\r")
    , ('\t', "\\t")
    , ('\b', "\\b")
    , ('\\', "\\\\")
    ]

parseEscapeCharacter :: Parser Char
parseEscapeCharacter = oneOf escapeCharacters

parseStringLiteral :: Parser String
parseStringLiteral =
    char '"' *>
    many (parseEscapeCharacter <|> satisfy (/= '"')) <*
    char '"'

parseExternalCommand :: Parser ExternalCommand
parseExternalCommand =
    BashCommandOnSelected <$>
        (string "bash" *>
         spaces1 *>
         parseStringLiteral)

parseNavimCommand :: Parser NavimCommand
parseNavimCommand =
    Internal <$> parseInternalCommand
    <|>
    External <$> parseExternalCommand

parseCommandMap :: Parser (Map (Key, [Modifier]) NavimCommand)
parseCommandMap =
    Map.fromList <$>
        (newlines *>
         (listWithSep newlines1 $
             (,) <$> (parseKeyWithModifier <* spaces <* char ':' <* spaces)
                 <*> parseNavimCommand
         ) <*
         newlines
        )

parseNavimConfigWithNavimCommand :: Parser (NavimConfig NavimCommand)
parseNavimConfigWithNavimCommand =
    NavimConfig <$> parseCommandMap
