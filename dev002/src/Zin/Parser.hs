{-# LANGUAGE OverloadedStrings #-}

module Zin.Parser 
  ( parseDocument
  , parseZinFile
  ) where

import qualified Data.Text as T
import Data.Text (Text)
import Control.Monad.Except
import Control.Monad.State
import Control.Monad
import Data.List (groupBy)

import Zin.Lexer
import Zin.AST
import Zin.Error

type Parser = ExceptT ParseError (State ParserState)

data ParserState = ParserState
  { currentLine :: Int
  , currentColumn :: Int
  , tokens :: [Token]
  , pendingStyles :: [Style]
  , pendingURL :: Maybe Text
  } deriving (Show)

parseZinFile :: Text -> Either ParseError Document
parseZinFile input = 
  let indentedLines = lexer input
      tokenList = tokenize indentedLines
  in parseDocument tokenList

parseDocument :: [Token] -> Either ParseError Document
parseDocument tokens = evalState (runExceptT parseDoc) initialState
  where
    initialState = ParserState 1 1 tokens [] Nothing
    parseDoc = do
      blocks <- parseBlocks
      return (Document blocks)

parseBlocks :: Parser [Block]
parseBlocks = do
  token <- peekToken
  case token of
    EOF -> return []
    Newline -> do
      _ <- consumeToken
      parseBlocks
    Comment _ -> do
      _ <- consumeToken
      parseBlocks
    Indent _ -> do
      _ <- consumeToken
      parseBlocks
    _ -> do
      block <- parseBlock
      rest <- parseBlocks
      return (block : rest)

parseBlock :: Parser Block
parseBlock = do
  token <- peekToken
  case token of
    TopLevelTag tag -> parseTopLevelBlock tag
    _ -> throwError (UnexpectedToken token)

parseTopLevelBlock :: Text -> Parser Block
parseTopLevelBlock tag
  | tag == "p" = parseParagraph
  | tag `elem` ["h1", "h2", "h3", "h4"] = parseHeader tag
  | tag `elem` ["ul", "ol", "tl"] = parseList tag
  | tag == "cb" = parseCodeBlock
  | tag == "q" = parseQuote
  | tag == "t" = parseTableHeader
  | tag == "r" = parseTableRow
  | otherwise = throwError (UnknownTag tag)

parseParagraph :: Parser Block
parseParagraph = do
  _ <- consumeToken -- consume 'p'
  styles <- collectStyles
  expectColon
  text <- parseTextContent
  continuationText <- parseContinuationLines
  let fullText = if T.null continuationText
                then text
                else text <> " " <> continuationText
  return (Paragraph styles fullText)

parseHeader :: Text -> Parser Block
parseHeader tag = do
  consumeToken -- consume header tag
  styles <- collectStyles
  expectColon
  text <- parseTextContent
  case getHeaderLevel tag of
    Just level -> return (Header level styles text)
    Nothing -> throwError (UnknownTag tag)

parseList :: Text -> Parser Block
parseList tag = do
  consumeToken -- consume list tag
  case getListType tag of
    Just listType -> do
      items <- parseListItems
      return (List listType items)
    Nothing -> throwError (UnknownTag tag)

parseListItems :: Parser [ListItem]
parseListItems = do
  token <- peekToken
  case token of
    Colon -> do
      _ <- consumeToken
      text <- parseTextContent
      let item = ListItem [] text
      rest <- parseListItems
      return (item : rest)
    _ -> return []

parseContinuationLines :: Parser Text
parseContinuationLines = do
  token <- peekToken
  case token of
    Newline -> do
      _ <- consumeToken
      nextToken <- peekToken
      case nextToken of
        Indent _ -> do
          _ <- consumeToken
          colonToken <- peekToken
          case colonToken of
            Colon -> do
              _ <- consumeToken
              text <- parseTextContent
              restText <- parseContinuationLines
              return $ if T.null restText
                      then text
                      else text <> " " <> restText
            _ -> return ""
        _ -> return ""
    _ -> return ""

parseCodeBlock :: Parser Block
parseCodeBlock = do
  consumeToken -- consume 'cb'
  expectColon
  code <- parseTextContent
  
  -- Try to parse language specification
  langToken <- peekToken
  case langToken of
    Text lang -> do
      consumeToken
      expectColon
      moreCode <- parseMultiLineText
      return (CodeBlock lang (code <> "\n" <> moreCode))
    _ -> return (CodeBlock "text" code)

parseQuote :: Parser Block
parseQuote = do
  consumeToken -- consume 'q'
  styles <- collectStyles
  expectColon
  text <- parseTextContent
  return (Quote styles text)

parseTableHeader :: Parser Block
parseTableHeader = do
  consumeToken -- consume 't'
  expectColon
  headers <- parseTableCells
  return (Table [HeaderRow headers])

parseTableRow :: Parser Block
parseTableRow = do
  consumeToken -- consume 'r'
  expectColon
  cells <- parseTableCells
  return (Table [DataRow cells])

parseTableCells :: Parser [Text]
parseTableCells = do
  -- テーブルの場合は特別な処理を行う
  fullText <- parseTableText
  return (parseTableCellsFromText fullText)

parseTableText :: Parser Text
parseTableText = do
  token <- peekToken
  case token of
    Text content -> do
      _ <- consumeToken
      rest <- parseTableText
      return (content <> rest)
    Pipe -> do
      _ <- consumeToken
      rest <- parseTableText
      return ("|" <> rest)
    _ -> return ""

parseTableCellsFromText :: Text -> [Text]
parseTableCellsFromText text =
  let textStr = T.unpack text
      cells = splitTableCells textStr
  in map (T.strip . T.pack) cells

splitTableCells :: String -> [String]
splitTableCells input = 
  case dropWhile (== '|') input of
    [] -> []
    str -> case break (== '|') str of
      (cell, rest) -> cell : splitTableCells rest

collectStyles :: Parser [Style]
collectStyles = do
  token <- peekToken
  case token of
    StyleTag name range -> do
      consumeToken
      url <- collectURL
      let style = createStyle name range url
      rest <- collectStyles
      return (style : rest)
    _ -> return []

collectURL :: Parser (Maybe Text)
collectURL = do
  token <- peekToken
  case token of
    URL url -> do
      consumeToken
      return (Just url)
    _ -> return Nothing

createStyle :: Text -> [Int] -> Maybe Text -> Style
createStyle "bold" [s, e] _ = Bold (s, e)
createStyle "italic" [s, e] _ = Italic (s, e)
createStyle "strike" [s, e] _ = Strike (s, e)
createStyle "link" [s, e] (Just url) = Link (s, e) url
createStyle "link" [s, e] Nothing = Link (s, e) ""
createStyle _ _ _ = Bold (0, 0) -- fallback

parseTextContent :: Parser Text
parseTextContent = do
  token <- peekToken
  case token of
    Text content -> do
      consumeToken
      rest <- parseTextContent
      return (content <> rest)
    _ -> return ""

parseMultiLineText :: Parser Text
parseMultiLineText = do
  token <- peekToken
  case token of
    Text content -> do
      consumeToken
      expectColon
      rest <- parseMultiLineText
      return (content <> "\n" <> rest)
    Newline -> do
      consumeToken
      parseMultiLineText
    EOF -> return ""
    _ -> return ""

-- Parser utility functions
peekToken :: Parser Token
peekToken = do
  state <- get
  case tokens state of
    [] -> return EOF
    (t:_) -> return t

consumeToken :: Parser Token
consumeToken = do
  state <- get
  case tokens state of
    [] -> return EOF
    (t:ts) -> do
      put state { tokens = ts }
      return t

expectColon :: Parser ()
expectColon = do
  token <- consumeToken
  case token of
    Colon -> return ()
    _ -> throwError MissingColon

expectPipe :: Parser ()
expectPipe = do
  token <- consumeToken
  case token of
    Pipe -> return ()
    _ -> throwError InvalidTableFormat