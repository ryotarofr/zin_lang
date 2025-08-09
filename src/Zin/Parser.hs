{-# LANGUAGE OverloadedStrings #-}

module Zin.Parser
  ( parseDocument,
    parseZinFile,
  )
where

import Control.Monad.Except
import Control.Monad.State
import Data.Text (Text)
import qualified Data.Text as T
import Zin.AST
import Zin.Error
import Zin.Lexer

type Parser = ExceptT ParseError (State ParserState)

data ParserState = ParserState
  { currentLine :: Int,
    currentColumn :: Int,
    tokens :: [Token],
    pendingStyles :: [Style],
    pendingURL :: Maybe Text
  }
  deriving (Show)

parseZinFile :: Text -> Either ParseError Document
parseZinFile input =
  let indentedLines = lexer input
      tokenList = tokenize indentedLines
   in parseDocument tokenList

parseDocument :: [Token] -> Either ParseError Document
parseDocument tokenList = evalState (runExceptT parseDoc) initialState
  where
    initialState = ParserState 1 1 tokenList [] Nothing
    parseDoc = Document <$> parseBlocks

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
    Colon -> do
      _ <- consumeToken
      parseBlocks
    TopLevelTag tag | tag `elem` ["t", "r"] -> do
      tableBlock <- parseTable
      rest <- parseBlocks
      return (tableBlock : rest)
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
  | tag == "t" = parseTable
  | tag == "r" = parseTable
  | otherwise = throwError (UnknownTag tag)

parseParagraph :: Parser Block
parseParagraph = do
  _ <- consumeToken -- consume 'p'
  styles <- collectStyles
  expectColon
  text <- parseTextContent
  continuationText <- parseContinuationLines
  let fullText =
        if T.null continuationText
          then text
          else text <> " " <> continuationText
  return (Paragraph styles fullText)

parseHeader :: Text -> Parser Block
parseHeader tag = do
  _ <- consumeToken -- consume header tag
  styles <- collectStyles
  expectColon
  text <- parseTextContent
  case getHeaderLevel tag of
    Just level -> return (Header level styles text)
    Nothing -> throwError (UnknownTag tag)

parseList :: Text -> Parser Block
parseList tag = do
  _ <- consumeToken -- consume list tag
  case getListType tag of
    Just listType -> do
      -- Check if next token is colon followed by newline (new syntax)
      nextToken <- peekToken
      case nextToken of
        Colon -> do
          _ <- consumeToken -- consume colon
          -- Check if followed by newline
          newlineToken <- peekToken
          case newlineToken of
            Newline -> do
              _ <- consumeToken -- consume newline
              List listType <$> parseNewStyleListItems
            _ -> do
              -- Old syntax: single item after colon
              text <- parseTextContent
              if T.null (T.strip text)
                then do
                  List listType <$> parseListItems
                else do
                  let item = ListItem [] text
                  rest <- parseListItems
                  return (List listType (item : rest))
        _ -> do
          List listType <$> parseListItems
    Nothing -> throwError (UnknownTag tag)

parseNewStyleListItems :: Parser [ListItem]
parseNewStyleListItems = do
  token <- peekToken
  case token of
    Colon -> do
      _ <- consumeToken -- consume colon
      text <- parseTextContent
      -- Consume newline if present
      nextToken <- peekToken
      case nextToken of
        Newline -> do
          _ <- consumeToken
          let item = ListItem [] text
          rest <- parseNewStyleListItems
          return (item : rest)
        _ -> do
          let item = ListItem [] text
          rest <- parseNewStyleListItems
          return (item : rest)
    Indent _ -> do
      _ <- consumeToken -- consume indent
      parseNewStyleListItems
    Newline -> do
      _ <- consumeToken -- consume newline
      parseNewStyleListItems
    _ -> return []

parseListItems :: Parser [ListItem]
parseListItems = do
  token <- peekToken
  case token of
    Colon -> do
      _ <- consumeToken
      text <- parseTextContent
      -- Skip empty list items (when colon is followed by newline immediately)
      if T.null (T.strip text)
        then parseListItems -- Skip this empty item and continue
        else do
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
              return $
                if T.null restText
                  then text
                  else text <> " " <> restText
            _ -> return ""
        _ -> return ""
    _ -> return ""

parseCodeBlock :: Parser Block
parseCodeBlock = do
  _ <- consumeToken -- consume 'cb'

  -- Check if there's a lang[...] specification
  langToken <- peekToken
  case langToken of
    LangTag lang -> do
      _ <- consumeToken -- consume lang[...]
      expectColon
      -- For code blocks, we don't need to preserve spaces after colon on first line
      code <- parseCodeBlockContent
      continuationLines <- parseContinuationCodeBlockLines
      let fullCode =
            if T.null continuationLines
              then T.stripEnd code
              else T.stripEnd code <> "\n" <> continuationLines
      return (CodeBlock lang fullCode)
    _ -> do
      expectColon
      -- For code blocks, we don't need to preserve spaces after colon on first line
      code <- parseCodeBlockContent
      continuationLines <- parseContinuationCodeBlockLines
      let fullCode =
            if T.null continuationLines
              then T.stripEnd code
              else T.stripEnd code <> "\n" <> continuationLines

      -- Check if the code is empty and there might be continuation lines for legacy format
      if T.null fullCode
        then do
          -- Try to parse legacy language specification on next line
          nextToken <- peekToken
          case nextToken of
            Newline -> do
              _ <- consumeToken
              indentToken <- peekToken
              case indentToken of
                Indent _ -> do
                  _ <- consumeToken
                  langOrCodeToken <- peekToken
                  case langOrCodeToken of
                    Text lang -> do
                      _ <- consumeToken
                      colonToken <- peekToken
                      case colonToken of
                        Colon -> do
                          _ <- consumeToken
                          CodeBlock lang <$> parseMultiLineText
                        _ -> return (CodeBlock "txt" lang)
                    _ -> return (CodeBlock "txt" "")
                _ -> return (CodeBlock "txt" "")
            _ -> return (CodeBlock "txt" fullCode)
        else return (CodeBlock "txt" fullCode)

parseQuote :: Parser Block
parseQuote = do
  _ <- consumeToken -- consume 'q'
  styles <- collectStyles
  expectColon
  Quote styles <$> parseTextContent

parseTable :: Parser Block
parseTable = do
  firstToken <- peekToken
  case firstToken of
    TopLevelTag tag | tag == "t" -> do
      _ <- consumeToken -- consume 't'
      expectColon
      headers <- parseTableCells
      rows <- parseTableRows
      return (Table (HeaderRow headers : rows))
    TopLevelTag tag | tag == "r" -> do
      _ <- consumeToken -- consume 'r'
      expectColon
      cells <- parseTableCells
      rows <- parseTableRows
      return (Table (DataRow cells : rows))
    _ -> throwError (UnexpectedToken firstToken)

parseTableRows :: Parser [Table]
parseTableRows = do
  token <- peekToken
  case token of
    Newline -> do
      _ <- consumeToken
      parseTableRows -- Skip newlines and continue
    TopLevelTag tag | tag `elem` ["t", "r"] -> do
      _ <- consumeToken -- consume tag
      expectColon
      cells <- parseTableCells
      rest <- parseTableRows
      let row = if tag == "t" then HeaderRow cells else DataRow cells
      return (row : rest)
    _ -> return []

parseTableCells :: Parser [Text]
parseTableCells = do parseTableCellsFromText <$> parseTableText

parseTableText :: Parser Text
parseTableText = do
  token <- peekToken
  case token of
    Text source -> do
      _ <- consumeToken
      rest <- parseTableText
      return (source <> rest)
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
      _ <- consumeToken
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
      _ <- consumeToken
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
    Text source -> do
      _ <- consumeToken
      rest <- parseTextContent
      return (source <> rest)
    _ -> return ""

parseTextContentWithColons :: Parser Text
parseTextContentWithColons = do
  token <- peekToken
  case token of
    Text source -> do
      _ <- consumeToken
      rest <- parseTextContentWithColons
      return (source <> rest)
    Colon -> do
      _ <- consumeToken
      rest <- parseTextContentWithColons
      return (":" <> rest)
    _ -> return ""

parseCodeBlockContent :: Parser Text
parseCodeBlockContent = do
  token <- peekToken
  case token of
    Text source -> do
      _ <- consumeToken
      rest <- parseCodeBlockContent
      return (source <> rest)
    Colon -> do
      _ <- consumeToken
      rest <- parseCodeBlockContent
      return (":" <> rest)
    Pipe -> do
      _ <- consumeToken
      rest <- parseCodeBlockContent
      return ("|" <> rest)
    StyleTag tagName range -> do
      _ <- consumeToken
      rest <- parseCodeBlockContent
      return (tagName <> "[" <> T.intercalate ":" (map (T.pack . show) range) <> "]" <> rest)
    LangTag lang -> do
      _ <- consumeToken
      rest <- parseCodeBlockContent
      return ("lang[" <> lang <> "]" <> rest)
    URL url -> do
      _ <- consumeToken
      rest <- parseCodeBlockContent
      return ("url[" <> url <> "]" <> rest)
    Newline -> return "" -- Stop at newline but don't consume it
    EOF -> return ""
    _ -> return ""

parseMultiLineTextWithFirstLine :: Parser Text
parseMultiLineTextWithFirstLine = do
  firstLine <- parseCodeBlockContent
  continuationLines <- parseContinuationCodeBlockLines
  let trimmedFirstLine = T.stripEnd firstLine -- Remove trailing whitespace
  return $
    if T.null continuationLines
      then trimmedFirstLine
      else trimmedFirstLine <> "\n" <> continuationLines

parseContinuationCodeBlockLines :: Parser Text
parseContinuationCodeBlockLines = do
  token <- peekToken
  case token of
    Newline -> do
      _ <- consumeToken
      nextToken <- peekToken
      case nextToken of
        Indent indentSize -> do
          _ <- consumeToken
          colonToken <- peekToken
          case colonToken of
            Colon -> do
              _ <- consumeToken
              -- For code blocks, we need to preserve the exact spacing after the colon
              -- The continuation line format is: [indent]:[ spaces]content
              -- We need to preserve the spaces after the colon
              spacesAfterColon <- parseSpacesAfterColon
              text <- parseCodeBlockContent
              -- Calculate actual content indentation
              -- indentSize includes the colon position, so real content indent = indentSize - 1
              let actualIndent = if indentSize > 1 then T.replicate (indentSize - 1) " " else ""
              let fullLine = actualIndent <> spacesAfterColon <> text
              let trimmedLine = T.stripEnd fullLine -- Remove trailing whitespace only
              restText <- parseContinuationCodeBlockLines
              return $
                if T.null restText
                  then trimmedLine
                  else trimmedLine <> "\n" <> restText
            _ -> return ""
        _ -> return ""
    _ -> return ""

parseContinuationCodeLines :: Parser Text
parseContinuationCodeLines = do
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
              text <- parseTextContentWithColons
              restText <- parseContinuationCodeLines
              return $
                if T.null restText
                  then text
                  else text <> "\n" <> restText
            _ -> return ""
        _ -> return ""
    _ -> return ""

parseMultiLineText :: Parser Text
parseMultiLineText = do
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
              text <- parseTextContentWithColons
              rest <- parseMultiLineText
              return $
                if T.null rest
                  then text
                  else text <> "\n" <> rest
            _ -> return ""
        _ -> return ""
    EOF -> return ""
    _ -> return ""

-- Parser utility functions
peekToken :: Parser Token
peekToken = do
  parserState <- get
  case tokens parserState of
    [] -> return EOF
    (t : _) -> return t

consumeToken :: Parser Token
consumeToken = do
  parserState <- get
  case tokens parserState of
    [] -> return EOF
    (t : ts) -> do
      put parserState {tokens = ts}
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

-- Parse spaces after colon in code blocks to preserve indentation
parseSpacesAfterColon :: Parser Text
parseSpacesAfterColon = do
  token <- peekToken
  case token of
    Text source ->
      -- If the text starts with spaces, consume only the spaces part
      let spaces = T.takeWhile (== ' ') source
       in if T.null spaces
            then return ""
            else do
              -- Update tokens to consume only the spaces
              parserState <- get
              let remainingText = T.dropWhile (== ' ') source
              let newTokens =
                    if T.null remainingText
                      then tail (tokens parserState) -- Consume entire token
                      else Text remainingText : tail (tokens parserState) -- Leave non-space part
              put parserState {tokens = newTokens}
              return spaces
    _ -> return ""