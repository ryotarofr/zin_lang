{-# LANGUAGE OverloadedStrings #-}

module Zin.Lexer
  ( Token (..),
    IndentedLine (..),
    lexer,
    tokenize,
  )
where

import Control.Monad.State
import Data.Text (Text)
import qualified Data.Text as T

data Token
  = TopLevelTag Text
  | StyleTag Text [Int]
  | LangTag Text
  | Colon
  | Pipe
  | Text Text
  | Newline
  | Indent Int
  | URL Text
  | Comment Text
  | EOF
  deriving (Show, Eq)

data IndentedLine = IndentedLine
  { indentLevel :: Int,
    lineNumber :: Int,
    content :: Text,
    originalLine :: Text -- 元の行を保持（インデントを含む）
  }
  deriving (Show, Eq)

type LexerState = State (Int, [Token])

lexer :: Text -> [IndentedLine]
lexer input = zipWith processLine [1 ..] (T.lines input)
  where
    processLine lineNum line =
      IndentedLine
        { indentLevel = countIndent line,
          lineNumber = lineNum,
          content = T.stripStart line,
          originalLine = line
        }

countIndent :: Text -> Int
countIndent text = T.length $ T.takeWhile isSpaceOrTab text
  where
    isSpaceOrTab c = c == ' ' || c == '\t'

tokenize :: [IndentedLine] -> [Token]
tokenize inputLines = concatMap tokenizeLine inputLines ++ [EOF]
  where
    tokenizeLine line
      | T.null (content line) = [Newline]
      | T.isPrefixOf "//" (content line) = [Comment (T.drop 2 (content line)), Newline]
      | otherwise =
          -- For lines starting with ":", preserve spaces after the colon
          if T.isPrefixOf ":" (content line)
            then Indent (indentLevel line) : tokenizeCodeLine (originalLine line) ++ [Newline]
            else Indent (indentLevel line) : tokenizeContent (content line) ++ [Newline]

tokenizeContent :: Text -> [Token]
tokenizeContent text = evalState (tokenizeText text) (0, [])
  where
    tokenizeText :: Text -> LexerState [Token]
    tokenizeText input
      | T.null input = return []
      | otherwise = do
          (token, rest) <- nextToken input
          tokens <- tokenizeText rest
          return (token : tokens)

nextToken :: Text -> LexerState (Token, Text)
nextToken input
  | T.null input = return (EOF, "")
  | T.head input == ' ' = nextToken (T.dropWhile (== ' ') input)
  | isTopLevelTag input = parseTopLevelTag input
  | isStyleTag input = parseStyleTag input
  | isLangTag input = parseLangTag input
  | isURL input = parseURL input
  | T.head input == ':' && isStandaloneColon input = return (Colon, T.tail input)
  | T.head input == '|' = return (Pipe, T.tail input)
  | otherwise = parseText input
  where
    isStandaloneColon text =
      case T.uncons text of
        Just (':', rest) -> T.null rest || T.head rest == ' '
        _ -> False

isTopLevelTag :: Text -> Bool
isTopLevelTag input = any (isExactTopLevelTag input) topLevelTags
  where
    topLevelTags = ["p", "h1", "h2", "h3", "h4", "ul", "ol", "tl", "cb", "q", "t", "r"]
    isExactTopLevelTag text tag =
      T.isPrefixOf tag text
        && ( T.length text == T.length tag
               || ( T.length text > T.length tag
                      && let nextChar = T.index text (T.length tag)
                          in nextChar == ' ' || nextChar == ':'
                  )
           )

parseTopLevelTag :: Text -> LexerState (Token, Text)
parseTopLevelTag input = do
  let tag = T.takeWhile (\c -> c /= ' ' && c /= ':') input
  let rest = T.drop (T.length tag) input
  return (TopLevelTag tag, rest)

isStyleTag :: Text -> Bool
isStyleTag input = any (`T.isPrefixOf` input) styleTags
  where
    styleTags = ["bold[", "italic[", "strike[", "link["]

parseStyleTag :: Text -> LexerState (Token, Text)
parseStyleTag input = do
  let tagName = T.takeWhile (/= '[') input
  let afterTag = T.drop (T.length tagName + 1) input
  let rangeStr = T.takeWhile (/= ']') afterTag
  let rest = T.drop (T.length rangeStr + 1) afterTag
  case parseRange rangeStr of
    Just range -> return (StyleTag tagName range, rest)
    Nothing -> parseText input

parseRange :: Text -> Maybe [Int]
parseRange rangeStr =
  case T.splitOn ":" rangeStr of
    [start, end] -> do
      s <- readMaybeInt start
      e <- readMaybeInt end
      return [s, e]
    _ -> Nothing
  where
    readMaybeInt :: Text -> Maybe Int
    readMaybeInt t = case reads (T.unpack t) of
      [(n, "")] -> Just n
      _ -> Nothing

isURL :: Text -> Bool
isURL = T.isPrefixOf "url["

isLangTag :: Text -> Bool
isLangTag = T.isPrefixOf "lang["

parseLangTag :: Text -> LexerState (Token, Text)
parseLangTag input = do
  let afterLang = T.drop 5 input
  let langContent = T.takeWhile (/= ']') afterLang
  let rest = T.drop (T.length langContent + 1) afterLang
  return (LangTag langContent, rest)

parseURL :: Text -> LexerState (Token, Text)
parseURL input = do
  let afterUrl = T.drop 4 input
  let urlContent = T.takeWhile (/= ']') afterUrl
  let rest = T.drop (T.length urlContent + 1) afterUrl
  return (URL urlContent, rest)

parseText :: Text -> LexerState (Token, Text)
parseText input = do
  let textContent = T.takeWhile notSpecialChar input
  let rest = T.drop (T.length textContent) input
  return (Text textContent, rest)
  where
    notSpecialChar c = c /= '|' && c /= '\n'

-- Special tokenizer for code block continuation lines that preserves spaces after colon
tokenizeCodeLine :: Text -> [Token]
tokenizeCodeLine line =
  -- Find the position of the colon in the original line
  case T.findIndex (== ':') line of
    Just colonPos ->
      let afterColon = T.drop (colonPos + 1) line -- Everything after the colon, including spaces
      -- For continuation lines, preserve all spaces after colon
      -- But remove the single space that typically follows the colon on the first line
          processedContent =
            if T.isPrefixOf " " afterColon && not (T.isPrefixOf "  " afterColon)
              then T.drop 1 afterColon -- Remove single space after colon
              else afterColon -- Keep multiple spaces (actual indentation)
       in if T.null processedContent
            then [Colon]
            else [Colon, Text processedContent]
    Nothing -> tokenizeContent (T.stripStart line)