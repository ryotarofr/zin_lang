{-# LANGUAGE OverloadedStrings #-}

module Zin.Error 
  ( ParseError(..)
  , ErrorInfo(..)
  , formatError
  , renderErrorWithSuggestion
  , createError
  ) where

import qualified Data.Text as T
import Data.Text (Text)
import Zin.Lexer (Token)

data ParseError
  = UnknownTag Text
  | InvalidStyleRange Text
  | MissingColon
  | InvalidTableFormat
  | UnexpectedToken Token
  | IndentationError
  | URLNotFound
  | StyleRangeOutOfBounds
  | EmptyInput
  | InvalidCodeBlockFormat
  | MissingTableHeader
  | InconsistentTableColumns
  deriving (Show, Eq)

data ErrorInfo = ErrorInfo
  { fileName :: Text
  , lineNumber :: Int
  , columnNumber :: Int
  , errorType :: ParseError
  , suggestion :: Maybe Text
  } deriving (Show, Eq)

formatError :: ErrorInfo -> Text
formatError err = T.concat
  [ fileName err
  , ":"
  , T.pack (show (lineNumber err))
  , ":"
  , T.pack (show (columnNumber err))
  , ": error: "
  , errorMessage (errorType err)
  , case suggestion err of
      Just sug -> "\n  suggestion: " <> sug
      Nothing -> ""
  ]

errorMessage :: ParseError -> Text
errorMessage (UnknownTag tag) = "Unknown tag '" <> tag <> "'"
errorMessage (InvalidStyleRange range) = "Invalid style range '" <> range <> "'"
errorMessage MissingColon = "Missing colon ':' after tag"
errorMessage InvalidTableFormat = "Invalid table format"
errorMessage (UnexpectedToken token) = "Unexpected token: " <> T.pack (show token)
errorMessage IndentationError = "Inconsistent indentation"
errorMessage URLNotFound = "URL not found for link style"
errorMessage StyleRangeOutOfBounds = "Style range is out of text bounds"
errorMessage EmptyInput = "Empty input file"
errorMessage InvalidCodeBlockFormat = "Invalid code block format"
errorMessage MissingTableHeader = "Table missing header row"
errorMessage InconsistentTableColumns = "Inconsistent number of table columns"

renderErrorWithSuggestion :: ParseError -> Maybe Text
renderErrorWithSuggestion (UnknownTag tag) = 
  Just $ "Use one of: p, h1, h2, h3, h4, ul, ol, tl, cb, q, t, r instead of '" <> tag <> "'"
renderErrorWithSuggestion (InvalidStyleRange _) = 
  Just "Use format: bold[0:5] or italic[2:8]"
renderErrorWithSuggestion MissingColon = 
  Just "Add ':' after the tag, e.g., 'p : text'"
renderErrorWithSuggestion InvalidTableFormat = 
  Just "Use format: t : | col1 | col2 | col3 |"
renderErrorWithSuggestion IndentationError = 
  Just "Ensure consistent indentation with spaces or tabs"
renderErrorWithSuggestion URLNotFound = 
  Just "Add url[https://example.com] after link style"
renderErrorWithSuggestion StyleRangeOutOfBounds = 
  Just "Ensure style range is within text length"
renderErrorWithSuggestion InvalidCodeBlockFormat = 
  Just "Use format: cb : code\\nlanguage : more code"
renderErrorWithSuggestion MissingTableHeader = 
  Just "Start table with header: t : | Header1 | Header2 |"
renderErrorWithSuggestion InconsistentTableColumns = 
  Just "Ensure all table rows have the same number of columns"
renderErrorWithSuggestion _ = Nothing

-- Helper function to create error info
createError :: Text -> Int -> Int -> ParseError -> ErrorInfo
createError file line col err = ErrorInfo
  { fileName = file
  , lineNumber = line
  , columnNumber = col
  , errorType = err
  , suggestion = renderErrorWithSuggestion err
  }