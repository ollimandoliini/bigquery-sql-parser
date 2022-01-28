{-# LANGUAGE OverloadedStrings #-}
module Lib where

import           Control.Monad              (void)
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import           Data.Text.IO               (readFile)
import           Data.Void
import           Prelude                    hiding (readFile)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Functor (($>))


type Parser = Parsec Void String


sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "--")
  empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

symbol' :: String -> Parser String
symbol' = L.symbol' sc

data Column
    = Star
    | ColumnName {
    name :: String
    , alias :: Maybe String
} deriving Show


newtype TableName = TableName String deriving Show

data Select = Select [Column] TableName deriving Show

reservedKeyWords :: Set.Set String
reservedKeyWords = Set.fromList ["FROM"]


pColumn :: Parser Column
pColumn =
    symbol "*" $> Star
    <|> pColumnName

pName :: Parser String
pName = do
    name <- some letterChar
    if name `Set.member` reservedKeyWords
        then
            fail $ "Reserved keyword: " <> name
        else
            return name


pColumnName :: Parser Column
pColumnName = try $ do
    colName <- lexeme pName
    alias <- optional $ symbol' "as" >> lexeme pName
    return $ ColumnName colName alias

pTableName :: Parser TableName
pTableName = TableName <$> lexeme (some letterChar)
    

pBigQuerySQL :: Parser Select
pBigQuerySQL = do
    sc
    symbol' "SELECT"
    columns <- pColumn `sepEndBy1` symbol ","
    symbol' "FROM"    
    Select columns <$> pTableName

f = parseTest pBigQuerySQL "SELECT *, aa, bb FROM morjesta"
