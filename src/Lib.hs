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

newtype ColumnName = Column String deriving Show
newtype TableName = TableName String deriving Show

data Select = Select [ColumnName] TableName deriving Show

reservedKeyWords :: Set.Set String
reservedKeyWords = Set.fromList ["FROM"]

pColumnName :: Parser ColumnName
pColumnName = try $ do
    colName <- some letterChar
    sc
    if colName `Set.member` reservedKeyWords
        then
            fail $ "Reserved keyword: " <> colName
        else
            return $ Column colName

pTableName :: Parser TableName
pTableName = TableName <$> lexeme (some letterChar)
    

pBigQuerySQL :: Parser Select
pBigQuerySQL = do
    sc
    symbol' "SELECT"
    columns <- pColumnName `sepEndBy1` symbol ","
    symbol' "FROM"    
    Select columns <$> pTableName

f = parseTest pBigQuerySQL "SELECT aa, bb, FROM morjesta"
