{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
module Lib where

import           Control.Monad              (guard, void)
import           Data.Functor               (($>))
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import           Data.Text.IO               (readFile)
import           Data.Void
import           Prelude                    hiding (readFile)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.RawString.QQ



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

data Query = Query {
    selectColumns   :: [SelectColumn]
    , fromTable     :: FromStatement
    , whereClause   :: Maybe WhereClause
    , groupByClause :: Maybe GroupByClause
    , havingClause  :: Maybe HavingClause
 } deriving Show

data SelectColumn
    = Star
    | PlainColumnName { name :: String, alias :: Maybe String}
    | FunctionApplication { functionName :: String, argument :: [String], alias :: Maybe String}
    deriving Show

newtype FromStatement = FromStatement String deriving Show

newtype WhereClause = WhereClause { stmnt :: String } deriving Show

newtype GroupByClause = GroupByClause [String] deriving Show

newtype HavingClause = HavingClause { stmnt :: String } deriving Show



reservedKeyWords :: Set.Set String
reservedKeyWords = Set.fromList ["FROM"]

pStar = symbol "*" $> Star



pName :: Parser String
pName = do
    name <- some letterChar
    guard (not $ name `Set.member` reservedKeyWords)
    return name


pColumnName :: Parser SelectColumn
pColumnName = try $ PlainColumnName <$> lexeme pName <*> optional (symbol' "as" >> lexeme pName)

pFunction :: Parser SelectColumn
pFunction = try $ do
    functionName <- some letterChar
    argument <- lexeme $ between "(" ")" (some letterChar `sepEndBy1` symbol ",")
    alias <- optional (symbol' "as" >> lexeme pName)
    pure $ FunctionApplication functionName argument alias

pColumn :: Parser SelectColumn
pColumn
    = pStar
    <|> pFunction
    <|> pColumnName

pSelectList :: Parser [SelectColumn]
pSelectList = do
    symbol' "SELECT"
    pColumn `sepEndBy1` symbol ","

pFromClause :: Parser FromStatement
pFromClause = do
    symbol' "FROM"
    FromStatement <$> lexeme (some letterChar)

pWhereClause :: Parser (Maybe WhereClause)
pWhereClause = optional $ do
    symbol' "WHERE"
    WhereClause <$> lexeme (some printChar)

pGroupByClause :: Parser (Maybe GroupByClause)
pGroupByClause = optional $ do
    symbol' "GROUP BY"
    foo <- GroupByClause <$> (some letterChar `sepEndBy1` symbol ",")
    sc
    return foo

pHavingClause :: Parser (Maybe HavingClause)
pHavingClause = optional $ do
    symbol' "HAVING"
    HavingClause <$> lexeme (some printChar)



pQuery :: Parser Query
pQuery = sc >>
    Query
        <$> pSelectList
        <*> pFromClause
        <*> pWhereClause
        <*> pGroupByClause
        <*> pHavingClause


sql :: String
sql = [r|
SELECT
    *,
    aa as yeah,
    sum(moro) as yeahyeah,
FROM
    morjesta
WHERE
    b > 1
GROUP BY
    a, b
HAVING
    y
|]

f :: IO ()
f = parseTest pQuery sql
