{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Used otherwise as a pattern" #-}
module Lib (someFunc, readExpr, parseExpr) where

import           Text.ParserCombinators.Parsec hiding (spaces)
import           Control.Monad

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err  -> "No match: " ++ show err
  Right val -> "Found value " ++ show val

someFunc :: IO ()
someFunc = putStrLn "someFunc"

spaces :: Parser ()
spaces = skipMany1 space

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
  deriving Show

parseString :: Parser LispVal
parseString = do
  _ <- char '"'
  x <- many (many (noneOf "\"") <|> string "\\\"")
  _ <- char '"'
  return $ String $ concat x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return
    $ case atom of
      "#t"      -> Bool True
      "#f"      -> Bool False
      otherwise -> Atom atom

parseNumber :: Parser LispVal
parseNumber = do
  digits <- many1 digit
  return $ (Number . read) digits

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseExpr :: Parser LispVal
parseExpr = parseAtom
  <|> parseString
  <|> parseNumber
  <|> do
    _ <- char '('
    x <- try parseList <|> parseDottedList
    _ <- char ')'
    return x