module Parser (readExpr, readExprList, load) where

import Text.ParserCombinators.Parsec hiding (spaces)
import qualified IO
import Directory
import Control.Monad
import Control.Monad.Error
import Environment

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseString, parseAtom, parseNumber :: Parser LispVal

parseString = do char '"'
                 x <- many $ noneOf "\""
                 char '"'
                 return $ String x


parseAtom = do first <- letter <|> symbol
               rest <- many $ letter <|> digit <|> symbol
               let atom = first : rest
               return $ case atom of
                             "#t" -> Bool True
                             "#f" -> Bool False
                             _    -> Atom atom

parseNumber = liftM (Number . read) $ many1 digit

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do head <- endBy parseExpr spaces
                     tail <- char '.' >> spaces >> parseExpr
                     return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do char '\''
                 x <- parseExpr
                 return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseQuoted
        <|> do char '('
               x <- try parseList <|> parseDottedList
               char ')'
               return x

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
                                Left err  -> throwError $ Parser err
                                Right val -> return val


readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr

load :: String -> IOThrowsError [LispVal]
load filename = do body <- liftIO $ IO.try $ readFile filename
                   case body of
                        Left err  -> liftThrows $ throwError $ IO err
                        Right val -> liftThrows $ readExprList val

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow $ endBy parseExpr spaces
