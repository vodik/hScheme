module Parser (readExpr, readExprList) where

import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric
import Complex
import Data.Array
import Control.Monad
import Control.Monad.Error
import Environment
import Util

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

escapedChars :: Parser Char
escapedChars = do char '\\'
                  x <- oneOf "\\\"nrt"
                  return $ case x of
                                '\\' -> x
                                '"'  -> x
                                'n'  -> '\n'
                                'r'  -> '\r'
                                't'  -> '\t'

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many $ escapedChars <|> noneOf "\""
                 char '"'
                 return $ String x

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many $ letter <|> digit <|> symbol
               return $ Atom $ first : rest

parseRatio :: Parser LispVal
parseRatio = do x <- many1 digit
                char '/'
                y <- many1 digit
                return $ Ratio (read x) (read y)

parseReal :: Parser LispVal
parseReal = do x <- many1 digit
               char '.'
               y <- many1 digit
               return $ Real $ fst . head $ readFloat $ x ++ "." ++ y

parseComplex :: Parser LispVal
parseComplex = do x <- (try parseReal <|> parseNumber)
                  char '+'
                  y <- (try parseReal <|> parseNumber)
                  char 'i'
                  return $ Complex $ toDouble x :+ toDouble y

parseDigital :: Parser LispVal
parseDigital = many1 digit >>= return . Number . read

parseDigital' :: Parser LispVal
parseDigital' = try $ string "#d" >> many1 digit >>= return . Number . read

parseHex :: Parser LispVal
parseHex = try $ string "#x" >> many1 hexDigit >>= return . Number . hex2dig

parseOct :: Parser LispVal
parseOct = try $ string "#o" >> many1 octDigit >>= return . Number . oct2dig

parseBin :: Parser LispVal
parseBin = try $ string "#b" >> many1 (oneOf "10") >>= return . Number . bin2dig

parseNumber :: Parser LispVal
parseNumber = (parseDigital <|> parseDigital' <|> parseHex <|> parseOct <|> parseBin) >>= return

parseBool :: Parser LispVal
parseBool = do string "#"
               x <- oneOf "tf"
               return $ case x of
                        't' -> Bool True
                        'f' -> Bool False

parseCharacter :: Parser LispVal
parseCharacter = do try $ string "#\\"
                    value <- try (string "newline" <|> string "space") <|> readChar
                    return $ Character $ case value of
                                              "space"   -> ' '
                                              "newline" -> '\n'
                                              otherwise -> value !! 0
                 where readChar = do x <- anyChar
                                     notFollowedBy alphaNum
                                     return [x]

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do head <- endBy parseExpr spaces
                     tail <- char '.' >> spaces >> parseExpr
                     return $ DottedList head tail

makeList :: String -> LispVal -> LispVal
makeList atom x = List [Atom atom, x]

parseQuoted :: Parser LispVal
parseQuoted = char '\'' >> parseExpr >>= return . makeList "quote"

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = char '`' >> parseExpr >>= return . makeList "quote"

parseUnQuoted :: Parser LispVal
parseUnQuoted = char ',' >> parseExpr >>= return . makeList "quote"

parseVector :: Parser LispVal
parseVector = do array <- sepBy parseExpr spaces
                 return $ Vector (listArray (0,(length array - 1)) array)

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> try parseComplex
        <|> try parseRatio
        <|> try parseReal
        <|> try parseNumber
        <|> try parseBool
        <|> try parseCharacter
        <|> parseQuasiQuoted
        <|> parseUnQuoted
        <|> parseQuoted
        <|> try (do string "#("
                    x <- parseVector
                    char ')'
                    return x)
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

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow $ endBy parseExpr spaces
