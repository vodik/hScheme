module Primitives.Basic (primitives) where

import Control.Monad
import Control.Monad.Error
import Environment
import Unpacking

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+",         numericBinop  (+)),
              ("-",         numericBinop  (-)),
              ("*",         numericBinop  (*)),
              ("/",         numericBinop  div),
              ("mod",       numericBinop  mod),
              ("quotient",  numericBinop  quot),
              ("remaider",  numericBinop  rem),

              ("symbol?",   unaryOp       isSymbol),
              ("string?",   unaryOp       isString),
              ("number?",   unaryOp       isNumber),
              ("bool?",     unaryOp       isBool),
              ("list?",     unaryOp       isList),

              ("symbol->string", unaryOp  stringFromSymbol),
              ("string->symbol", unaryOp  symbolFromString),

              ("=",         numBoolBinop  (==)),
              ("<",         numBoolBinop  (<)),
              (">",         numBoolBinop  (>)),
              ("/=",        numBoolBinop  (/=)),
              (">=",        numBoolBinop  (>=)),
              ("<=",        numBoolBinop  (<=)),

              ("&&",        boolBoolBinop (&&)),
              ("||",        boolBoolBinop (||)),

              ("string=?",  strBoolBinop  (==)),
              ("string<?",  strBoolBinop  (<)),
              ("string>?",  strBoolBinop  (>)),
              ("string<=?", strBoolBinop  (<=)),
              ("string>=?", strBoolBinop  (>=))]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp op [arg]  = return $ op arg
unaryOp _ badArgs = throwError $ NumArgs 2 badArgs

isSymbol :: LispVal -> LispVal
isSymbol (Atom _) = Bool True
isSymbol _        = Bool False

isString :: LispVal -> LispVal
isString (String _) = Bool True
isString _          = Bool False

isNumber :: LispVal -> LispVal
isNumber (Number _) = Bool True
isNumber _          = Bool False

isBool :: LispVal -> LispVal
isBool (Bool _) = Bool True
isBool _        = Bool False

isList :: LispVal -> LispVal
isList (List _)         = Bool True
isList (DottedList _ _) = Bool True
isList _                = Bool False

symbolFromString :: LispVal -> LispVal
symbolFromString (String s) = Atom s
symbolFromString _          = Atom ""

stringFromSymbol :: LispVal -> LispVal
stringFromSymbol (Atom s) = String s
stringFromSymbol _        = String ""

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op [l, r] = do left <- unpacker l
                                  right <- unpacker r
                                  return $ Bool $ left `op` right
boolBinop _ _ badArgs        = throwError $ NumArgs 2 badArgs

numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinop = boolBinop unpackNum

strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop = boolBinop unpackStr

boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinop = boolBinop unpackBool

