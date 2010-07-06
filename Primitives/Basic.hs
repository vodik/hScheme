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

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right

numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinop = boolBinop unpackNum

strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop = boolBinop unpackStr

boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinop = boolBinop unpackBool

