module Eval (apply, eval, load) where

import IO
import List
import Control.Monad
import Control.Monad.Error
import Environment
import Parser

import Primitives.List

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (IOFunc func) args        = func args
apply (Func params varargs body closure) args =
    if num params /= num args && varargs == Nothing
       then throwError $ NumArgs (num params) args
       else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
    where remainingArgs       = drop (length params) args
          num                 = toInteger . length
          evalBody env        = liftM last $ mapM (eval env) body
          bindVarArgs arg env = case arg of
                                      Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
                                      Nothing      -> return env

makeFunc varargs env params body = return $ Func (map show params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarargs = makeFunc . Just . show

eqv' (Bool arg1) (Bool arg2)     = arg1 == arg2
eqv' (Number arg1) (Number arg2) = arg1 == arg2
eqv' (String arg1) (String arg2) = arg1 == arg2
eqv' (Atom arg1) (Atom arg2)     = arg1 == arg2

eqvLst' :: [LispVal] -> LispVal -> Bool
eqvLst' (a:as) arg2 = if eqv' a arg2
                         then True
                         else eqvLst' as arg2
eqvLst' [] arg2 = False

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _)     = return val
eval env val@(Number _)     = return val
eval env val@(Ratio _ _)    = return val
eval env val@(Real _)       = return val
eval env val@(Complex _)    = return val
eval env val@(Bool _ )      = return val
eval env val@(Character _ ) = return val
eval env val@(Void)         = return val
eval env (Atom id)          = getVar env id

eval env (List [Atom "quote", val]) = return val

eval env (List [Atom "set!", Atom var, form]) =
    eval env form >>= setVar env var

eval env (List [Atom "define", Atom var, form]) =
    eval env form >>= defineVar env var

eval env (List [Atom "load", String filename]) =
    load filename >>= liftM last . mapM (eval env)

eval env (List (Atom "begin" : expr : rest)) = do
    eval' expr rest
    where eval' expr (x : xs) = eval env expr >> eval' x xs
          eval' expr []       = eval env expr >>= return

eval env (List (Atom "if" : expr : rest)) = do
    eval env expr >>= evalIf rest
    where evalIf [conseq, alt] (Bool result) = do
              case result of
                   True  -> eval env conseq
                   False -> eval env alt
          evalIf [conseq] (Bool result) = do
              case result of
                   True  -> eval env conseq
                   False -> return Void
          evalIf _ notBool = throwError $ TypeMismatch "boolean" notBool

eval env (List (Atom "cond" : expr : rest)) =
    eval' expr rest
    where eval' (List [cond, value]) (x : xs) = do
              result <- eval env cond
              case result of
                   Bool False -> eval' x xs
                   Bool True  -> eval env value
                   otherwise  -> throwError $ TypeMismatch "boolean" cond
          eval' (List [Atom "else", value]) [] = do
               eval env value
          eval' (List [cond, value]) [] = do
              result <- eval env cond
              case result of
                   Bool True  -> eval env value
                   otherwise  -> throwError $ TypeMismatch "boolean" cond

eval env (List (Atom "case" : key : clause : rest)) = do
    k <- eval env key
    eval' k clause rest
    where eval' k (List [(List cond), value]) (x : xs) = do
              if eqvLst' cond k
                 then eval env value
                 else eval' k x xs
          eval' _ (List [Atom "else", value]) [] = do
               eval env value
          eval' k (List [(List cond), value]) [] = do
              if eqvLst' cond k
                 then eval env value
                 else throwError $ TypeMismatch "boolean" value

eval env (List (Atom "define" : List (Atom var : params) : body)) =
    makeNormalFunc env params body >>= defineVar env var

eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
    makeVarargs varargs env params body >>= defineVar env var

eval env (List (Atom "lambda" : List params : body)) =
    makeNormalFunc env params body

eval env (List (Atom "lambda" : DottedList params varargs : body)) =
    makeVarargs varargs env params body

eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
    makeVarargs varargs env [] body

eval env (List (function : args)) = do
    func <- eval env function
    argVals <- mapM (eval env) args
    apply func argVals

eval _ badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

load :: String -> IOThrowsError [LispVal]
load filename = do body <- liftIO $ try $ readFile filename
                   case body of
                        Left err  -> liftThrows $ throwError $ IO err
                        Right val -> liftThrows $ readExprList val

