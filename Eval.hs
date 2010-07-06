module Eval (apply, eval, load) where

import IO
import Control.Monad
import Control.Monad.Error
import Environment
import Parser

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

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _)     = return val
eval env val@(Number _)     = return val
eval env val@(Bool _ )      = return val
eval env val@(Character _ ) = return val
eval env (Atom id)          = getVar env id

eval env (List [Atom "quote", val]) = return val

eval env (List [Atom "if", pred, conseq, alt]) = do
    result <- eval env pred
    case result of
         Bool False -> eval env alt
         Bool True  -> eval env conseq
         otherwise  -> throwError $ TypeMismatch "boolean" pred

eval env (List [Atom "set!", Atom var, form]) =
    eval env form >>= setVar env var

eval env (List [Atom "define", Atom var, form]) =
    eval env form >>= defineVar env var

eval env (List [Atom "load", String filename]) =
    load filename >>= liftM last . mapM (eval env)

eval env (List (Atom "cond" : expr : rest)) = do
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

