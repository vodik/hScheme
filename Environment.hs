module Environment where

import IO
import Data.Array
import Complex
import Text.ParserCombinators.Parsec
import Data.IORef
import Control.Monad.Error

data LispVal = Atom String
             | List [LispVal]
             | Vector (Array Int LispVal)
             | DottedList [LispVal] LispVal
             | Number Integer
             | Ratio Integer Integer
             | Real Double
             | Complex (Complex Double)
             | String String
             | Bool Bool
             | Character Char
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Port Handle
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Func {params  :: [String],
                     varargs :: (Maybe String),
                     body    :: [LispVal],
                     closure :: Env}
             | Void

instance Show LispVal where show = showLispVal

unwordsLispList :: [LispVal] -> String
unwordsLispList = unwords . map showLispVal

showLispVal :: LispVal -> String
showLispVal (String contents)      = "\"" ++ contents ++ "\""
showLispVal (Atom name)            = name
showLispVal (Number value)         = show value
showLispVal (Ratio num dem)        = (show num) ++ "/" ++ (show dem)
showLispVal (Real value)           = show value
showLispVal (Complex cmpx)         = show cmpx
showLispVal (Bool True)            = "#t"
showLispVal (Bool False)           = "#f"
showLispVal (Character char)       = [char]
showLispVal (List contents)        = "(" ++ unwordsLispList contents ++ ")"
showLispVal (DottedList head tail) = "(" ++ unwordsLispList head ++ " . " ++ showLispVal tail ++ ")"
showLispVal (PrimitiveFunc _)      = "#<primitive:" ++ n ++ ">"
                                     where n = ""
showLispVal (Port _)               = "#<IO port>"
showLispVal (IOFunc _)             = "#<IO primitive>"
showLispVal (Func params varargs _ _) =
    "(lambda (" ++ unwords (map show params) ++
    (case varargs of
          Nothing     -> ""
          Just params -> " . " ++ params) ++ ") ...)"
showLispVal (Void)                 = "#<void>"

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | IO IOError
               | Default String

instance Show LispError where show = showLispError

instance Error LispError where
    noMsg  = Default "An error has occured"
    strMsg = Default

type ThrowsError = Either LispError

showLispError :: LispError -> String
showLispError (UnboundVar msg var)      = msg ++ ": " ++ var
showLispError (BadSpecialForm msg form) = msg ++ ": " ++ show form
showLispError (NotFunction msg func)    = msg ++ ": " ++ show func
showLispError (NumArgs expt fnd)        = "Expected " ++ show expt ++ " args; " ++ found
                                          where found | length fnd == 0 = "none found"
                                                      | otherwise       = "found values " ++ unwordsLispList fnd
showLispError (TypeMismatch expt fnd)   = "Invalid type: expected " ++ expt ++ ", found " ++ show fnd
showLispError (Parser err)              = "Parse error at " ++ show err
showLispError (IO err)                  = "IO error at " ++ show err

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

type Env = IORef [(String, IORef LispVal)]
type IOThrowsError = ErrorT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err)  = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

runIOThrows_ :: IOThrowsError LispVal -> IO ()
runIOThrows_ action = runErrorT action >> return ()

nullEnv :: IO Env
nullEnv = newIORef []

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do env <- liftIO $ readIORef envRef
                       maybe (throwError $ UnboundVar "Getting an unbound variable" var)
                             (liftIO . readIORef)
                             (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwError $ UnboundVar "Setting an unbound variable" var)
                                   (liftIO . (flip writeIORef value))
                                   (lookup var env)
                             return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
    alreadyDefined <- liftIO $ isBound envRef var
    if alreadyDefined
       then setVar envRef var value >> return value
       else liftIO $ do valueRef <- newIORef value
                        env <- readIORef envRef
                        writeIORef envRef ((var, valueRef):env)
                        return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings =
    readIORef envRef >>= extendEnv bindings >>= newIORef
    where extendEnv bindings env  = liftM (++ env) (mapM addBinding bindings)
          addBinding (var, value) = do ref <- newIORef value
                                       return (var, ref)
