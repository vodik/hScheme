module Lisp (runOne, runRepl) where

import IO
import Control.Monad
import Environment
import Eval
import Repl
import Primitives.Basic
import Primitives.List
import Primitives.IO

--newtype Scheme a = Scheme (String -> [(a, String)])

--instance Monad Scheme where
    --return a = Scheme 
    --s >>= f  = 

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc PrimitiveFunc) (primitives ++ listPrimitives)
                                              ++ map (makeFunc IOFunc) ioPrimitives)
                    where makeFunc constructor (var, func) = (var, constructor func)

runOne :: [String] -> IO ()
runOne args = do
    env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
    (runIOThrows $ liftM show $ eval env (List [Atom "load", String (args !! 0)])) >>= hPutStrLn stderr

runRepl :: IO ()
runRepl = do
    env <- primitiveBindings
    runIOThrows_ $ eval env (List [Atom "load", String "stdlib.scm"])
    until_ (== "quit") (readPrompt ">>> ") (evalAndPrint env)
