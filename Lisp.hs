module Lisp (runOne, runRepl) where

import IO
import Control.Monad
import Environment
import Eval
import Repl
import Primitives.Basic
import Primitives.List
import Primitives.IO

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc PrimitiveFunc) (primitives ++ listPrimitives)
                                              ++ map (makeFunc IOFunc) ioPrimitives)
                    where makeFunc constructor (var, func) = (var, constructor func)

runOne :: [String] -> IO ()
runOne args = do
    env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
    (runIOThrows $ liftM show $ eval env (List [Atom "load", String (args !! 0)])) >>= hPutStrLn stderr

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt ">>> ") . evalAndPrint
