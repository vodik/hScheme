module Main where

import Control.Monad
import System.Environment

import Lisp

main :: IO ()
main = do args <- getArgs
          if null args
             then runRepl
             else runOne $ args
