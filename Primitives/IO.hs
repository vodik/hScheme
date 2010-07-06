module Primitives.IO (ioPrimitives) where

import IO
import Control.Monad
import Control.Monad.Error
import Environment
import Eval
import Parser

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply",             applyProc),
                ("open-input-file",   makePort ReadMode),
                ("open-output-file",  makePort WriteMode),
                ("close-input-port",  closePort),
                ("close-output-port", closePort),
                ("read",              readProc),
                ("write",             writeProc),
                ("read-contents",     readContents),
                ("read-all",          readAll)]

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = do port <- liftIO $ try $ openFile filename mode
                                     case port of
                                          Left err  -> liftThrows $ throwError $ IO err
                                          Right val -> liftM Port $ liftIO $ return val

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _           = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc []          = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj]            = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = do body <- liftIO $ try $ readFile filename
                                    case body of
                                         Left err  -> liftThrows $ throwError $ IO err
                                         Right val -> liftM String $ liftIO $ return val

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename
