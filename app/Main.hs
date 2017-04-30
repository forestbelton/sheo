module Main where

import System.Environment

import Language.Sheo.Parser
import Language.Sheo.Printer

compile :: String -> IO ()
compile fileName = do prgm <- parse fileName
                      case prgm of
                          Just p' -> print $ prettyPrint p'
                          Nothing -> return ()

usage :: IO ()
usage = print "pass a filename ya dingus"

main :: IO ()
main = do args <- getArgs
          case args of
              [fileName] -> compile fileName;
               _         -> usage