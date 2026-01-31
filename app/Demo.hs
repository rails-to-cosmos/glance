{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Org (orgParseM, display)

main :: IO ()
main = do
    args <- getArgs
    case args of
        (inputString:_) -> do
            let txt = T.pack inputString
            TIO.putStrLn $ display $ head $ fst $ orgParseM txt

        [] -> putStrLn "Please provide an Org-mode string as an argument"
