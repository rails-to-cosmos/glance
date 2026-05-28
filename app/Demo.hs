{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Org (orgParse, display)

main :: IO ()
main = do
    args <- getArgs
    case args of
        (inputString:_) -> do
            let txt = T.pack inputString
            let (elems, _ctx, _err) = orgParse mempty txt
            case elems of
              (x:_) -> TIO.putStrLn $ display x
              []    -> putStrLn "No elements parsed"

        [] -> putStrLn "Please provide an Org-mode string as an argument"
