{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Eval
import Lambda

main :: IO ()
main =
  do
    let example = [ex| (\f. \x . f x) (\x. x) z|]
    putStrLn $ pp $ eval example
