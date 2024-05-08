{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Eval
import Lambda

main :: IO ()
main =
  do
    print [expr| x |]
    print [expr| \x . x |]
    print [expr| (\x . x) y |]
