{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Lambda

main :: IO ()
main = 
  do
    print [expr| x |]
    print [expr| \x . x |]
    print [expr| (\x . x) y |]
