{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Lambda

-- TODO this only works for constants right now, not patterns

ex :: LExp
ex = [expr| \ x. x |]

ex2 :: LExp
ex2 = [expr| (\ x . x ) (\y.y) |]

-- TODO fails with a single variable or immediate applications
-- ex3 :: LExp
-- ex3 = [expr| a |]

y :: LExp
y = [expr| \f . (\x. f (x x)) (\x. f (x x)) |]

main :: IO ()
main = 
  do
    putStrLn $ pp ex
    putStrLn $ pp ex2 
    putStrLn $ pp y
    
    -- works fine with parser, but not QQ
    -- maybe a problem with free variables???
    -- e <- parseIO (topLevel pexp) "x"
    -- putStrLn $ pp e
    -- e <- parseIO (topLevel pexp) "a b c"
    -- putStrLn $ pp e

