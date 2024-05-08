{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Eval
import Lambda

main :: IO ()
main =
  do
    let example_1 = [ex| (\f. \x . f x) (\x. x) z|]
    putStrLn $ pp example_1
    putStrLn $ pp $ call_by_value example_1
    putStrLn ""

    let var_replace = V "x"
    let replacement = [ex| y z|]
    let body = [ex| \y. x y|]
    putStrLn $ "[" ++ vs var_replace ++ " -> " ++ (pp replacement) ++ "](" ++ pp body ++ "]"
    putStrLn $ pp (subst body var_replace replacement)
    putStrLn ""

    let scc = [ex|\n. \s. \z. s (n s z)|]
    let plus = [ex|\m. \n. \s. \z. m s (n s z)|]

    let zero = [ex| \s. \z. z|]
    let one = full_beta [ex| $e:scc $e:zero |]
    let two = full_beta [ex| $e:scc $e:one |]
    let three = full_beta [ex| $e:scc $e:two |]

    putStrLn $ pp $ one
    putStrLn $ pp $ [ex| \s. \z. s z |]
    putStrLn ""

    putStrLn $ pp $ full_beta [ex| $e:plus $e:one $e:two|]
    putStrLn $ pp $ three
    putStrLn ""
