{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Exception (assert)
import Eval
import Lambda

main :: IO ()
main =
  do
    let example_1 = [ex| (λf. λx . f x) (λx. x) z|]
    putStrLn $ pp example_1
    putStrLn $ pp $ call_by_value example_1
    putStrLn ""

    let var_replace = V "x"
    let replacement = [ex| y z|]
    let body = [ex| λy. x y|]
    putStrLn $ "[" ++ vs var_replace ++ " -> " ++ (pp replacement) ++ "](" ++ pp body ++ "]"
    putStrLn $ pp (subst body var_replace replacement)
    putStrLn ""

    let scc = [ex|λn. λs. λz. s (n s z)|]
    let plus = [ex|λm. λn. λs. λz. m s (n s z)|]

    let zero = [ex| λs. λz. z|]
    let one = full_beta [ex| $e:scc $e:zero |]
    let two = full_beta [ex| $e:scc $e:one |]
    let three = full_beta [ex| $e:scc $e:two |]

    print $ assert (one == [ex| λs. λz. s z |]) one
    print $ assert (three == full_beta [ex| $e:plus $e:one $e:two|]) three
