{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Exception (assert)
import Eval
import Lambda

main :: IO ()
main =
  do
    let scc = [ex|λn. λs. λz. s (n s z)|]
    let plus = [ex|λm. λn. λs. λz. m s (n s z)|]

    let zero = [ex| λs. λz. z|]
    let one = full_beta [ex| $e:scc $e:zero |]
    let two = full_beta [ex| $e:scc $e:one |]
    let three = full_beta [ex| $e:scc $e:two |]
    
    print $ assert (one == [ex| λs. λz. s z |]) one
    print $ assert (three == full_beta [ex| $e:plus $e:one $e:two|]) three    
