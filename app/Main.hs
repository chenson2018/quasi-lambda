{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Exception (assert)
import Eval
import Lambda

toChurch :: Int -> Exp
toChurch 0 = [ex| λs. λz. z|]
toChurch n = full_beta [ex| $e:scc $e:prev |]
  where
    scc = [ex|λn. λs. λz. s (n s z)|]
    prev = toChurch $ n - 1

fromChurch :: Exp -> Maybe Int
fromChurch [ex| λs. λz. z|] = Just 0
fromChurch [ex| λs. λz. s $e:e|] = (+ 1) <$> fromChurch [ex| λs. λz. $e:e|]
fromChurch _ = Nothing

toChurch' :: Int -> Exp
toChurch' 0 = Lam (V "s") $ Lam (V "z") (Var $ V "z")
toChurch' n = full_beta $ App scc prev
  where
    scc = Lam (V "n") $ Lam (V "s") $ Lam (V "z") $ App (Var $ V "s") (App (App (Var $ V "n") (Var $ V "s")) (Var $ V "z"))
    prev = toChurch' $ n - 1

fromChurch' :: Exp -> Maybe Int
fromChurch' (Lam (V "s") (Lam (V "z") (Var (V "z")))) = Just 0
fromChurch' (Lam (V "s") (Lam (V "z") (App (Var (V "s")) e))) = (+ 1) <$> fromChurch' (Lam (V "s") (Lam (V "z") e))
fromChurch' _ = Nothing

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

    let i = 100
    print $ assert (Just i == (fromChurch . toChurch) i) (Just i)
    print $ assert (Just i == (fromChurch' . toChurch') i) (Just i)
