{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Eval where

import Data.List
import Lambda
import Text.Printf (printf)

free :: Exp -> [Var]
free [ex| $v:x |] = [x]
free [ex| \$v:v . $e:e|] = free e \\ [v]
free [ex| $e:e1 $e:e2|] = free e1 `union` free e2
free _ = undefined

occurs :: Exp -> [Var]
occurs [ex| $v:x |] = [x]
occurs [ex| \$v:v . $e:e|] = v : occurs e
occurs [ex| $e:e1 $e:e2|] = occurs e1 `union` occurs e2
occurs _ = undefined

allFresh :: [Var]
allFresh = map (V . ("fresh_" ++) . (show :: Integer -> String)) [1 ..]

subst :: Exp -> Var -> Exp -> Exp
subst e' x' y' = subst' (allFresh \\ occurs e' `union` occurs y') e' x' y'
  where
    subst' :: [Var] -> Exp -> Var -> Exp -> Exp
    subst' _ e@[ex| $v:v|] x y
      | v == x = y
      | otherwise = e
    subst' fresh e@[ex| \$v:v . $e:body|] x y
      | v == x = e
      -- TODO metavars here???
      | v `elem` free y = Lam v' (subst' fresh' body' x y)
      | otherwise = Lam v (subst' fresh body x y)
      where
        v' : fresh' = fresh
        body' = subst' (error "fresh variables not so fresh") body v (Var v')
    subst' fresh [ex| $e:e1 $e:e2|] x y = App (subst' fresh e1 x y) (subst' fresh e2 x y)
    subst' _ _ _ _ = undefined

eval :: Exp -> Exp
eval e@(Var _) = e
eval e@(Lam _ _) = e
eval (App e1 e2) =
  case eval e1 of
    Lam v body -> eval (subst body v e2)
    e' -> App e' (eval e2)
eval _ = undefined

-- pretty printer
-- all applications have explicit parens
-- one layer of special case for single variables
pp :: Exp -> String
pp [ex| $v:x |] = vs x
pp [ex| \$v:x . $e:e |] = printf "λ%s. %s" (vs x) (pp e)
pp [ex| $v:x1 $v:x2 |] = printf "%s %s" (vs x1) (vs x2)
pp [ex| $v:x $e:e |] = printf "%s (%s)" (vs x) (pp e)
pp [ex| $e:e $v:x |] = printf "(%s) %s" (pp e) (vs x)
pp [ex| $e:e1 $e:e2 |] = printf "(%s)(%s)" (pp e1) (pp e2)
pp _ = undefined
