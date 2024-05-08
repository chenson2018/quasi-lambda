{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Eval where

import Lambda
import Text.Printf (printf)

-- pretty printer
-- all applications have explicit parens
-- one layer of special case for single variables
pp :: LExp -> String
pp [expr| $var:x |] = vs x
pp [expr| \ $var:x . $exp:e |] = printf "λ%s. %s" (vs x) (pp e)
pp [expr| $var:x1 $var:x2 |] = printf "%s %s" (vs x1) (vs x2)
pp [expr| $var:x $exp:e |] = printf "%s %s" (vs x) (pp e)
pp [expr| $exp:e $var:x |] = printf "%s %s" (pp e) (vs x)
pp [expr| $exp:e1 $exp:e2 |] = printf "%s %s" (pp e1) (pp e2)
pp _ = undefined
