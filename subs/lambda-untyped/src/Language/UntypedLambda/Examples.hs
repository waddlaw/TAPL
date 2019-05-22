{-# LANGUAGE OverloadedStrings #-}
module Language.UntypedLambda.Examples where

import Language.UntypedLambda
import Language.UntypedLambda.Lib.Base
import Language.UntypedLambda.Lib.Bool
import Language.UntypedLambda.Lib.Pair

-- | s t u
example1 :: UntypedLambda
example1 = TmApp (TmApp "s" "t") "u"

-- | λx. (λy. ((x y) x))
example2 :: UntypedLambda
example2 = TmLam "x" (TmLam "y" (TmApp (TmApp "x" "y") "x"))

-- | (λx.x) ((λx.x) (λz. (λx.x) z))
example3 :: UntypedLambda
example3 = TmApp id (TmApp id (TmLam "z" (TmApp id "z")))

-- | (λx.x) x
example4 :: UntypedLambda
example4 = TmApp (TmLam "x" "x") "x"

-- | λz. λx. λy. x (y z)
example5 :: UntypedLambda
example5 = TmLam "z" (TmLam "x" (TmLam "y" (TmApp "x" (TmApp "y" "z"))))

-- | (λx. x (λx. x)) (u r)
example6 :: UntypedLambda
example6 = TmApp (TmLam "x" (TmApp "x" (TmLam "x" "x"))) (TmApp "u" "r")

-- | test tru tru fls
example7 :: UntypedLambda
example7 = TmApp (TmApp (TmApp test tru) tru) fls

-- | and tru tru
example8 :: UntypedLambda
example8 = TmApp (TmApp and tru) tru

-- | and tru fls
example9 :: UntypedLambda
example9 = TmApp (TmApp and tru) fls

-- | fst (pair v w)
example10 :: UntypedLambda
example10 = TmApp fst (TmApp (TmApp pair "v") "w")

-- | [x]
example11 :: UntypedLambda
example11 = TmLam "c" (TmLam "n" (TmApp (TmApp "c" "x") "n"))
