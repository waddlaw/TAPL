module KnuthBendix.Original where

import           KnuthBendix.Types (Axiom (..), Term (..))



maxLength :: Axiom -> Int
maxLength axiom = max ((getLength.lhs) axiom) ((getLength.rhs) axiom)

getLength :: Term -> Int
getLength = length.findVarsInTerm

findVarsInTerm :: Term -> [Term]
findVarsInTerm t = removeDuplicateVars (findVars t []) where
    findVars :: Term -> [Term] -> [Term]
    findVars (Var v) vars           = (Var v):vars
    findVars (Func f []) vars       = vars
    findVars (Func f (a:args)) vars = findVars (Func f args) (findVars a vars)
    removeDuplicateVars :: [Term] -> [Term]
    removeDuplicateVars vars = removeDuplicates vars [] where
        removeDuplicates :: [Term] -> [Term] -> [Term]
        removeDuplicates [] result = result
        removeDuplicates (v:vars) result = if elem v result
          then removeDuplicates vars result
          else removeDuplicates vars (v:result)
