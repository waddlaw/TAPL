{-# LANGUAGE ViewPatterns #-}
module KnuthBendix where

import           Data.Function     (on)
import           Data.List
import           KnuthBendix.Types
import           KnuthBendix.Utils

kbc :: AlgorithmStatus -> AlgorithmStatus
kbc (kb -> result@(CanProceed _ _)) = kbc result
kbc result                          = result

kb :: AlgorithmStatus -> AlgorithmStatus
kb (CanProceed currentAxioms@(axiom:restAxioms) currentRules)
  | rel == EQ && not (checkLexEq axiomLhs axiomRhs) = FailedOn normalisedAxiom currentAxioms currentRules
  | rel /= EQ && notElem rule currentRules = CanProceed (superposeRules rule restAxioms newRules) newRules
  | null restAxioms = Finished currentRules
  | otherwise = CanProceed restAxioms currentRules
  where
    rel = compare axiomLhs axiomRhs
    normalisedAxiom = normaliseAxiom axiom currentRules
    axiomLhs = lhs normalisedAxiom
    axiomRhs = rhs normalisedAxiom
    rule = renameVarsInReductionRuleWithPrefix "" (orderAxiom normalisedAxiom)
    newRules = makeNewRules rule currentRules

makeNewRules :: ReductionRule -> [ReductionRule] -> [ReductionRule]
makeNewRules rule' = reverse . (rule':) . foldl' go []
  where
    go acc = maybe acc (addRule acc) . reduceRule rule'

addRule :: [ReductionRule] -> ReductionRule -> [ReductionRule]
addRule acc rule'
  | rule' `elem` acc = acc
  | otherwise        = rule':acc

reduceRule :: ReductionRule -> ReductionRule -> Maybe ReductionRule
reduceRule reductingRule (ReductionRule ruleTerm resultTerm)
  | rel == EQ = Nothing
  | rel == LT = Just $ ReductionRule reducedResult reducedRule
  | otherwise = Just $ ReductionRule reducedRule   reducedResult
  where
    rel = compare reducedRule reducedResult
    reducedRule   = reduceTerm reductingRule ruleTerm
    reducedResult = reduceTerm reductingRule resultTerm

normaliseAxiom :: Axiom -> [ReductionRule] -> Axiom
normaliseAxiom (Axiom termA termB) rs = toAxiom termA termB
  where
    toAxiom = Axiom `on` (reduceToNormalised rs)

reduceToNormalised :: [ReductionRule] -> Term -> Term
reduceToNormalised rules = go
  where
    go :: Term -> Term
    go term
      | compare result term == EQ = term
      | otherwise = go result
      where
        result = reduce term rules

reduce :: Term -> [ReductionRule] -> Term
reduce term [] = term
reduce term (rule:rest) =
    if compare result term == EQ
    then reduce term rest
    else result
  where
    result      = reduceTerm renamedRule term
    renamedRule = renameVarsInReductionRuleWithPrefix "r" rule

orderAxiom :: Axiom -> ReductionRule
orderAxiom (Axiom lhs rhs)
  | compare lhs rhs == GT = ReductionRule lhs rhs
  | otherwise = ReductionRule rhs lhs

checkLexEq :: Term -> Term -> Bool
checkLexEq (Var x) (Var y) = x == y
checkLexEq (Func f argsF) (Func g argsG) = foldr ((&&) . uncurry checkLexEq) (f == g) $ zip argsF argsG

superposeRules :: ReductionRule -> [Axiom] -> [ReductionRule] -> [Axiom]
superposeRules rule axioms [] = axioms
superposeRules rule axioms (r:rules) = superposeRules rule newAxioms rules
  where
    newAxioms = findCriticalPair rule r axioms

findCriticalPair :: ReductionRule -> ReductionRule -> [Axiom] -> [Axiom]
findCriticalPair ruleA ruleB axioms = find ruleB ruleA $ find ruleA ruleB axioms
  where
    find :: ReductionRule -> ReductionRule -> [Axiom] -> [Axiom]
    find ruleA ruleB axioms
      | checkCriticalPair (rule renamedRuleA) (rule renamedRuleB) = addCriticalPair renamedRuleA renamedRuleB axioms
      | otherwise = axioms
      where
        renamedRuleA = renameVarsInReductionRuleWithPrefix "l" ruleA
        renamedRuleB = renameVarsInReductionRuleWithPrefix "r" ruleB

checkCriticalPair :: Term -> Term -> Bool
checkCriticalPair (Func nameA argsA) (Func nameB argsB)
  | compare (Func nameA argsA) (Func nameB argsB) == EQ = any (\a -> checkSuperposition a (Func nameB argsB)) argsA
  | checkSuperposition (Func nameA argsA) (Func nameB argsB) = True
  | otherwise = any (\a -> checkCriticalPair a (Func nameB argsB)) argsA
checkCriticalPair _ _ = False

checkSuperposition :: Term -> Term -> Bool
checkSuperposition (Func nameA argsA) (Func nameB argsB) =
    if checkStructure (Func nameA argsA) (Func nameB argsB)
      then checkBindedVars $ fixBindedVars (listBindedVars (ReductionRule {rule=Func nameA argsA,result=Func nameA argsA}) (Func nameB argsB))
      else False
    where
    checkStructure :: Term -> Term -> Bool
    checkStructure (Var aV) (Func bName bArgs) = True
    checkStructure (Var aV) (Var bV) = True
    checkStructure (Func aName (a:aArgs)) (Var bV) = True --False --True --propably! False -> r6, r8; True -> r13
    checkStructure (Func aName []) (Var bV) = True --propably!
    checkStructure (Func aName aArgs) (Func bName bArgs) =
      if aName == bName && length aArgs == length bArgs
        then all (uncurry checkStructure) (zip aArgs bArgs)
        else False
    checkBindedVars :: [(Term,Term)] -> Bool
    checkBindedVars [] = True
    checkBindedVars ((Var v,term):rest) =
      if checkBindedVar (Var v,term) rest
        then checkBindedVars rest
        else False
      where
      checkBindedVar :: (Term,Term) -> [(Term,Term)] -> Bool
      checkBindedVar (Var v,bindedTerm) [] = True
      checkBindedVar (Var v,bindedTerm) ((Var h,hTerm):rest) =
        if v == h
          then bindedTerm == hTerm && (checkBindedVar (Var v,bindedTerm) rest)
          else checkBindedVar (Var v,bindedTerm) rest
checkSuperposition _ _ = False

addCriticalPair :: ReductionRule -> ReductionRule -> [Axiom] -> [Axiom]
addCriticalPair ruleA ruleB axioms =
    add axioms (createCriticalPair ruleA ruleB)
    where
    add :: [Axiom] -> [Axiom] -> [Axiom]
    add axioms [] = axioms
    add axioms (newAxiom:newAxioms) =
      if not (elem newAxiom axioms)
        then add (axioms++[newAxiom]) newAxioms
        else add axioms newAxioms

createCriticalPair :: ReductionRule -> ReductionRule -> [Axiom]
createCriticalPair (ReductionRule {rule=ruleA,result=resultA}) (ReductionRule {rule=ruleB,result=resultB}) =
    createCritical (ReductionRule {rule=ruleA,result=resultA}) (ReductionRule {rule=ruleB,result=resultB}) (createCriticalTerm ruleA ruleB)
    where
    createCritical :: ReductionRule -> ReductionRule -> [Term] -> [Axiom]
    createCritical _ _ [] = []
    createCritical (ReductionRule {rule=ruleA,result=resultA}) (ReductionRule {rule=ruleB,result=resultB}) ((Func name args):rest) =
      if compare ruleA ruleB /= EQ
        then
          if (compare (Func rname rargs) reductionA /= EQ && compare (Func rname rargs) reductionB /= EQ )
            then (Axiom reductionA reductionB):createRest
            else createRest
        else
          if (compare (Func rname rargs) reductionA /= EQ && compare (Func rname rargs) reductionRecB /= EQ )
            then (Axiom reductionA reductionRecB):createRest
            else createRest
      where
      (Func rname rargs) = renameVars (Func name args)
      reductionA = reduceTerm (ReductionRule {rule=ruleA,result=resultA}) (Func rname rargs)
      reductionB = reduceTerm (ReductionRule {rule=ruleB,result=resultB}) (Func rname rargs)
      reductionRecB = Func rname (mapOnlyFirst (reduceTerm (ReductionRule {rule=ruleB,result=resultB})) rargs)
      mapOnlyFirst :: (Term -> Term) -> [Term] -> [Term]
      mapOnlyFirst f [] = []
      mapOnlyFirst f (a:args) =
        if compare a b == EQ
          then a:(mapOnlyFirst f args)
          else b:args
        where b = f a
      createRest = createCritical (ReductionRule {rule=ruleA,result=resultA}) (ReductionRule {rule=ruleB,result=resultB}) rest

--better
createCriticalTerm :: Term -> Term -> [Term]
createCriticalTerm (Func nameA argsA) (Func nameB argsB) =
    create (Func nameA argsA) (Func nameB argsB) (Func nameA argsA)
    where
    create :: Term -> Term -> Term ->[Term]
    create (Func nameA argsA) (Func nameB argsB) result =
      if compare (Func nameA argsA) (Func nameB argsB) == EQ
        then
          superposeArgs argsA (Func nameB argsB) result
        else
          if checkSuperposition (Func nameA argsA) (Func nameB argsB)
            then
              (superpose (Func nameA argsA) (Func nameB argsB) result):(superposeArgs argsA (Func nameB argsB) result)
            else
              superposeArgs argsA (Func nameB argsB) result


--write different
superpose :: Term -> Term -> Term -> Term
superpose termA termB termResult =
    bindingAtoB (fixedBindingBtoA (fixedBindingAtoB termResult))
    where
    bindingAtoB :: Term -> Term
    bindingAtoB term = foldl (changeBinding) term (listBindedVars (ReductionRule {rule=termA,result=termA}) termB)
    bindingBtoA :: Term -> Term
    bindingBtoA term = foldl (changeBinding) term (listBindedVars (ReductionRule {rule=termB,result=termB}) termA)
    fixedBindingAtoB :: Term -> Term
    fixedBindingAtoB term = foldl (changeBinding) term (fix $ listBindedVars (ReductionRule {rule=termA,result=termA}) termB)
    fixedBindingBtoA :: Term -> Term
    fixedBindingBtoA term = foldl (changeBinding) term (fix $ listBindedVars (ReductionRule {rule=termB,result=termB}) termA)


fix :: [(Term,Term)] -> [(Term,Term)]
fix ((Var a,Var b):rest)          = fix rest
fix ((Var v,Func name args):rest) = (Var v,Func name args):(fix rest)
fix []                            = []

superposeArgs :: [Term] -> Term -> Term -> [Term]
superposeArgs ((Func nameA argsA):terms) termB termResult =
    if checkSuperposition (Func nameA argsA) termB
      then
        (superpose (Func nameA argsA) termB termResult):(superposeArgs terms termB termResult)
      else
        superposeArgs (terms++argsA) termB termResult
superposeArgs ((Var a):terms) termB termResult =
    superposeArgs terms termB termResult
superposeArgs [] termB termResult = []

--end better

fixBindedVars :: [(Term,Term)] -> [(Term,Term)]
fixBindedVars bindings =
   fix bindings bindings []
   where --change fix to work
   fix :: [(Term,Term)] -> [(Term,Term)] -> [(Term,Term)] -> [(Term,Term)]
   fix [] input result = result
   fix ((Var v,Func name args):rest) input result =
     fix rest input (result++[(Var v,Func name args)])
   fix ((Var v,Var r):rest) input result =
     if isBindedToFunc (Var v,Var r) input
       then fix rest input result
       else fix rest input (result++[(Var v,Var r)])
     where
     isBindedToFunc :: (Term,Term) -> [(Term,Term)] -> Bool
     isBindedToFunc (Var v,Var r) [] = False
     isBindedToFunc (Var v,Var r) ((Var b,Var a):rest) =
       isBindedToFunc (Var v,Var r) rest
     isBindedToFunc (Var v,Var r) ((Var b,Func name args):rest) =
       if v == b
         then True
         else isBindedToFunc (Var v,Var r) rest


reduceTerm :: ReductionRule -> Term -> Term
reduceTerm rr (Func name args) =
    if checkRuleInTerm (rr) (Func name args)
      then swapTerm (rr) (Func name args)
      else Func name (mapOnlyFirst (reduceTerm rr) args)
    where
    mapOnlyFirst :: (Term -> Term) -> [Term] -> [Term]
    mapOnlyFirst f [] = []
    mapOnlyFirst f (a:args) =
      if compare a b == EQ --a == b
        then a:(mapOnlyFirst f args)
        else b:args
      where b = f a
    swapTerm :: ReductionRule -> Term -> Term
    swapTerm rr term =
      foldl (changeBinding) (result rr) (listBindedVars rr term)
reduceTerm (rr) (Var v) = Var v

changeBinding :: Term -> (Term,Term) -> Term
changeBinding (Var t) (Var old, binded) = if (Var old) == (Var t) then binded else Var t
changeBinding (Func t args) (Var old, binded) = Func t (map (\x -> changeBinding x (Var old,binded)) args)


listBindedVars :: ReductionRule -> Term -> [(Term, Term)]
listBindedVars rr term =
    bindedVars rr term (findVarsInTerm (rule rr)) []
    where
    bindedVars :: ReductionRule -> Term -> [Term] -> [(Term, Term)] -> [(Term,Term)]
    bindedVars _ _ [] binded = binded
    bindedVars rr term ((Var v):vars) binded =
      bindedVars rr term vars (binded ++ (getBinding (Var v) (rule rr,term)))
      where
      getBinding :: Term -> (Term,Term) -> [(Term,Term)]
      getBinding (Var v) (Var a,term) =
        if v == a
          then [(Var v,term)]
          else []
      getBinding (Var v) (Func name args,Func termName termArgs) =
        concatMap (\(l,r) -> getBinding (Var v) (l,r)) list
        where list = zip args termArgs
      getBinding (Var v) (Func name args,Var a) =
        [(Var v,Var a)]

checkRuleInTerm :: ReductionRule -> Term -> Bool
checkRuleInTerm (ReductionRule {rule=Func ruleFuncName ruleFuncArgs,result=res}) (Func name args) =
    if checkStructure (Func ruleFuncName ruleFuncArgs) (Func name args)
      then checkBindedVars (listBindedVars (ReductionRule {rule=Func ruleFuncName ruleFuncArgs,result=res}) (Func name args) )
      else False
    where
    checkStructure :: Term -> Term -> Bool
    checkStructure (Var rv) _ = True
    checkStructure (Func rName rArgs) (Var v) = False
    checkStructure (Func rName rArgs) (Func name args) =
      if rName == name && length rArgs == length args
        then all (uncurry checkStructure) (zip rArgs args)
        else False
    checkBindedVars :: [(Term,Term)] -> Bool
    checkBindedVars [] = True
    checkBindedVars ((Var v,term):rest) =
      if checkBindedVar (Var v,term) rest
        then checkBindedVars rest
        else False
      where
      checkBindedVar :: (Term,Term) -> [(Term,Term)] -> Bool
      checkBindedVar (Var v,bindedTerm) [] = True
      checkBindedVar (Var v,bindedTerm) ((Var h,hTerm):rest) =
        if v == h
          then bindedTerm == hTerm && (checkBindedVar (Var v,bindedTerm) rest)
          else checkBindedVar (Var v,bindedTerm) rest
