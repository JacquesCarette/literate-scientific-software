{-# LANGUAGE PostfixOperators #-}
module Drasil.SSP.IMods where

import Prelude hiding (tan, product, sin, cos)

import Language.Drasil
import Theory.Drasil
import Utils.Drasil
import Utils.Drasil.Concepts
import qualified Utils.Drasil.Sentence as S
import Drasil.DocLang.SRS (propCorSol)

-- Needed for derivations
import Data.Drasil.Concepts.Documentation (analysis, assumption, constraint,
  definition, first, goal, method_, physical, problem, solution, value)
import Data.Drasil.Concepts.Math (angle, equation, leftSide)
import Data.Drasil.Concepts.PhysicalProperties (mass)
import Data.Drasil.Concepts.Physics (force)

import Drasil.SSP.Assumptions (assumpSSC, assumpINSFL,
  assumpES, assumpSF, assumpSL)
import Drasil.SSP.BasicExprs (eqlExpr, eqlExprN, eqlExprSepG, eqlExprNSepG,
  eqlExprNoKQ, eqlExprNNoKQ, sliceExpr, momExpr, momExprNoKQ)
import Drasil.SSP.DataDefs (convertFunc1, convertFunc2,
  intersliceWtrF, lengthB, angleA, angleB, slcHeight, ratioVariation)
import Drasil.SSP.GenDefs (normShrRGD, momentEqlGD, normForcEqGD, mobShearWOGD,
  resShearWOGD, bsShrFEqGD, mobShrGD, srfWtrFGD)
import Drasil.SSP.Goals (goals, identifyCritAndFSGS)
import Drasil.SSP.Defs (crtSlpSrf, factorOfSafety, morPrice, slice,
  slpSrf, ssa)
import Drasil.SSP.References (chen2005, li2010, karchewski2012)
import Drasil.SSP.TMods (equilibrium, mcShrStrgth)
import Drasil.SSP.Unitals (baseAngle, baseHydroForce, baseLngth, baseWthX,
  critCoords,
  effCohesion, constF, dryWeight, earthqkLoadFctr, fricAngle, fs, fsMin,
  index, indx1, indxn, intNormForce, intShrForce, inxi, inxiM1, midpntHght,
  minFunction, mobShrC, mobShrI, nrmForceSum, nrmShearNum, normToShear,
  nrmFSubWat, numbSlices, satWeight, scalFunc, shearFNoIntsl, nrmShearDen,
  shearRNoIntsl, shrResC, slipDist, slipHght, slopeDist, slopeHght, sum1toN,
  surfAngle, surfHydroForce, surfLoad, totNrmForce, varblV, watrForce,
  waterDist, waterHght, waterWeight, watForceSum, xi, xMaxExtSlip, xMaxEtrSlip,
  xMinExtSlip, xMinEtrSlip, yi, yMaxSlip, yMinSlip)

-----------------------
--  Instance Models  --
-----------------------

iMods :: [InstanceModel]
iMods = [fctSfty, nrmShrFor, nrmShrForNum, nrmShrForDen, intsliceFs, crtSlpId]

--

fctSfty :: InstanceModel
fctSfty = im (equationalModel' fctSftyQD)
 [qwUC slopeDist, qwUC slopeHght, qwUC waterHght, qwUC effCohesion, qwUC fricAngle,
  qwUC dryWeight, qwUC satWeight, qwUC waterWeight, qwUC slipDist, qwUC slipHght, qwUC constF]
  (qw fs) [] (map ref [chen2005, karchewski2012])
  (Just fctSftyDeriv) "fctSfty" [fctSftyDesc]

fctSftyQD :: QDefinition 
fctSftyQD = mkQuantDef' fs factorOfSafety fctSftyExpr

fctSftyExpr :: Expr
fctSftyExpr = sumOp shearRNoIntsl $/ sumOp shearFNoIntsl
  where prodOp = defprod (eqSymb varblV) (sy index) (sy numbSlices $- int 1)
          (idx (sy mobShrC) (sy varblV))
        sumOp sym = defsum (eqSymb index) (int 1) (sy numbSlices $- int 1)
          (idx (sy sym) (sy index) `mulRe` prodOp) `addRe` idx (sy sym) (sy numbSlices)

fctSftyDesc :: Sentence
fctSftyDesc = foldlList Comma List [shearRNoIntsl `definedIn'''` resShearWOGD,
  mobShrC `definedIn'''` convertFunc2,
  shearFNoIntsl `definedIn'''` mobShearWOGD]

fctSftyDeriv :: Derivation
fctSftyDeriv = mkDerivNoHeader (weave [fctSftyDerivSentences1, map eS fctSftyDerivEqns1] ++
  map eS [fctSftyDerivEqn10b, fctSftyDerivEqn10c] ++ [fctSftyDerivEllipsis] ++
  map eS [fctSftyDerivEqn10d, fctSftyDerivEqn10e, fctSftyDerivEqn10f] ++
  weave [fctSftyDerivSentences2, map eS fctSftyDerivEqns2] ++
  fctSftyDerivSentence20)

fctSftyDerivSentences1 :: [Sentence]
fctSftyDerivSentences1 = map foldlSentCol [fctSftyDerivSentence1,
  fctSftyDerivSentence2, fctSftyDerivSentence3, fctSftyDerivSentence4,
  fctSftyDerivSentence5, fctSftyDerivSentence6, fctSftyDerivSentence7,
  fctSftyDerivSentence8, fctSftyDerivSentence9, fctSftyDerivSentence10]

fctSftyDerivSentences2 :: [Sentence]
fctSftyDerivSentences2 = map foldlSentCol [fctSftyDerivSentence11,
  fctSftyDerivSentence12, fctSftyDerivSentence13, fctSftyDerivSentence14,
  fctSftyDerivSentence15, fctSftyDerivSentence16, fctSftyDerivSentence17,
  fctSftyDerivSentence18, fctSftyDerivSentence19]

fctSftyDerivEqns1 :: [Expr]
fctSftyDerivEqns1 = [fctSftyDerivEqn1, fctSftyDerivEqn2, fctSftyDerivEqn3,
  fctSftyDerivEqn4, fctSftyDerivEqn5, fctSftyDerivEqn6, fctSftyDerivEqn7,
  fctSftyDerivEqn8, fctSftyDerivEqn9, fctSftyDerivEqn10a]

fctSftyDerivEqns2 :: [Expr]
fctSftyDerivEqns2 = [fctSftyDerivEqn11, fctSftyDerivEqn12, fctSftyDerivEqn13,
  fctSftyDerivEqn14, fctSftyDerivEqn15, fctSftyDerivEqn16, fctSftyDerivEqn17,
  fctSftyDerivEqn18, sy fs $= fctSftyExpr]

fctSftyDerivSentence1 :: [Sentence]
fctSftyDerivSentence1 = [atStartNP (the mobShrI), definedIn'' bsShrFEqGD, 
  S "can be substituted into the definition of",
  phrase mobShrI, S "based on the", phrase fs `sC` S "from",
  refS mobShrGD, S "yielding", eqN 1, S "below"]

fctSftyDerivSentence2 :: [Sentence]
fctSftyDerivSentence2 = [S "An expression for the", phrase nrmFSubWat `sC`
  ch nrmFSubWat `sC` S "can be derived by substituting the",
  phrase totNrmForce, S "equilibrium from", refS normForcEqGD,
  S "into the definition for", phrase nrmFSubWat, S "from" +:+. refS resShearWOGD, S "This results in", eqN 2]

fctSftyDerivSentence3 :: [Sentence]
fctSftyDerivSentence3 = [S "Substituting", eqN 2, S "into", eqN 1, S "gives"]

fctSftyDerivSentence4 :: [Sentence]
fctSftyDerivSentence4 = [S "Since the", phrase intShrForce,
  ch intShrForce `S.and_` phrase intNormForce, ch intNormForce,
  S "are unknown, they are separated from the other terms as follows"]

fctSftyDerivSentence5 :: [Sentence]
fctSftyDerivSentence5 = [S "Applying assumptions", refS assumpSF `S.and_`
  refS assumpSL `sC` S "which state that the",
  phraseNP (earthqkLoadFctr `andThe` surfLoad) `sC`
  S "respectively, are zero, allows for further simplification as shown below"]

fctSftyDerivSentence6 :: [Sentence]
fctSftyDerivSentence6 = [S "The definitions of", refS resShearWOGD
  `S.and_` refS mobShearWOGD, S "are present in this equation, and",
  S "thus can be replaced by", eS (inxi shearRNoIntsl) `S.and_`
  eS (inxi shearFNoIntsl) `sC` S "respectively"]

fctSftyDerivSentence7 :: [Sentence]
fctSftyDerivSentence7 = [atStartNP (the intShrForce), ch intShrForce,
  S "can be expressed in terms of the", phrase intNormForce,
  ch intNormForce, S "using", refS assumpINSFL `S.and_`
  refS normShrRGD `sC` S "resulting in"]

fctSftyDerivSentence8 :: [Sentence]
fctSftyDerivSentence8 = [S "Rearranging yields the following"]

fctSftyDerivSentence9 :: [Sentence]
fctSftyDerivSentence9 = [S "The definitions for" +:+ ch shrResC `S.and_`
  ch mobShrC +:+ S "from" +:+ refS convertFunc1 `S.and_`
  refS convertFunc2 +:+ S "simplify the above to", eqN 3]

fctSftyDerivSentence10 :: [Sentence]
fctSftyDerivSentence10 = [S "Versions of", eqN 3, S "instantiated for slices",
  S "1 to", ch numbSlices, S "are shown below"]

fctSftyDerivEllipsis :: Sentence
fctSftyDerivEllipsis = S "..."

fctSftyDerivSentence11 :: [Sentence]
fctSftyDerivSentence11 = [S "Applying", refS assumpES `sC`
  S "which says that", eS (idx (sy intNormForce) (int 0)) `S.and_`
  eS (indxn intNormForce), S "are zero, results in the following special cases:",eqN 8, S "for the first slice"]

fctSftyDerivSentence12 :: [Sentence]
fctSftyDerivSentence12 = [S "and", eqN 9, S "for the", ch numbSlices :+:
  S "th slice"]

fctSftyDerivSentence13 :: [Sentence]
fctSftyDerivSentence13 = [S "Substituting", eqN 8, S "into", eqN 4, S "yields", eqN 10]

fctSftyDerivSentence14 :: [Sentence]
fctSftyDerivSentence14 = [S "which can be substituted into", eqN 5, S "to get", eqN 11]

fctSftyDerivSentence15 :: [Sentence]
fctSftyDerivSentence15 = [S "and so on until", eqN 12, S "is obtained from", eqN 7]

fctSftyDerivSentence16 :: [Sentence]
fctSftyDerivSentence16 = [eqN 9, S "can then be substituted into the",
  phrase leftSide `S.of_` eqN 12 `sC` S "resulting in"]

fctSftyDerivSentence17 :: [Sentence]
fctSftyDerivSentence17 = [S "This can be rearranged by multiplying both sides",
  S "by", eS (idx (sy mobShrC) (sy numbSlices $- int 1)) `S.and_`
  S "then distributing the multiplication of each", ch mobShrC,
  S "over addition to obtain"]

fctSftyDerivSentence18 :: [Sentence]
fctSftyDerivSentence18 = [S "The multiplication of the", ch mobShrC,
  S "terms can be further distributed over the subtractions, resulting in the",
  S "equation having terms that each either contain an", ch shearRNoIntsl,
  S "or a" +:+. ch shearFNoIntsl, S "The equation can then be rearranged so",
  S "terms containing an", ch shearRNoIntsl, S "are on one side of the",
  S "equality, and terms containing a", ch shearFNoIntsl +:+.
  S "are on the other", S "The multiplication by the", phrase fs,
  S "is common to all of the", ch shearFNoIntsl, S "terms, and thus can be",
  S "factored out, resulting in"]

fctSftyDerivSentence19 :: [Sentence]
fctSftyDerivSentence19 = [S "Isolating the", phrase fs, S "on the left-hand",
  S "side and using compact notation for the products and sums yields",
  eqN 13 `sC` S "which can also be seen in", refS fctSfty]

fctSftyDerivSentence20 :: [Sentence]
fctSftyDerivSentence20 = [ch fs +:+ S "depends on the unknowns" +:+
  ch normToShear +:+ sParen (refS nrmShrFor) `S.and_` ch intNormForce +:+.
  sParen (refS intsliceFs)]

fctSftyDerivEqn1 :: Expr
fctSftyDerivEqn1 = --FIXME: pull the right side of this from GD4
  eqlExpr sin cos (\x y -> x $- inxiM1 intShrForce `addRe` inxi intShrForce `addRe` y)
  $= (inxi nrmFSubWat `mulRe` tan (sy fricAngle) `addRe` (sy effCohesion `mulRe`
  inxi baseLngth)) $/ sy fs

fctSftyDerivEqn2 :: Expr
fctSftyDerivEqn2 = inxi nrmFSubWat $= eqlExprN cos sin (\x y -> x $-
  inxiM1 intShrForce `addRe` inxi intShrForce `addRe` y) $- inxi baseHydroForce

fctSftyDerivEqn3 :: Expr
fctSftyDerivEqn3 = eqlExpr sin cos (\x y -> x $- inxiM1 intShrForce `addRe`
  inxi intShrForce `addRe` y) $= ((eqlExprN cos sin (\x y -> x $-
  inxiM1 intShrForce `addRe` inxi intShrForce `addRe` y) $- inxi baseHydroForce) `mulRe`
  tan (sy fricAngle) `addRe` (sy effCohesion `mulRe` inxi baseLngth)) $/ sy fs

fctSftyDerivEqn4 :: Expr
fctSftyDerivEqn4 = eqlExprSepG sin cos addRe `addRe`
  ((neg (inxiM1 intShrForce) `addRe` inxi intShrForce) `mulRe` sin (inxi baseAngle)) $=
  ((eqlExprNSepG cos sin addRe `addRe`
  (((neg (inxiM1 intShrForce) `addRe` inxi intShrForce) `mulRe` cos (inxi baseAngle)) $-
  inxi baseHydroForce) `mulRe`
  tan (sy fricAngle)) `addRe` (sy effCohesion `mulRe` inxi baseLngth)) $/ sy fs

fctSftyDerivEqn5 :: Expr
fctSftyDerivEqn5 = eqlExprNoKQ sin cos addRe `addRe`
  ((neg (inxiM1 intShrForce) `addRe` inxi intShrForce) `mulRe` sin (inxi baseAngle)) $=
  ((eqlExprNNoKQ cos sin addRe `addRe`
  ((neg (inxiM1 intShrForce) `addRe` inxi intShrForce) `mulRe` cos (inxi baseAngle)) $-
  inxi baseHydroForce) `mulRe`
  tan (sy fricAngle) `addRe` (sy effCohesion `mulRe` inxi baseLngth)) $/ sy fs

fctSftyDerivEqn6 :: Expr
fctSftyDerivEqn6 = (inxi shearFNoIntsl `addRe` ((neg (inxiM1 intShrForce) `addRe`
  inxi intShrForce) `mulRe` sin (inxi baseAngle)) $- ((neg (inxi intNormForce) `addRe`
  inxiM1 intNormForce) `mulRe` cos (inxi baseAngle))) $= (inxi shearRNoIntsl `addRe`
  (((neg (inxiM1 intShrForce) `addRe` inxi intShrForce) `mulRe` cos (inxi baseAngle) `addRe`
  ((neg (inxi intNormForce) `addRe` inxiM1 intNormForce) `mulRe` sin (inxi baseAngle))) `mulRe`
  tan (sy fricAngle))) $/ sy fs

fctSftyDerivEqn7 :: Expr
fctSftyDerivEqn7 = (inxi shearFNoIntsl `addRe` ((neg (sy normToShear) `mulRe` inxiM1 scalFunc `mulRe`
  inxiM1 intNormForce `addRe` (sy normToShear `mulRe` inxi scalFunc `mulRe` inxi intNormForce)) `mulRe`
  sin (inxi baseAngle)) $- ((neg (inxi intNormForce) `addRe` inxiM1 intNormForce) `mulRe` cos (inxi baseAngle)))
  $= (inxi shearRNoIntsl `addRe` (((neg (sy normToShear) `mulRe`
  inxiM1 scalFunc `mulRe` inxiM1 intNormForce `addRe` (sy normToShear `mulRe` inxi scalFunc `mulRe`
  inxi intNormForce)) `mulRe` cos (inxi baseAngle) `addRe` ((neg (inxi intNormForce) `addRe` inxiM1 intNormForce)
  `mulRe` sin (inxi baseAngle))) `mulRe` tan (sy fricAngle))) $/ sy fs

fctSftyDerivEqn8 :: Expr
fctSftyDerivEqn8 = (inxi intNormForce `mulRe` ((sy normToShear `mulRe` inxi scalFunc `mulRe`
  cos (inxi baseAngle) $- sin (inxi baseAngle)) `mulRe` tan (sy fricAngle) $-
  ((sy normToShear `mulRe` inxi scalFunc `mulRe` sin (inxi baseAngle) `addRe`
  cos (inxi baseAngle)) `mulRe` sy fs))) $= (inxiM1 intNormForce `mulRe`
  ((sy normToShear `mulRe` inxiM1 scalFunc `mulRe` cos (inxi baseAngle) $-
  sin (inxi baseAngle)) `mulRe` tan (sy fricAngle) $- ((sy normToShear `mulRe`
  inxiM1 scalFunc `mulRe` sin (inxi baseAngle) `addRe` cos (inxi baseAngle)) `mulRe`
  sy fs)) `addRe` (sy fs `mulRe` inxi shearFNoIntsl) $- inxi shearRNoIntsl)

fctSftyDerivEqn9 :: Expr
fctSftyDerivEqn9 = (inxi intNormForce `mulRe` inxi shrResC) $= (inxiM1 mobShrC `mulRe`
  inxiM1 intNormForce `mulRe` inxiM1 shrResC `addRe` (sy fs `mulRe` inxi shearFNoIntsl) $-
  inxi shearRNoIntsl)

fctSftyDerivEqn10a :: Expr
fctSftyDerivEqn10a = sliceExpr 1

fctSftyDerivEqn10b :: Expr
fctSftyDerivEqn10b = sliceExpr 2

fctSftyDerivEqn10c :: Expr
fctSftyDerivEqn10c = sliceExpr 3

fctSftyDerivEqn10d :: Expr
fctSftyDerivEqn10d = idx (sy intNormForce) (sy numbSlices $- int 2) `mulRe`
  idx (sy shrResC) (sy numbSlices $- int 2) $= (idx (sy mobShrC) (sy numbSlices $- int 3) `mulRe` idx (sy intNormForce) (sy numbSlices $- int 3) `mulRe`
  idx (sy shrResC) (sy numbSlices $- int 3) `addRe` (sy fs `mulRe`
  idx (sy shearFNoIntsl) (sy numbSlices $- int 2)) $-
  idx (sy shearRNoIntsl) (sy numbSlices $- int 2))

fctSftyDerivEqn10e :: Expr
fctSftyDerivEqn10e = idx (sy intNormForce) (sy numbSlices $- int 1) `mulRe`
  idx (sy shrResC) (sy numbSlices $- int 1) $= (idx (sy mobShrC) (sy numbSlices $- int 2) `mulRe` idx (sy intNormForce) (sy numbSlices $- int 2) `mulRe`
  idx (sy shrResC) (sy numbSlices $- int 2) `addRe` (sy fs `mulRe`
  idx (sy shearFNoIntsl) (sy numbSlices $- int 1)) $-
  idx (sy shearRNoIntsl) (sy numbSlices $- int 1))

fctSftyDerivEqn10f :: Expr
fctSftyDerivEqn10f = idx (sy intNormForce) (sy numbSlices) `mulRe`
  idx (sy shrResC) (sy numbSlices) $= (idx (sy mobShrC) (sy numbSlices $- int 1) `mulRe` idx (sy intNormForce) (sy numbSlices $- int 1) `mulRe`
  idx (sy shrResC) (sy numbSlices $- int 1) `addRe` (sy fs `mulRe`
  idx (sy shearFNoIntsl) (sy numbSlices)) $-
  idx (sy shearRNoIntsl) (sy numbSlices))

fctSftyDerivEqn11 :: Expr
fctSftyDerivEqn11 = (indx1 intNormForce `mulRe` indx1 shrResC) $=
  (sy fs `mulRe` indx1 shearFNoIntsl $- indx1 shearRNoIntsl)

fctSftyDerivEqn12 :: Expr
fctSftyDerivEqn12 = neg ((sy fs `mulRe` indxn shearFNoIntsl $- indxn shearRNoIntsl) $/
  idx (sy mobShrC) (sy numbSlices $- int 1)) $=
  (idx (sy intNormForce) (sy numbSlices $- int 1) `mulRe`
  idx (sy shrResC) (sy numbSlices $- int 1))

fctSftyDerivEqn13 :: Expr
fctSftyDerivEqn13 = idx (sy intNormForce) (int 2) `mulRe` idx (sy shrResC) (int 2) $=
  (idx (sy mobShrC) (int 1) `mulRe` (sy fs `mulRe` idx (sy shearFNoIntsl) (int 1) $-
  idx (sy shearRNoIntsl) (int 1)) `addRe` (sy fs `mulRe` idx (sy shearFNoIntsl) (int 2)) $-
  idx (sy shearRNoIntsl) (int 2))

fctSftyDerivEqn14 :: Expr
fctSftyDerivEqn14 = idx (sy intNormForce) (int 3) `mulRe` idx (sy shrResC) (int 3) $=
  (idx (sy mobShrC) (int 2) `mulRe` (idx (sy mobShrC) (int 1) `mulRe` (sy fs `mulRe` idx (sy shearFNoIntsl) (int 1) $-
  idx (sy shearRNoIntsl) (int 1)) `addRe` (sy fs `mulRe` idx (sy shearFNoIntsl) (int 2)) $-
  idx (sy shearRNoIntsl) (int 2)) `addRe` (sy fs `mulRe` idx (sy shearFNoIntsl) (int 3)) $-
  idx (sy shearRNoIntsl) (int 3))

-- Need to add ellipses where appropriate
fctSftyDerivEqn15 :: Expr
fctSftyDerivEqn15 = idx (sy intNormForce) (sy numbSlices $- int 1) `mulRe`
  idx (sy shrResC) (sy numbSlices $- int 1) $= (idx (sy mobShrC) (sy numbSlices $- int 2) `mulRe`
  (idx (sy mobShrC) (sy numbSlices $- int 3) `mulRe` (idx (sy mobShrC) (int 1) `mulRe`
  (sy fs `mulRe` idx (sy shearFNoIntsl) (int 1) $- idx (sy shearRNoIntsl) (int 1)) `addRe` (sy fs `mulRe`
  idx (sy shearFNoIntsl) (int 2)) $- idx (sy shearRNoIntsl) (int 2)) `addRe` (sy fs `mulRe`
  idx (sy shearFNoIntsl) (sy numbSlices $- int 2)) $-
  idx (sy shearRNoIntsl) (sy numbSlices $- int 2)) `addRe` (sy fs `mulRe`
  idx (sy shearFNoIntsl) (sy numbSlices $- int 1)) $-
  idx (sy shearRNoIntsl) (sy numbSlices $- int 1))

-- Ellipses needed here too
fctSftyDerivEqn16 :: Expr
fctSftyDerivEqn16 = neg ((sy fs `mulRe` indxn shearFNoIntsl $- indxn shearRNoIntsl) $/
  idx (sy mobShrC) (sy numbSlices $- int 1)) $= (idx (sy mobShrC) (sy numbSlices $-
  int 2) `mulRe` (idx (sy mobShrC) (sy numbSlices $- int 3) `mulRe` (idx (sy mobShrC) (int 1) `mulRe`
  (sy fs `mulRe` idx (sy shearFNoIntsl) (int 1) $- idx (sy shearRNoIntsl) (int 1)) `addRe` (sy fs `mulRe`
  idx (sy shearFNoIntsl) (int 2)) $- idx (sy shearRNoIntsl) (int 2)) `addRe` (sy fs `mulRe`
  idx (sy shearFNoIntsl) (sy numbSlices $- int 2)) $-
  idx (sy shearRNoIntsl) (sy numbSlices $- int 2)) `addRe` (sy fs `mulRe`
  idx (sy shearFNoIntsl) (sy numbSlices $- int 1)) $-
  idx (sy shearRNoIntsl) (sy numbSlices $- int 1))

-- Ellipses needed here too
fctSftyDerivEqn17 :: Expr
fctSftyDerivEqn17 = neg (sy fs `mulRe` indxn shearFNoIntsl $- indxn shearRNoIntsl) $=
  (idx (sy mobShrC) (sy numbSlices $- int 1) `mulRe` idx (sy mobShrC) (sy numbSlices $- int 2) `mulRe` idx (sy mobShrC) (int 1) `mulRe` (sy fs `mulRe` indx1 shearFNoIntsl $-
  indx1 shearRNoIntsl) `addRe` (idx (sy mobShrC) (sy numbSlices $- int 1) `mulRe` idx (sy mobShrC) (sy numbSlices $- int 2) `mulRe` idx (sy mobShrC) (int 2) `mulRe` (sy fs `mulRe`
  idx (sy shearFNoIntsl) (int 2) $- idx (sy shearRNoIntsl) (int 2))) `addRe`
  (idx (sy mobShrC) (sy numbSlices $- int 1) `mulRe` (sy fs `mulRe`
  idx (sy shearFNoIntsl) (sy numbSlices $- int 1) $-
  idx (sy shearRNoIntsl) (sy numbSlices $- int 1))))

-- Ellipses needed here too
fctSftyDerivEqn18 :: Expr
fctSftyDerivEqn18 = sy fs `mulRe` (idx (sy mobShrC) (sy numbSlices $- int 1) `mulRe`
  idx (sy mobShrC) (sy numbSlices $- int 2) `mulRe` idx (sy mobShrC) (int 1) `mulRe`
  idx (sy shearFNoIntsl) (int 1) `addRe` (idx (sy mobShrC) (sy numbSlices $- int 1) `mulRe`
  idx (sy mobShrC) (sy numbSlices $- int 2) `mulRe` idx (sy mobShrC) (int 2) `mulRe`
  idx (sy shearFNoIntsl) (int 2)) `addRe` (idx (sy mobShrC) (sy numbSlices $- int 1) `mulRe`
  idx (sy shearFNoIntsl) (sy numbSlices $- int 1)) `addRe` indxn shearFNoIntsl) $=
  (idx (sy mobShrC) (sy numbSlices $- int 1) `mulRe`
  idx (sy mobShrC) (sy numbSlices $- int 2) `mulRe` idx (sy mobShrC) (int 1) `mulRe`
  idx (sy shearRNoIntsl) (int 1) `addRe` (idx (sy mobShrC) (sy numbSlices $- int 1) `mulRe`
  idx (sy mobShrC) (sy numbSlices $- int 2) `mulRe` idx (sy mobShrC) (int 2) `mulRe`
  idx (sy shearRNoIntsl) (int 2)) `addRe` (idx (sy mobShrC) (sy numbSlices $- int 1) `mulRe`
  idx (sy shearRNoIntsl) (sy numbSlices $- int 1)) `addRe` indxn shearRNoIntsl)

------------------------------------------------------------------------

nrmShrFor :: InstanceModel
nrmShrFor = im (othModel' nrmShrForRC) [qwUC slopeDist, qwUC slopeHght, qwUC waterHght,
  qwUC waterWeight, qwUC slipDist, qwUC slipHght, qwUC constF]
  (qw normToShear) [] (map ref [chen2005, karchewski2012])
  (Just nrmShrDeriv) "nrmShrFor" [nrmShrFDesc]

nrmShrForRC :: RelationConcept
nrmShrForRC = makeRC "nrmShrForRC" (nounPhraseSP "normal and shear force proportionality constant")
  nrmShrFDesc nrmShrFRel -- nrmShrForL

nrmShrFRel :: Relation
nrmShrFRel = sy normToShear $= sum1toN (inxi nrmShearNum) $/ sum1toN (inxi nrmShearDen)

nrmShrFDesc :: Sentence
nrmShrFDesc = nrmShearNum `definedIn'''`
  nrmShrForNum `S.and_` (nrmShearDen `definedIn'''`
  nrmShrForDen !.)

nrmShrDeriv :: Derivation
nrmShrDeriv = mkDerivNoHeader (weave [nrmShrDerivationSentences, map eS nrmShrDerivEqns] ++
  nrmShrDerivSentence5)

nrmShrDerivSentence1 :: [Sentence]
nrmShrDerivSentence1 = [S "From the", phrase momentEqlGD `S.of_`
  refS momentEqlGD, S "with the primary", phrase assumption,
  S "for the Morgenstern-Price method of", refS assumpINSFL `S.and_`
  S "associated", phrase definition, refS normShrRGD `sC` eqN 14,
  S "can be derived"]

nrmShrDerivSentence2 :: [Sentence]
nrmShrDerivSentence2 = [S "Rearranging the", phrase equation, S "in terms of",
  ch normToShear, S "leads to", eqN 15]

nrmShrDerivSentence3 :: [Sentence]
nrmShrDerivSentence3 = [S "This", phrase equation, S "can be simplified by",
  S "applying", plural assumption, refS assumpSF `S.and_`
  refS assumpSL `sC` S "which state that the seismic" `S.and_`
  plural surfLoad `sC` S "respectively" `sC` S "are zero"]

nrmShrDerivSentence4 :: [Sentence]
nrmShrDerivSentence4 = [S "Taking the summation of all", plural slice `sC`
  S "and applying", refS assumpES, S "to set",
  eS (idx (sy intNormForce) (int 0)) `sC` eS (indxn intNormForce) `sC`
  eS (idx (sy watrForce) (int 0)) `sC` S "and", eS (indxn watrForce),
  S "equal to zero" `sC` S "a general", phrase equation, S "for the",
  getTandS normToShear, S "is developed in", eqN 16 `sC` S "which combines", refS nrmShrFor `sC` refS nrmShrForNum `sC` S "and",
  refS nrmShrForDen]

nrmShrDerivSentence5 :: [Sentence]
nrmShrDerivSentence5 = [eqN 16 `S.for` ch normToShear +:+
  S "is a function of the unknown" +:+ getTandS intNormForce +:+
  sParen (refS intsliceFs) +:+ S "which itself depends on the unknown" +:+
  getTandS fs +:+. sParen (refS fctSfty)]

nrmShrDerivationSentences :: [Sentence]
nrmShrDerivationSentences = map foldlSentCol [nrmShrDerivSentence1,
  nrmShrDerivSentence2, nrmShrDerivSentence3, nrmShrDerivSentence4]

nrmShrDerivEqns :: [Expr]
nrmShrDerivEqns = [nrmShrDerivEqn1, nrmShrDerivEqn2, nrmShrDerivEqn3,
  nrmShrDerivEqn4]

nrmShrDerivEqn1, nrmShrDerivEqn2, nrmShrDerivEqn3, nrmShrDerivEqn4 :: Expr
nrmShrDerivEqn1 = exactDbl 0 $=
  momExpr (\ x y -> x `addRe` (sy normToShear `mulRe` half (inxi baseWthX) `mulRe`
  (inxi intNormForce `mulRe` inxi scalFunc `addRe` (inxiM1 intNormForce `mulRe`
  inxiM1 scalFunc))) `addRe` y)

nrmShrDerivEqn2 = sy normToShear $= momExpr addRe
  $/ neg (half (inxi baseWthX) `mulRe` (inxi intNormForce `mulRe` inxi scalFunc `addRe`
  (inxiM1 intNormForce `mulRe` inxiM1 scalFunc)))

nrmShrDerivEqn3 = sy normToShear $= momExprNoKQ addRe
  $/ neg (half (inxi baseWthX) `mulRe` (inxi intNormForce `mulRe` inxi scalFunc `addRe`
  (inxiM1 intNormForce `mulRe` inxiM1 scalFunc)))

nrmShrDerivEqn4 = sy normToShear $= sum1toN
  (inxi baseWthX `mulRe` (sy nrmForceSum `addRe` sy watForceSum) `mulRe` tan (inxi baseAngle) `addRe`
  (inxi midpntHght `mulRe` neg (exactDbl 2 `mulRe` inxi surfHydroForce `mulRe` sin (inxi surfAngle))))
  $/ sum1toN
  (inxi baseWthX `mulRe` (inxi intNormForce `mulRe` inxi scalFunc `addRe`
  (inxiM1 intNormForce `mulRe` inxiM1 scalFunc)))

---------------------------------------------------------------------

nrmShrForNum :: InstanceModel
nrmShrForNum = im (othModel' nrmShrForNumRC) [qwUC slopeDist, qwUC slopeHght, qwUC waterHght,
  qwUC waterWeight, qwUC slipDist, qwUC slipHght]
  (qw nrmShearNum) [] (map ref [chen2005, karchewski2012])
  (Just nrmShrFNumDeriv) "nrmShrForNum" [nrmShrFNumDesc]

nrmShrForNumRC :: RelationConcept
nrmShrForNumRC = makeRC "nrmShrForNumRC" (nounPhraseSP "normal and shear force proportionality constant numerator")
  nrmShrFNumDesc nrmShrFNumRel

nrmShrFNumRel :: Relation
nrmShrFNumRel = inxi nrmShearNum $= incompleteCase [case1,case2,case3]
  where case1 = (indx1 baseWthX `mulRe` (indx1 intNormForce `addRe` indx1 watrForce) `mulRe`
          tan (indx1 baseAngle), sy index $= int 1)
        case2 = ((inxi baseWthX `mulRe`
          (sy nrmForceSum `addRe` sy watForceSum)
           `mulRe` tan (inxi baseAngle)) `addRe` (sy midpntHght `mulRe` (neg
          (exactDbl 2) `mulRe` inxi surfHydroForce `mulRe` sin (inxi surfAngle))),
          int 2 $<= sy index $<= (sy numbSlices $- int 1))
        case3 = (indxn baseWthX `mulRe` (idx (sy intNormForce)
          (sy numbSlices $- int 1) `addRe` idx (sy watrForce)
          (sy numbSlices $- int 1)) `mulRe` tan (idx (sy baseAngle)
          (sy numbSlices $- int 1)), sy index $= sy numbSlices)

nrmShrFNumDeriv :: Derivation
nrmShrFNumDeriv = mkDerivNoHeader [foldlSent [S "See", refS nrmShrFor,
  S "for the derivation" `S.of_` ch nrmShearNum]]

nrmShrFNumDesc :: Sentence
nrmShrFNumDesc = (foldlList Comma List [baseWthX `definedIn'''` lengthB,
  watrForce `definedIn'''` intersliceWtrF,
  baseAngle `definedIn'''` angleA,
  midpntHght `definedIn'''` slcHeight,
  surfHydroForce `definedIn'''` srfWtrFGD,
  surfAngle `definedIn'''` angleB] !.)

---------------------------------------------------------------------------

nrmShrForDen :: InstanceModel
nrmShrForDen = im (othModel' nrmShrForDenRC) [qwUC slipDist, qwUC constF]
  (qw nrmShearDen) [] (map ref [chen2005, karchewski2012])
  (Just nrmShrFDenDeriv) "nrmShrForDen" [nrmShrFDenDesc]

nrmShrForDenRC :: RelationConcept
nrmShrForDenRC = makeRC "nrmShrForDenRC" (nounPhraseSP "normal and shear force proportionality constant denominator")
  nrmShrFDenDesc nrmShrFDenRel

nrmShrFDenRel :: Relation
nrmShrFDenRel = inxi nrmShearDen $= incompleteCase [
  (indx1 baseWthX `mulRe` indx1 scalFunc `mulRe` indx1 intNormForce, sy index $= int 1),
  (inxi baseWthX `mulRe` (inxi scalFunc `mulRe` inxi intNormForce `addRe`
    (inxiM1 scalFunc `mulRe` inxiM1 intNormForce)),
    int 2 $<= sy index $<= (sy numbSlices $- int 1)),
  (indxn baseWthX `mulRe` idx (sy intNormForce) (sy numbSlices $- int 1) `mulRe`
    idx (sy scalFunc) (sy numbSlices $- int 1), sy index $= sy numbSlices)
  ]

nrmShrFDenDeriv :: Derivation
nrmShrFDenDeriv = mkDerivNoHeader [foldlSent [S "See", refS nrmShrFor,
  S "for the derivation" `S.of_` ch nrmShearDen]]

nrmShrFDenDesc :: Sentence
nrmShrFDenDesc = baseWthX `definedIn'''`
  lengthB `S.and_` (scalFunc `definedIn'''`
  ratioVariation !.)

--------------------------------------------------------------------------

intsliceFs :: InstanceModel
intsliceFs = im (othModel' intsliceFsRC) [qwUC slopeDist, qwUC slopeHght, qwUC waterHght
  , qwUC effCohesion, qwUC fricAngle, qwUC dryWeight, qwUC satWeight, qwUC waterWeight
  , qwUC slipDist, qwUC slipHght, qwUC constF]
  (qw intNormForce) [] [ref chen2005] (Just intrSlcDeriv) "intsliceFs" [sliceFsDesc]

intsliceFsRC :: RelationConcept
intsliceFsRC = makeRC "intsliceFsRC" (nounPhraseSP "interslice normal forces")
  sliceFsDesc sliceFsRel -- inslideFxL

sliceFsRel :: Relation
sliceFsRel = inxi intNormForce $= incompleteCase [
  ((sy fs `mulRe` indx1 shearFNoIntsl $- indx1 shearRNoIntsl) $/ indx1 shrResC,
    sy index $= int 1),
  ((inxiM1 mobShrC `mulRe` inxiM1 intNormForce `addRe`
    (sy fs `mulRe` inxi shearFNoIntsl $- inxi shearRNoIntsl)) $/ inxi shrResC,
    int 2 $<= sy index $<= (sy numbSlices $- int 1)),
  (int 0, sy index $= int 0 $|| sy index $= sy numbSlices)]
  -- FIXME: Use index i as part of condition

sliceFsDesc :: Sentence
sliceFsDesc = (foldlList Comma List [shearFNoIntsl `definedIn'''` mobShearWOGD,
  shearRNoIntsl `definedIn'''` resShearWOGD,
  shrResC `definedIn'''` convertFunc1,
  mobShrC `definedIn'''` convertFunc2] !.)

intrSlcDeriv :: Derivation
intrSlcDeriv = mkDerivNoHeader (weave [intrSlcDerivationSentences, map eS intrSlcDerivEqns] ++ intrSlcDerivSentence3)

intrSlcDerivSentence1 :: [Sentence]
intrSlcDerivSentence1 = [S "This derivation is identical to the derivation for",
  refS fctSfty, S "up until", eqN 3, S "shown again below"]

intrSlcDerivSentence2 :: [Sentence]
intrSlcDerivSentence2 = [S "A simple rearrangement of", eqN 3, S "leads to",
  eqN 17 `sC` S "also seen in", refS intsliceFs]

intrSlcDerivSentence3 :: [Sentence]
intrSlcDerivSentence3 = [S "The cases shown in" +:+ refS intsliceFs +:+
  S "for when" +:+ eS (sy index $= int 0) `sC` eS (sy index $= int 1) `sC` S "or" +:+
  eS (sy index $= sy numbSlices) +:+ S "are derived by applying" +:+
  refS assumpES `sC` S "which says that" +:+
  eS (idx (sy intNormForce) (int 0)) `S.and_` eS (indxn intNormForce) +:+
  S "are zero" `sC` S "to" +:+. eqN 17 +:+ ch intNormForce +:+
  S "depends on the unknowns" +:+ ch fs +:+ sParen (refS fctSfty) `S.and_`
  ch normToShear +:+. sParen (refS nrmShrFor)]

intrSlcDerivationSentences :: [Sentence]
intrSlcDerivationSentences = map foldlSentCol [intrSlcDerivSentence1,
  intrSlcDerivSentence2]

intrSlcDerivEqns :: [Expr]
intrSlcDerivEqns = [fctSftyDerivEqn9, intrSlcDerivEqn]

intrSlcDerivEqn :: Expr
intrSlcDerivEqn = inxi intNormForce $=
  (inxiM1 mobShrC `mulRe` inxiM1 intNormForce `addRe`
  (sy fs `mulRe` inxi shearFNoIntsl) $- inxi shearRNoIntsl) $/ inxi shrResC

--------------------------------------------------------------------------
crtSlpId :: InstanceModel
crtSlpId = imNoDeriv crtSlpIdMK [qwUC slopeDist, qwUC slopeHght, qwUC waterDist,
  qwUC waterHght, qwUC effCohesion, qwUC fricAngle, qwUC dryWeight, qwUC satWeight,
  qwUC waterWeight, qwUC constF] (qw fsMin) [] [ref li2010] "crtSlpId"
  [crtSlpIdDesc]

crtSlpIdMK :: ModelKind
crtSlpIdMK = equationalModel "crtSlpIdIM"
  (nounPhraseSP "critical slip surface identification") crtSlpIdQD

crtSlpIdQD :: QDefinition
crtSlpIdQD = mkQuantDef fsMin crtSlpIdExpr

-- FIXME: horrible hack. This is short an argument... that was never defined!
-- FIXME: critCoords should also be an output
crtSlpIdExpr :: Expr
crtSlpIdExpr = apply minFunction [sy slopeDist,
  sy slopeHght, sy waterDist, sy waterHght, sy effCohesion, sy fricAngle,
  sy dryWeight, sy satWeight, sy waterWeight, sy constF]

-- FIXME: The constraints described here should be replaced with formal constraints on the input variables once that is possible
crtSlpIdDesc :: Sentence
crtSlpIdDesc = foldlSent [atStartNP (the minFunction), S "must enforce the",
  pluralNP (constraint `onThePS` crtSlpSrf), S "expressed in" +:+.
  (refS assumpSSC `S.and_` refS (propCorSol [] [])),
  S "The sizes of", ch waterDist `S.and_` ch waterHght +:+.
  S "must be equal and not 1", S "The", S "sizes of", ch slopeDist `S.and_`
  ch slopeHght +:+. (S "must be equal" `S.and_` S "at least 2"),
  atStartNP (the first) `S.and_` S "last", ch waterDist,
  plural value, S "must be equal to the", phrase first `S.and_` S "last",
  ch slopeDist +:+. plural value, ch waterDist `S.and_` ch slopeDist,
  plural value +:+. S "must be monotonically increasing", ch xMaxExtSlip `sC`
  ch xMaxEtrSlip `sC` ch xMinExtSlip `sC` S "and", ch xMinEtrSlip, S "must be",
  S "between or equal to the minimum and maximum", ch slopeDist +:+.
  plural value, ch yMaxSlip, S "cannot be below the minimum", ch slopeHght +:+.
  phrase value, ch yMinSlip, S "cannot be above the maximum", ch slopeHght +:+.
  phrase value, S "All", ch xi, plural value `S.of_` ch critCoords, S "must be",
  S "between" +:+. (ch xMinEtrSlip `S.and_` ch xMaxExtSlip), S "All", ch yi,
  plural value `S.of_` ch critCoords, S "must not be below" +:+. ch yMinSlip,
  S "For any given vertex in", ch critCoords, S "the", ch yi, phrase value,
  S "must not exceed the", ch slopeHght, phrase value, S "corresponding to the",
  S "same", ch xi +:+. phrase value, atStartNP (the first) `S.and_` S "last",
  S "vertices in", ch critCoords, S "must each be equal to one of the vertices",
  S "formed by" +:+. (ch slopeDist `S.and_` ch slopeHght), S "The slope between",
  S "consecutive vertices must be always increasing as", ch xi +:+.
  S "increases", S "The internal", phrase angle, S "between consecutive",
  S "vertices in", ch critCoords +:+ S "must not be below 110 degrees"]

-----------
-- Intro --
-----------

instModIntro :: [Sentence]
instModIntro = [instModIntro1, instModIntro2]

instModIntro1, instModIntro2 :: Sentence

instModIntro1 = foldlSent [atStartNP' (the goal), foldlList Comma List
  (map refS goals) `S.are` S "met by the simultaneous" +:+. (phrase solution `S.of_`
  foldlList Comma List (map refS [fctSfty, nrmShrFor, intsliceFs])), atStartNP (the goal),
  refS identifyCritAndFSGS `S.is` S "also contributed to by",
  refS crtSlpId]

instModIntro2 = foldlSent [titleizeNP (the morPrice),
  phrase method_, S "is a vertical", phrase slice `sC` S "limit equilibrium",
  phrase ssa +:+. phrase method_, atStart analysis, S "is performed by",
  S "breaking the assumed", phrase slpSrf,
  S "into a series of vertical", plural slice, S "of" +:+. phrase mass,
  S "Static equilibrium analysis is performed, using two", phrase force,
  plural equation `S.and_` S "one moment", phrase equation, S "as in" +:+. refS equilibrium,
  atStartNP (the problem), S "is statically indeterminate with only these 3",
  plural equation, S "and one constitutive", phrase equation,
  sParen $ S "the Mohr Coulomb shear strength of" +:+
  refS mcShrStrgth, S "so the", phrase assumption,
  refS normShrRGD `S.and_` S "corresponding equation",
  refS normShrRGD +:+. S "are used",
  atStartNP (the force), S "equilibrium", plural equation, S "can be modified",
  S "to be expressed only in terms of known", phrase physical, plural value `sC`
  S "as done in", refS resShearWOGD `S.and_` refS mobShearWOGD]

-- References -- 
iModRefs :: [Reference]
iModRefs = map ref iMods