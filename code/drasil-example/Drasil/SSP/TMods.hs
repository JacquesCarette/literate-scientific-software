{-# LANGUAGE PostfixOperators #-}
module Drasil.SSP.TMods (tMods, factOfSafety, equilibrium, mcShrStrgth, effStress, tModRefs) 
  where

import Control.Lens ((^.))
import Prelude hiding (tan)
import qualified Data.List.NonEmpty as NE

import Language.Drasil
import Theory.Drasil
import Utils.Drasil
import Utils.Drasil.Concepts
import qualified Utils.Drasil.Sentence as S

import Data.Drasil.Quantities.Physics (distance, force)

import Data.Drasil.Concepts.Documentation (model)
import Data.Drasil.Concepts.Physics (friction, linear)
import Data.Drasil.Theories.Physics (newtonSL)

import Drasil.SSP.Assumptions (assumpENSL, assumpSBSBISL)
import Drasil.SSP.Defs (factorOfSafety)
import Drasil.SSP.References (fredlund1977)
import Drasil.SSP.Unitals (effCohesion, effNormStress, effectiveStress, 
  fricAngle, fs, fx, fy, genericM, mobilizedShear, nrmFSubWat, porePressure, 
  resistiveShear, shrStress, totNormStress)
import Drasil.SSP.DataDefs (normStressDD)

--------------------------
--  Theoretical Models  --
--------------------------
tMods :: [TheoryModel]
tMods = [factOfSafety, equilibrium, mcShrStrgth, effStress, newtonSL]

------------- New Chunk -----------
factOfSafety :: TheoryModel
factOfSafety = tm (equationalModelU "factOfSafetyTM" factOfSafetyQD)
  [qw fs, qw resistiveShear, qw mobilizedShear] ([] :: [ConceptChunk])
  [factOfSafetyQD] [] [] [ref fredlund1977] "factOfSafety" []

------------------------------------
factOfSafetyQD :: QDefinition
factOfSafetyQD = mkQuantDef' fs factorOfSafety factOfSafetyExpr

factOfSafetyExpr :: Expr
factOfSafetyExpr = sy resistiveShear $/ sy mobilizedShear

--
------------- New Chunk -----------
equilibrium :: TheoryModel
equilibrium = tm (equationalConstraints' equilibriumCS)
  [qw fx] ([] :: [ConceptChunk])
  [] (map toDispExpr equilibriumRels) [] [ref fredlund1977] "equilibrium" [eqDesc]

------------------------------------  

equilibriumRels :: [Expr]
equilibriumRels = map (($= int 0) . sumAll (variable "i") . sy) [fx, fy, genericM]

-- FIXME: variable "i" is a hack.  But we need to sum over something!
equilibriumCS :: ConstraintSet
equilibriumCS = mkConstraintSet
  (dccWDS "equilibriumCS" (nounPhraseSP "equilibrium") eqDesc) $
  NE.fromList equilibriumRels
-- makeRC "equilibriumRC" (nounPhraseSP "equilibrium") eqDesc eqRel

eqDesc :: Sentence
eqDesc = foldlSent [S "For a body in static equilibrium, the net",
  pluralNP (force `and_PP` genericM) +:+. S "acting on the body will cancel out",
  S "Assuming a 2D problem", sParen (refS assumpENSL) `sC` S "the", getTandS fx `S.and_`
  getTandS fy, S "will be equal to" +:+. eS (exactDbl 0), S "All", plural force,
  S "and their", phrase distance, S "from the chosen point of rotation",
  S "will create a net", phrase genericM, S "equal to" +:+ eS (exactDbl 0)]

--
------------- New Chunk -----------
mcShrStrgth :: TheoryModel
mcShrStrgth = tm (equationalModelU "mcShrSrgth" mcShrStrgthQD)
  [qw shrStress, qw effNormStress, qw fricAngle, qw effCohesion] 
  ([] :: [ConceptChunk])
  [mcShrStrgthQD] [] [] [ref fredlund1977] "mcShrStrgth" [mcShrStrgthDesc]

------------------------------------
mcShrStrgthQD :: QDefinition
mcShrStrgthQD = fromEqnSt' (shrStress ^. uid) (nounPhraseSP "Mohr-Coulumb shear strength")
 mcShrStrgthDesc (symbol shrStress) Real mcShrStrgthExpr

mcShrStrgthExpr :: Expr
mcShrStrgthExpr = sy effNormStress `mulRe` tan (sy fricAngle) `addRe` sy effCohesion

mcShrStrgthDesc :: Sentence
mcShrStrgthDesc = foldlSent [S "In this", phrase model, S "the",
  getTandS shrStress, S "is proportional to the product of the",
  phrase effNormStress, ch effNormStress, S "on the plane", 
  S "with its static", phrase friction, S "in the angular form" +:+.
  eS (tan $ sy fricAngle),
  S "The", ch shrStress, S "versus", ch effNormStress,
  S "relationship is not truly",
  phrase linear `sC` S "but assuming the", phrase nrmFSubWat, 
  S "is strong enough, it can be approximated with a", phrase linear,
  S "fit", sParen (refS assumpSBSBISL), S "where the", phrase effCohesion, 
  ch effCohesion, S "represents the", ch shrStress,
  S "intercept of the fitted line"]

--
------------- New Chunk -----------
effStress :: TheoryModel
effStress = tm (equationalModelU "effectiveStressTM" effStressQD)
  [qw effectiveStress, qw totNormStress, qw porePressure] 
  ([] :: [ConceptChunk])
  [effStressQD] [] [] [ref fredlund1977] "effStress" [effStressDesc]

------------------------------------
effStressQD :: QDefinition
effStressQD = fromEqnSt' (effectiveStress ^. uid) (nounPhraseSP "effective stress")
 effStressDesc (symbol effectiveStress) Real effStressExpr

effStressExpr :: Expr
effStressExpr = sy totNormStress $- sy porePressure

effStressDesc :: Sentence
effStressDesc = (totNormStress `definedIn'''` normStressDD !.)

-- References --
tModRefs :: [Reference]
tModRefs = map ref tMods