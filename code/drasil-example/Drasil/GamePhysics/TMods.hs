{-# LANGUAGE PostfixOperators #-}
module Drasil.GamePhysics.TMods (tMods, newtonSL, newtonSLR, newtonTL, newtonLUG, tModRefs) where

import qualified Data.List.NonEmpty as NE

import Language.Drasil
import Theory.Drasil
import Utils.Drasil
import qualified Utils.Drasil.Sentence as S

import Drasil.GamePhysics.Assumptions (assumpOD)
import Drasil.GamePhysics.Unitals (dispNorm, dVect, force_1, force_2,
  mass_1, mass_2, sqrDist, distMass)

import Data.Drasil.Concepts.Documentation (constant)
import Data.Drasil.Concepts.Physics (rigidBody, twoD)
import Data.Drasil.Quantities.PhysicalProperties (mass)
import Data.Drasil.Quantities.Physics (angularAccel,
  force, gravitationalConst, momentOfInertia, torque)
import Data.Drasil.Theories.Physics (newtonSL)

----- Theoretical Models -----

tMods :: [TheoryModel]
tMods = [newtonSL, newtonTL, newtonLUG, newtonSLR]

-- T1 : Newton's second law of motion --

-- T2 : Newton's third law of motion --

newtonTL :: TheoryModel
newtonTL = tmNoRefs (equationalModel' newtonTLQD) [qw force_1, qw force_2]
  ([] :: [ConceptChunk]) [newtonTLQD] [] [] "NewtonThirdLawMot" [newtonTLNote]

newtonTLQD :: QDefinition
newtonTLQD = mkQuantDef' force_1 (nounPhraseSP "Newton's third law of motion") newtonTLExpr

newtonTLExpr :: Expr
newtonTLExpr = neg (sy force_2)

newtonTLNote :: Sentence
newtonTLNote = foldlSent [(S "Every action has an equal and opposite reaction" !.),
  S "In other words, the", phrase force, ch force_1, S "exerted on the second",
  phrase rigidBody, S "by the first is equal in magnitude and in the opposite direction" `S.toThe`
  phrase force, ch force_2, S "exerted on the first", phrase rigidBody, S "by the second"]

-- T3 : Newton's law of universal gravitation --

-- FIXME: Missing ConceptDomain!
newtonLUGModel :: ModelKind
newtonLUGModel = equationalRealm' $ mkMultiDefnForQuant newtonForceQuant EmptyS $ NE.fromList [
    mkDefiningExpr "newtonLUGviaDeriv" [] EmptyS (sy gravitationalConst `mulRe` (sy mass_1 `mulRe` sy mass_2 $/ square (sy dispNorm)) `mulRe` sy dVect),
    mkDefiningExpr "newtonLUGviaForm" [] EmptyS (sy gravitationalConst `mulRe` (sy mass_1 `mulRe` sy mass_2 $/ square (sy dispNorm)) `mulRe` (sy distMass $/ sy dispNorm))
  ]

newtonLUG :: TheoryModel
newtonLUG = tmNoRefs newtonLUGModel
  [qw force, qw gravitationalConst, qw mass_1, qw mass_2,
  qw dispNorm, qw dVect, qw distMass] ([] :: [ConceptChunk])
  [] [toDispExpr newtonLUGModel] [] "UniversalGravLaw" newtonLUGNotes

newtonForceQuant :: QuantityDict
newtonForceQuant = mkQuant' "force" (nounPhraseSP "Newton's law of universal gravitation") Nothing Real (symbol force) Nothing

-- Can't include fractions within a sentence (in the part where 'r denotes the
-- unit displacement vector, equivalent to r/||r||' (line 184)). Changed to a
-- verbal description instead.

-- Can't properly include the gravitational constant in a sentence (in the last
-- sentence, supposed to include "6.673 `mulRe` 10^{-11} m/kgs^2" (line 187)).

newtonLUGNotes :: [Sentence]
newtonLUGNotes = map foldlSent [
  [S "Two", plural rigidBody `S.inThe` S "universe attract each other with a",
   getTandS force, S "that is directly proportional to the product of their",
   plural mass `sC` ch mass_1 `S.and_` ch mass_2 `sC` EmptyS `S.and_`
   S "inversely proportional" `S.toThe` getTandS sqrDist, S "between them"]]

-- T4 : Newton's second law for rotational motion --

newtonSLR :: TheoryModel
newtonSLR = tmNoRefs (equationalModelU "newtonSLR" newtonSLRQD)
  [qw torque, qw momentOfInertia, qw angularAccel]
  ([] :: [ConceptChunk]) [newtonSLRQD] [] [] "NewtonSecLawRotMot" newtonSLRNotes

newtonSLRQD :: QDefinition
newtonSLRQD = mkQuantDef' torque (nounPhraseSP "Newton's second law for rotational motion") newtonSLRExpr

newtonSLRExpr :: Relation
newtonSLRExpr = sy momentOfInertia `mulRe` sy angularAccel

newtonSLRNotes :: [Sentence]
newtonSLRNotes = map foldlSent [
  [S "The net", getTandS torque, S "on a", phrase rigidBody `S.is`
   S "proportional to its", getTandS angularAccel `sC` S "where",
   ch momentOfInertia, S "denotes", phrase momentOfInertia `S.the_ofThe`
   phrase rigidBody, S "as the", phrase constant `S.of_` S "proportionality"],
  [S "We also assume that all", plural rigidBody, S "involved" `S.are`
   phrase twoD, fromSource assumpOD]]

-- References --
tModRefs :: [Reference]
tModRefs = map ref tMods
