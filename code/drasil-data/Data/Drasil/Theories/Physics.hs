module Data.Drasil.Theories.Physics where

import Language.Drasil
import Theory.Drasil
import Utils.Drasil
import qualified Utils.Drasil.Sentence as S

import Data.Drasil.Citations (velocityWiki, accelerationWiki)
import Data.Drasil.Concepts.Documentation (component, material_, value, constant)
import Data.Drasil.Concepts.Math (cartesian, equation, vector)
import Data.Drasil.Concepts.Physics (gravity, twoD, rigidBody)
import qualified Data.Drasil.Quantities.PhysicalProperties as QPP (density, 
  mass, specWeight, vol)
import qualified Data.Drasil.Quantities.Physics as QP (acceleration, velocity, position,
  force, gravitationalAccel, pressure, torque, weight, positionVec, time, momentOfInertia,
  angularAccel, speed)
import Data.Drasil.Equations.Defining.Physics (newtonSLQD, newtonSLDesc, weightEqn,
  weightDerivAccelEqn, weightDerivNewtonEqn, weightDerivReplaceMassEqn, weightDerivSpecWeightEqn,
  hsPressureEqn, accelerationQD, velocityQD, speedEqn)

physicsTMs :: [TheoryModel]
physicsTMs = [newtonSL]

newtonSL :: TheoryModel
newtonSL = tmNoRefs (equationalModelU "newtonSL" newtonSLQD)
  [qw QP.force, qw QPP.mass, qw QP.acceleration] ([] :: [ConceptChunk])
  [newtonSLQD] [] [] "NewtonSecLawMot" [newtonSLDesc]

--

weightGD :: GenDefn
weightGD = gd (equationalModel' weightQD) (getUnit QP.weight) (Just weightDeriv) [weightSrc] 
  "weight" [{-Notes-}]

weightQD :: QDefinition
weightQD = mkQuantDef' QP.weight (nounPhraseSP "weight") weightEqn

weightSrc :: Reference
weightSrc = makeURI "weightSrc" "https://en.wikipedia.org/wiki/Weight" $
  shortname' $ S "Definition of Weight"

weightDeriv :: Derivation
weightDeriv = mkDerivName (phrase QP.weight) $ weave [weightDerivSentences, weightDerivEqns]

weightDerivSentences, weightDerivEqns :: [Sentence]
weightDerivSentences = map foldlSentCol [weightDerivAccelSentence, 
  weightDerivNewtonSentence, weightDerivReplaceMassSentence, 
  weightDerivSpecWeightSentence]
weightDerivEqns = map eS [weightDerivAccelEqn, weightDerivNewtonEqn, 
  weightDerivReplaceMassEqn, weightDerivSpecWeightEqn]

weightDerivAccelSentence :: [Sentence]
weightDerivAccelSentence = [S "Under the influence" `S.of_` phrase gravity `sC` 
  S "and assuming a", short twoD, phrase cartesian, S "with down as positive" `sC`
  S "an object has an", phrase QP.acceleration, phrase vector, S "of"]

weightDerivNewtonSentence :: [Sentence]
weightDerivNewtonSentence = [S "Since there is only one non-zero", 
  phrase vector, phrase component `sC` S "the scalar", phrase value, 
  ch QP.weight, S "will be used for the" +:+. phrase QP.weight,
  S "In this scenario" `sC` phrase newtonSL, S "from", refS newtonSL, 
  S "can be expressed as"]

weightDerivReplaceMassSentence :: [Sentence]
weightDerivReplaceMassSentence = [atStart QPP.mass, S "can be expressed as",
  phrase QPP.density, S "multiplied by", phrase QPP.vol `sC` S "resulting in"]

weightDerivSpecWeightSentence :: [Sentence]
weightDerivSpecWeightSentence = [S "Substituting", phrase QPP.specWeight, 
  S "as the product" `S.of_` phrase QPP.density `S.and_` phrase QP.gravitationalAccel,
  S "yields"]

--

hsPressureGD :: GenDefn
hsPressureGD = gd (equationalModel' hsPressureQD) (getUnit QP.pressure) Nothing
  [hsPressureSrc] "hsPressure" [hsPressureNotes]

hsPressureQD :: QDefinition
hsPressureQD = mkQuantDef' QP.pressure (nounPhraseSP "hydrostatic pressure") hsPressureEqn

hsPressureSrc :: Reference
hsPressureSrc = makeURI "hsPressureSrc" "https://en.wikipedia.org/wiki/Pressure" $
  shortname' $ S "Definition of Pressure"

hsPressureNotes :: Sentence
hsPressureNotes = S "This" +:+ phrase equation +:+ S "is derived from" +:+
  S "Bernoulli's" +:+ phrase equation +:+ S "for a slow moving fluid" +:+
  S "through a porous" +:+. phrase material_

--

torqueDD :: DataDefinition
torqueDD = ddNoRefs torque Nothing "torque" [torqueDesc] 

torque :: QDefinition
torque = mkQuantDef QP.torque torqueEqn

torqueEqn :: Expr
torqueEqn = sy QP.positionVec `cross` sy QP.force

torqueDesc :: Sentence
torqueDesc = foldlSent [S "The", phrase torque, 
  S "on a body measures the", S "tendency" `S.of_` S "a", phrase QP.force, 
  S "to rotate the body around an axis or pivot"]

--

vecMagQD :: QDefinition
vecMagQD = mkQuantDef QP.speed speedEqn

magNote :: Sentence
magNote = foldlSent [S "For a given", phrase QP.velocity, S "vector", ch QP.velocity `sC`
  S "the magnitude of the vector", sParen (eS speedEqn) `S.isThe`
  S "scalar called", phrase QP.speed]

vecMag :: DataDefinition
vecMag = ddNoRefs vecMagQD Nothing "vecMag" [magNote]

--
newtonSLR :: TheoryModel
newtonSLR = tmNoRefs (equationalModelU "newtonSLR" newtonSLRQD)
  [qw QP.torque, qw QP.momentOfInertia, qw QP.angularAccel] 
  ([] :: [ConceptChunk]) [newtonSLRQD] [] [] "NewtonSecLawRotMot" newtonSLRNotes

newtonSLRQD :: QDefinition
newtonSLRQD = mkQuantDef' QP.torque (nounPhraseSP "Newton's second law for rotational motion") newtonSLRExpr

newtonSLRExpr :: Expr
newtonSLRExpr = sy QP.momentOfInertia `mulRe` sy QP.angularAccel

newtonSLRNotes :: [Sentence]
newtonSLRNotes = map foldlSent [
  [S "The net", getTandS QP.torque, S "on a", phrase rigidBody `S.is`
   S "proportional to its", getTandS QP.angularAccel `sC` S "where",
   ch QP.momentOfInertia, S "denotes", phrase QP.momentOfInertia `S.the_ofThe`
   phrase rigidBody, S "as the", phrase constant `S.of_` S "proportionality"]]
--

accelerationTM :: TheoryModel
accelerationTM = tm (equationalModelU "accelerationTM" accelerationQD)
  [qw QP.acceleration, qw QP.velocity, qw QP.time] ([] :: [ConceptChunk]) [accelerationQD] [] []
  [ref accelerationWiki] "acceleration" []

----------

velocityTM :: TheoryModel
velocityTM = tm (equationalModelU "velocityTM" velocityQD)
  [qw QP.velocity, qw QP.position, qw QP.time] ([] :: [ConceptChunk]) [velocityQD] [] []
  [ref velocityWiki] "velocity" []
