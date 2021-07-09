module Drasil.GamePhysics.DataDefs (dataDefs, ctrOfMassDD, linDispDD, linVelDD,
  linAccDD, angDispDD, angVelDD, angAccelDD, torqueDD, kEnergyDD,
  coeffRestitutionDD, reVelInCollDD, impulseVDD, momentOfInertiaDD,
  collisionAssump, rightHandAssump, rigidTwoDAssump, potEnergyDD, dataDefRefs) where

import Language.Drasil
import Theory.Drasil (DataDefinition, dd, ddNoRefs)
import Utils.Drasil
import Utils.Drasil.Concepts
import qualified Utils.Drasil.Sentence as S
import Control.Lens ((^.))

import Drasil.GamePhysics.Assumptions (assumpOT, assumpOD, assumpAD, assumpCT, assumpDI)
import Drasil.GamePhysics.References (chaslesWiki)
import Drasil.GamePhysics.Unitals (initRelVel, massj, mTot, normalVect,
  posCM, posj, velB, velO, rOB, finRelVel,
  velAP, velBP, rRot, velo_1, velo_2, timeT, time_1, time_2, massj,
   mTot, normalVect, velo_1, velo_2)

import Data.Drasil.Concepts.Math (rightHand)
import Data.Drasil.Concepts.Physics (rigidBody, twoD)

import qualified Data.Drasil.Quantities.Math as QM (orientation)
import qualified Data.Drasil.Quantities.Physics as QP (angularAccel, angularDisplacement, angularVelocity,
  displacement, linearAccel, linearDisplacement, linearVelocity, position,
  restitutionCoef, time, velocity,force, torque, kEnergy, energy, impulseV, chgInVelocity,
  acceleration, potEnergy, height, gravitationalAccel, momentOfInertia)

import qualified Data.Drasil.Quantities.PhysicalProperties as QPP (mass)

import Data.Drasil.Theories.Physics (torqueDD)
----- Data Definitions -----

dataDefs :: [DataDefinition]
dataDefs = [ctrOfMassDD, linDispDD, linVelDD, linAccDD, angDispDD,
 angVelDD, angAccelDD, chaslesDD, torqueDD, kEnergyDD,
 coeffRestitutionDD, reVelInCollDD, impulseVDD, potEnergyDD, momentOfInertiaDD]

-- DD1 : Centre of mass --

ctrOfMassDD :: DataDefinition
ctrOfMassDD = ddNoRefs ctrOfMass Nothing "ctrOfMass" [rigidBodyAssump]

ctrOfMass :: QDefinition
ctrOfMass = mkQuantDef posCM ctrOfMassEqn

-- FIXME (variable "i") is a horrible hack
ctrOfMassEqn :: Expr
ctrOfMassEqn = sumAll (variable "j") (sy massj `mulRe` sy posj) $/ sy mTot

-- DD2 : Linear displacement --

--FIXME: Why do QDefinitions only have term and not defn? They should have both!
--FIXME: HACK - Adding fixme defn for now, since NP can't (currently) incorporate sentence.
--fixme :: NP
--fixme = nounPhraseSP $ "FIXME: THIS HACK NEEDS TO BE UPDATED TO A NOUNPHRASE, " ++
--  "SEE Drasil.GamePhysics.DataDefs for more info"
{-
linDispQDef :: Sentence
linDispQDef = foldl (+:+) (EmptyS) def
  where def = [phrase $ QP.linearDisplacement ^. term, S "of a",
              phrase $ rigidBody ^. term, S "as a function of",
              phrase $ QP.time ^. term, ch QP.time,
              S "also equal to the derivate of its linear",
              phrase $ QP.velocity ^. term, S "with respect to",
              phrase $ QP.time ^. term, ch QP.time]
-}

linDispDD :: DataDefinition
linDispDD = ddNoRefs linDisp Nothing "linDisp" [rigidBodyAssump]

linDisp :: QDefinition
linDisp = mkQuantDef QP.linearDisplacement dispEqn

dispEqn :: Expr
dispEqn = deriv (apply1 QP.position QP.time) QP.time
{-
dd2descr :: Sentence
dd2descr = S "linear" +:+ (QP.displacement ^. term) +:+ S "of a" +:+
  ( rigidBody ^. term) +:+ S "as a function of" +:+ (QP.time ^. term) +:+ 
  ch QP.time +:+ sParen (Sy (unit_symb QP.time)) `sC`
  S "also equal to the derivative of its linear" +:+ (QP.position ^. term) +:+ 
  S "with respect to" +:+ (QP.time ^. term) +:+ ch QP.time
-}
-- DD3 : Linear velocity --

{-
linVelQDef :: Sentence
linVelQDef = foldl (+:+) (EmptyS) def
  where def = [phrase $ QP.linearVelocity ^. term, S "of a",
              phrase $ rigidBody ^. term, S "as a function of" ,
              phrase $ QP.time ^. term, QP.time,
              S "also equal to the derivative of its linear",
              phrase $ QP.velocity ^. term, S "with respect to",
              phrase $ QP.time ^. term, ch QP.time]
-}

linVelDD :: DataDefinition
linVelDD = ddNoRefs linVel Nothing "linVel" [rigidBodyAssump]

linVel :: QDefinition
linVel = mkQuantDef QP.linearVelocity velEqn

velEqn :: Expr
velEqn = deriv (apply1 QP.displacement QP.time) QP.time
{-
dd3descr :: Sentence
dd3descr = S "linear" +:+ (QP.velocity ^. term) +:+ S "of a" +:+
  (rigidBody ^. term) +:+ S "as a function of" +:+ (QP.time ^. term) +:+ 
  ch QP.time +:+ sParen (Sy (unit_symb QP.time)) `sC`
  S "also equal to the derivative of its linear" +:+ (QP.velocity ^. term) +:+
  S "with respect to" +:+ (QP.time ^. term) +:+ ch QP.time
-}
-- DD4 : Linear acceleration --

linAccDD :: DataDefinition
linAccDD = ddNoRefs linAcc Nothing "linAcc" [rigidBodyAssump]

linAcc :: QDefinition
linAcc = mkQuantDef QP.linearAccel accelEqn

accelEqn :: Expr
accelEqn = deriv (apply1 QP.velocity QP.time) QP.time
{-
dd4descr :: Sentence
dd4descr = S "linear" +:+ (accel ^. term) +:+ S "of a" +:+
  (rigidBody ^. term) +:+ S "as a function of" +:+ (QP.time ^. term) +:+ 
  ch QP.time +:+ sParen (Sy (unit_symb QP.time)) `sC`
  S "also equal to the derivative of its linear" +:+ (accel ^. term) +:+
  S "with respect to" +:+ (QP.time ^. term) +:+ ch QP.time
-}
-- DD5 : Angular displacement --

angDispDD :: DataDefinition
angDispDD = ddNoRefs angDisp Nothing "angDisp" [rigidTwoDAssump]

angDisp :: QDefinition
angDisp = mkQuantDef QP.angularDisplacement angDispEqn

angDispEqn :: Expr
angDispEqn = deriv (apply1 QM.orientation QP.time) QP.time
{-
dd5descr :: Sentence
dd5descr = (QP.angularDisplacement ^. term) +:+ S "of a" +:+
  (rigidBody ^. term) +:+ S "as a function of" +:+ (QP.time ^. term) +:+ 
  ch QP.time +:+ sParen (Sy (unit_symb QP.time)) `sC`
  S "also equal to the derivative of its" +:+ (QM.orientation ^. term) +:+
  S "with respect to" +:+ (QP.time ^. term) +:+ ch QP.time
-}
-- DD6 : Angular velocity --

angVelDD :: DataDefinition
angVelDD = ddNoRefs angVel Nothing "angVel" [rigidTwoDAssump]

angVel :: QDefinition
angVel = mkQuantDef QP.angularVelocity angVelEqn

angVelEqn :: Expr
angVelEqn = deriv (apply1 QP.angularDisplacement QP.time) QP.time
{-
dd6descr :: Sentence
dd6descr = ((QP.angularVelocity ^. term)) +:+ S "of a" +:+
  (rigidBody ^. term) +:+ S "as a function of" +:+ (QP.time ^. term) +:+ 
  ch QP.time +:+ sParen (Sy (unit_symb QP.time)) `sC`
  S "also equal to the derivative of its" +:+ (QP.angularDisplacement ^. term) +:+
  S "with respect to" +:+ (QP.time ^. term) +:+ ch QP.time
-}
-- DD7 : Angular acceleration --
-----------------------------------DD8 Angular Acceleration-------------------
angAccelDD :: DataDefinition
angAccelDD = ddNoRefs angAccel Nothing "angAccel" [rigidTwoDAssump]

angAccel :: QDefinition
angAccel = mkQuantDef QP.angularAccel angAccelEqn

angAccelEqn :: Expr
angAccelEqn = deriv (apply1 QP.angularVelocity QP.time) QP.time
{-
dd7descr :: Sentence
dd7descr = (QP.angularAccel ^. term) +:+ S "of a" +:+
  (rigidBody ^. term) +:+ S "as a function of" +:+ (QP.time ^. term) +:+
  ch QP.time +:+ sParen (Sy (unit_symb QP.time)) `sC`
  S "also equal to the derivative of its" +:+ ((QP.angularVelocity ^. term)) +:+
  S "with respect to" +:+ (QP.time ^. term) +:+ ch QP.time
-}
-- DD8 : Impulse for collision response --

-- Currently a super crude implementation requiring lots of custom chunks;
-- need norms and cross products

-------------------------DD8 Impulse for Collision-------------------------------
--moved to GenDefs.hs

------------------------DD9 Chasles Theorem----------------------------------
chaslesDD :: DataDefinition
chaslesDD = dd chasles [ref chaslesWiki] Nothing "chaslesThm" 
  [chaslesThmNote, rigidBodyAssump]

chasles :: QDefinition
chasles = mkQuantDef' velB (nounPhraseSP "Chasles' theorem") chaslesEqn

-- The last two terms in the denominator should be cross products.
chaslesEqn :: Expr
chaslesEqn = sy velO `addRe` cross (sy  QP.angularVelocity) (sy rOB)

chaslesThmNote :: Sentence
chaslesThmNote = foldlSent [atStartNP (the QP.linearVelocity),
  ch velB `S.of_` S "any point B in a", phrase rigidBody `S.isThe` S "sum" `S.ofThe`
  phrase QP.linearVelocity +:+ ch velO `S.ofThe` phrase rigidBody,
  S "at the origin (axis of rotation)" `S.andThe` S "resultant vector from",
  S "cross product" `S.the_ofThe` phrasePoss rigidBody,
  getTandS QP.angularVelocity `S.andThe` getTandS rOB]

---------------DD10 Impulse(Vector)-----------------------------------------------------------------------
impulseVDD :: DataDefinition
impulseVDD = ddNoRefs impulseV (Just impulseVDeriv) "impulseV"
  [impulseVDesc, rigidBodyAssump]

impulseV :: QDefinition
impulseV = mkQuantDef QP.impulseV impulseVEqn

impulseVEqn :: Expr
impulseVEqn = sy QPP.mass `mulRe` sy QP.chgInVelocity

impulseVDesc :: Sentence
impulseVDesc = foldlSent [S "An", getTandS QP.impulseV, S "occurs when a",
  getTandS QP.force, S "acts over a body over an interval" `S.of_` phrase QP.time]

impulseVDeriv :: Derivation
impulseVDeriv = mkDerivName (phrase QP.impulseV) (weave [impulseVDerivSentences, map eS impulseVDerivEqns])

impulseVDerivSentences :: [Sentence]
impulseVDerivSentences = map foldlSentCol [impulseVDerivSentence1, 
 impulseVDerivSentence2, impulseVDerivSentence3]  

impulseVDerivSentence1 :: [Sentence]
impulseVDerivSentence1 = [S "Newton's second law of motion states"]

impulseVDerivSentence2 :: [Sentence]
impulseVDerivSentence2 = [S "Rearranging"] 

impulseVDerivSentence3 :: [Sentence]
impulseVDerivSentence3 = [S "Integrating the right hand side"] 

impulseVDerivEqn1 :: Expr
impulseVDerivEqn1 =  sy QP.force $= sy QPP.mass `mulRe` sy QP.acceleration
                     $= sy QPP.mass `mulRe` deriv (sy QP.velocity) QP.time

impulseVDerivEqn2 :: Expr
impulseVDerivEqn2 = defint (eqSymb timeT) (sy time_1) (sy time_2) (sy QP.force) $=
                    sy QPP.mass `mulRe` defint (eqSymb QP.velocity) (sy velo_1) (sy velo_2) (exactDbl 1)


impulseVDerivEqn3 :: Expr
impulseVDerivEqn3 = defint (eqSymb timeT) (sy time_1) (sy time_2) (sy QP.force)
                    $= (sy QPP.mass `mulRe` sy velo_2) $- (sy QPP.mass `mulRe` sy velo_1) 
                    $= sy QPP.mass `mulRe` sy QP.chgInVelocity
                                      
impulseVDerivEqns :: [Expr]
impulseVDerivEqns = [impulseVDerivEqn1, impulseVDerivEqn2, impulseVDerivEqn3]

-----------------DD11 Relative Velocity in Collision------------------------------------------------------- 
reVelInCollDD :: DataDefinition
reVelInCollDD = ddNoRefs reVelInColl Nothing "reVeInColl"
  [reVelInCollDesc, rigidBodyAssump]

reVelInColl :: QDefinition
reVelInColl = mkQuantDef initRelVel reVelInCollEqn

reVelInCollEqn :: Expr
reVelInCollEqn = sy velAP $- sy velBP

reVelInCollDesc :: Sentence
reVelInCollDesc = foldlSent [S "In a collision, the", phraseNP (QP.velocity
  `ofA` rigidBody), S "A colliding with another", phrase rigidBody,
  S "B relative to that body", ch initRelVel `S.isThe` S "difference between the",
  plural QP.velocity, S "of A and B at point P"]
-----------------DD13 Torque-------------------------------------------------------------------------------

-- Imported from Theories.Physics

----------------------DD14 Coefficient of Restitution--------------------------
coeffRestitutionDD :: DataDefinition
coeffRestitutionDD = ddNoRefs coeffRestitution Nothing "coeffRestitution"
 [coeffRestitutionDesc]

coeffRestitution :: QDefinition
coeffRestitution = mkQuantDef QP.restitutionCoef coeffRestitutionEqn

coeffRestitutionEqn :: Expr
coeffRestitutionEqn = neg $ sy finRelVel $.
  sy normalVect $/ sy initRelVel $.
  sy normalVect

coeffRestitutionDesc :: Sentence
coeffRestitutionDesc = foldlSent [S "The", getTandS QP.restitutionCoef,
  S "determines the elasticity of a collision between two" +:+. plural rigidBody,
  foldlList Comma List [
  eS (sy QP.restitutionCoef $= exactDbl 1) +:+ S "results in an elastic collision",
  eS (sy QP.restitutionCoef $< exactDbl 1) +:+ S "results in an inelastic collision",
  eS (sy QP.restitutionCoef $= exactDbl 0) +:+ S "results in a totally inelastic collision"]]
-----------------------DD15 Kinetic Energy--------------------------------  
kEnergyDD :: DataDefinition
kEnergyDD = ddNoRefs kEnergy Nothing "kEnergy"
 [kEnergyDesc, rigidTwoDAssump, noDampingAssump] 

kEnergy :: QDefinition
kEnergy = mkQuantDef QP.kEnergy kEnergyEqn

kEnergyEqn :: Expr
kEnergyEqn = sy QPP.mass `mulRe` half (square (sy QP.velocity))

kEnergyDesc :: Sentence
kEnergyDesc = foldlSent [atStart QP.kEnergy `S.is` (QP.kEnergy ^. defn)]
-----------------------DD16 Moment Of Inertia--------------------------------------------------------

momentOfInertiaDD :: DataDefinition
momentOfInertiaDD = ddNoRefs momentOfInertia Nothing "momentOfInertia"
 [momentOfInertiaDesc, rigidBodyAssump] 

momentOfInertia :: QDefinition
momentOfInertia = mkQuantDef QP.momentOfInertia momentOfInertiaEqn

momentOfInertiaEqn :: Expr
momentOfInertiaEqn = sumAll (variable "j") $ sy massj `mulRe` square (sy rRot)

momentOfInertiaDesc :: Sentence
momentOfInertiaDesc = foldlSent [S "The", getTandS QP.momentOfInertia,
 S "of a body measures how much", phrase QP.torque,
 S "is needed for the body to achieve angular acceleration about the axis of rotation"]

---------------------------DD17 Potential Energy-------------------------------------------

potEnergyDD :: DataDefinition
potEnergyDD = ddNoRefs potEnergy Nothing "potEnergy"
 [potEnergyDesc, rigidTwoDAssump, noDampingAssump] 

potEnergy :: QDefinition
potEnergy = mkQuantDef QP.potEnergy potEnergyEqn

potEnergyEqn :: Expr
potEnergyEqn = sy QPP.mass `mulRe` sy QP.gravitationalAccel `mulRe` sy QP.height

potEnergyDesc :: Sentence
potEnergyDesc = foldlSent [atStartNP (the QP.potEnergy) `S.of_`
  S "an object" `S.isThe` phrase QP.energy, S "held by an object because of its",
  phrase QP.position, S "to other objects"]

---

collisionAssump, noDampingAssump, rightHandAssump, rigidBodyAssump, rigidTwoDAssump :: Sentence
collisionAssump = S "All collisions are vertex-to-edge" +:+. fromSource assumpCT
noDampingAssump = S "No damping occurs during the simulation" +:+. fromSource assumpDI
rightHandAssump = S "A" +:+ phrase rightHand `S.is` S "used" +:+. fromSource assumpAD
rigidBodyAssump = S "All bodies are assumed to be rigid" +:+. fromSource assumpOT
rigidTwoDAssump = foldlSent [S "All bodies are assumed to be rigid",
  fromSource assumpOT `S.and_` phrase twoD, fromSource assumpOD]

-- References --
dataDefRefs :: [Reference]
dataDefRefs = map ref dataDefs