module Drasil.GamePhysics.Unitals where

import Language.Drasil
import Language.Drasil.Display (Symbol(..))
import Language.Drasil.ShortHands
import Utils.Drasil.Concepts

import Data.Drasil.SI_Units(kilogram, metre, m_2, newton, second)
import qualified Data.Drasil.Concepts.Physics as CP (rigidBody)
import qualified Data.Drasil.Quantities.Physics as QP (acceleration, angularAccel,
  angularDisplacement, angularVelocity, chgInVelocity, chgMomentum, displacement, distance,
  final, force, gravitationalAccel, gravitationalConst, gravitationalConstValue,
  height, impulseS, impulseV, initial, kEnergy, linearAccel, linearDisplacement,
  linearVelocity, momentOfInertia, position, potEnergy, restitutionCoef, time,
  torque, velocity, fOfGravity, positionVec)

import qualified Data.Drasil.Quantities.Math as QM (euclidNorm, normalVect, 
  orientation, perpVect, pi_, unitVect)
import qualified Data.Drasil.Quantities.PhysicalProperties as QPP (len, mass)
import Data.Drasil.Units.Physics (accelU, angVelU, impulseU, momtInertU,
  torqueU, velU, angAccelU)

import Control.Lens((^.))
import Data.Drasil.Constraints (gtZeroConstr)

defSymbols :: [DefinedQuantityDict]
defSymbols = map dqdWr unitSymbs ++ map dqdWr inputConstraints ++
  map dqdWr outputConstraints

unitSymbs :: [UnitaryConceptDict]
unitSymbs = map ucw unitalChunks ++ map ucw [iVect, jVect, normalVect,
 force_1, force_2, forcej, mass_1, mass_2, 
  dispNorm, sqrDist, velA, velB, velO, rOB, angVelA, angVelB,
  posCM, massj, posj, accj, angAccj, mTot, velj, torquej, timeC, initRelVel, 
  massA, massB, massIRigidBody, normalLen, contDispA, contDispB, 
  perpLenA, momtInertA, perpLenB, momtInertB, timeT, inittime, 
  momtInertK, pointOfCollision, contDispK, collisionImpulse, velAP,
  velBP, time_1, time_2, velo_1, velo_2, rRot, mLarger, distMass, dVect]

----------------------
-- TABLE OF SYMBOLS --
----------------------

symbols, symbolsAll, inputSymbols, outputSymbols :: [QuantityDict]

symbolsAll = symbols ++ inputSymbols ++ outputSymbols

symbols = map qw unitalChunks ++ 
  map qw unitless ++ 
  map qw inputConstraints

inputSymbols = map qw [QP.position, QP.velocity, QP.force, QM.orientation, 
  QP.angularVelocity, QP.linearVelocity, QP.gravitationalConst, QPP.mass, 
  QPP.len, QP.momentOfInertia, QP.torque, QP.kEnergy, QP.chgInVelocity, QP.potEnergy, QP.fOfGravity, QP.positionVec] ++
  [qw QP.restitutionCoef]

outputSymbols = map qw [QP.position, QP.velocity, QM.orientation, 
  QP.angularVelocity, QP.chgMomentum, QP.chgInVelocity]


unitalChunks :: [UnitalChunk]
unitalChunks = [QP.acceleration, QP.angularAccel, QP.gravitationalAccel, 
  QP.impulseV, QP.impulseS, iVect, jVect, normalVect, QP.distance, QP.displacement, 
  QP.time, QP.angularDisplacement, posCM, posj, massj, mTot, accj, velj,
  QP.linearDisplacement, QP.linearVelocity, QP.linearAccel, initRelVel, normalLen,
  perpLenA, perpLenB, forcej, torquej, timeC, velA, velB, massA, massB,
  angVelA, angVelB, force_1, force_2, mass_1, mass_2, 
  dispNorm, sqrDist, velO, rOB, massIRigidBody, contDispA, contDispB, 
  momtInertA, momtInertB, timeT, inittime, momtInertK, pointOfCollision, 
  contDispK, collisionImpulse, QP.kEnergy, finRelVel, velAP, velBP, time_1, time_2, velo_1, velo_2,
  QP.chgInVelocity, QP.potEnergy, QP.height, rRot, mLarger, QP.fOfGravity, QP.positionVec, distMass, 
  dVect, QP.chgMomentum,QP.chgInVelocity, time_1, time_2, velo_1, velo_2]

-----------------------
-- PARAMETRIZED HACK --
-----------------------
--FIXME: parametrized hack
--FIXME: "A" is not being capitalized when it should be.
forceParam, massParam, timeParam :: String -> String -> Symbol -> UnitalChunk
forceParam n w s = ucs'
 (dccWDS ("force" ++ n) (cn $ "force exerted by the " ++ w ++ 
  " body (on another body)") (phrase QP.force)) 
  (sub (eqSymb QP.force) s) Real newton

massParam n w s = ucs'
 (dccWDS ("mass" ++ n) (cn $ "mass of the " ++ w ++ " body") 
  (phrase QPP.mass)) (sub (eqSymb QPP.mass) s) Real kilogram

timeParam n w s = ucs'
 (dccWDS ("time" ++ n) (cn $ "time at a point in " ++ w ++ " body ") 
  (phrase QP.time)) (sub (eqSymb QP.time) s) Real second

contParam :: String -> String -> Symbol -> Symbol -> UnitalChunk
contParam n m w s = ucs'
 (dccWDS ("r_" ++ n ++ m) contdispN (phrase QP.displacement))
  (sub (eqSymb QP.displacement) (Concat [w, s])) Real metre
  where contdispN = cn $ "displacement vector between the centre of mass of rigid body " ++
                         n ++ " and contact point " ++ m

angParam, momtParam, perpParam, rigidParam, velBodyParam, velParam :: String -> Symbol -> UnitalChunk

angParam n w = ucs'
 (dccWDS ("angular velocity" ++ n) (compoundPhrase'
  (cn $ n ++ " body's") (QP.angularVelocity ^. term))
  (phrase QP.angularVelocity)) (sub (eqSymb QP.angularVelocity) w) Real angVelU

momtParam n w = ucs'
 (dccWDS ("momentOfInertia" ++ n) (compoundPhrase'
  (QP.momentOfInertia ^. term) (cn $ "of rigid body " ++ n))
  (phrase QP.momentOfInertia)) (sub (eqSymb QP.momentOfInertia) w) Real momtInertU

perpParam n w = ucs'
 (dccWDS ("|| r_A" ++ n ++ " x n ||") 
  (compoundPhrase' (QPP.len `ofThe` QM.perpVect)
  (cn $ "to the contact displacement vector of rigid body " ++ n)) 
  (phrase QM.perpVect)) (Concat [label "||", w, label "*", --should be x for cross
  eqSymb QM.perpVect, label "||"]) Real metre

rigidParam n w = ucs'
 (dccWDS ("rig_mass" ++ n) (compoundPhrase' (QPP.mass ^. term)
  (cn $ "of rigid body " ++ n)) (phrase QPP.mass)) (sub (eqSymb QPP.mass) w) Real kilogram

velBodyParam n w = ucs'
 (dccWDS ("velocity" ++ n) (compoundPhrase' (QP.velocity ^. term)
  (cn $ "of the  " ++ n ++ " body")) (phrase QP.velocity)) (sub (eqSymb QP.velocity) w) Real velU

velParam n w = ucs'
 (dccWDS ("velocity" ++ n) (compoundPhrase' (QP.velocity ^. term)
  (cn $ "at point " ++ n)) (phrase QP.velocity)) (sub (eqSymb QP.velocity) w) Real velU

-----------------------
-- CHUNKS WITH UNITS --
-----------------------

iVect, jVect, normalVect, force_1, force_2, forcej, mass_1, mass_2, 
  dispNorm, sqrDist, velA, velB, velO, rOB, angVelA, angVelB,
  posCM, massj, posj, accj, angAccj, mTot, velj, torquej, timeC, initRelVel, 
  massA, massB, massIRigidBody, normalLen, contDispA, contDispB, 
  perpLenA, momtInertA, perpLenB, momtInertB, timeT, inittime, 
  momtInertK, pointOfCollision, contDispK, collisionImpulse, finRelVel,
  velAP, velBP, time_1, time_2, velo_1, velo_2, rRot, mLarger, distMass, dVect :: UnitalChunk

iVect = ucs' (dccWDS "unitVect" (compoundPhrase' (cn "horizontal")
               (QM.unitVect ^. term)) (phrase QM.unitVect)) 
               (eqSymb QM.unitVect) Real metre
jVect       = ucs' (dccWDS "unitVectJ" (compoundPhrase' (cn "vertical")
               (QM.unitVect ^. term)) (phrase QM.unitVect)) (vec $ hat lJ) Real metre
normalVect  = ucs' (dccWDS "normalVect" (nounPhraseSent (S "collision" +:+
                   phrase QM.normalVect)) (phrase QM.normalVect)) 
                   (eqSymb QM.normalVect) Real metre

dVect = ucs' (dccWDS "unitVect" 
          (cn "unit vector directed from the center of the large mass to the center of the smaller mass") 
                   (phrase QM.unitVect)) (vec (hat lD)) Real metre

dispNorm = ucs' (dccWDS "euclideanNormDisp" (cn "Euclidean norm of the distance between the center of mass of two bodies")
               (phrase QM.euclidNorm) ) (eqSymb QM.euclidNorm) Real metre

distMass = ucs' (dccWDS "distMass" (cn "distance between the center of mass of the rigid bodies") 
                 (phrase QP.distance)) (vec lD) Real metre

sqrDist = ucs' (dccWDS "euclideanNorm" (cn' "squared distance")
               (phrase QM.euclidNorm)) (sup (eqSymb QM.euclidNorm) 
               label2) Real m_2
             
rOB    = uc' "rOB" 
  (nounPhraseSP "displacement vector between the origin and point B")
  "FIXME: Define this or remove the need for definitions" 
  (sub (eqSymb QP.displacement) (Concat [lOrigin, lBodyB])) metre
  
posCM = ucs "p_CM" (nounPhraseSP "Center of Mass")
 --"mass-weighted average position of a rigid " ++
 -- "body's particles") 
  "FIXME: Define this or remove the need for definitions" 
  (sub (eqSymb QP.position) lCMass) Real metre

massj = ucs' (dccWDS "m_j" (compoundPhrase' (QPP.mass ^. term)
                (cn "of the j-th particle")) (phrase QPP.mass)) 
                (sub (eqSymb QPP.mass) lJ) Real kilogram

posj = ucs' (dccWDS "p_j" (compoundPhrase' (QP.position ^. term) 
               (cn "vector of the j-th particle")) (phrase QP.position))
               (sub (eqSymb QP.position) lJ) Real metre

accj = ucs' (dccWDS "accj" (compoundPhrase' (cn "j-th body's")
               (QP.acceleration ^. term)) (phrase QP.acceleration))
               (sub (eqSymb QP.acceleration) lJ) Real accelU

angAccj = ucs' (dccWDS "angAccj" (compoundPhrase' (cn "j-th body's")
               (QP.angularAccel ^. term)) (phrase QP.angularAccel))
               (sub (eqSymb QP.angularAccel) lJ) Real angAccelU

velj = ucs' (dccWDS "velj" (compoundPhrase' (QP.velocity ^. term) 
               (cn "of the j-th body's velocity")) (phrase QP.velocity))
               (sub (eqSymb QP.velocity) lJ) Real velU

torquej = ucs' (dccWDS "torquej" 
               (cn "torque applied to the j-th body")
               (phrase QP.torque)) (sub (eqSymb QP.torque) lJ) Real torqueU

mTot = ucs' (dccWDS "M_T" (compoundPhrase' (cn "total mass of the") 
                 (CP.rigidBody ^. term)) (phrase QPP.mass))
                 (sub (eqSymb QPP.mass) cT) Real kilogram

mLarger = ucs' (dccWDS "mLarger" (compoundPhrase' (cn "mass of the larger") 
                 (CP.rigidBody ^. term)) (phrase QPP.mass)) cM Real kilogram

timeC = ucs' (dccWDS "timeC" (cn "denotes the time at collision") 
                (phrase QP.time)) (sub (eqSymb QP.time) lColl) Real second

initRelVel = ucs' (dccWDS "v_i^AB" (compoundPhrase'
                 (compoundPhrase' (cn "initial relative") (QP.velocity ^. term))
                 (cn "between rigid bodies of A and B")) (phrase QP.velocity))
                 (sup (sub (eqSymb QP.velocity) QP.initial) (Concat [lBodyA, lBodyB])) Real velU

finRelVel = ucs' (dccWDS "v_f^AB" (compoundPhrase'
                 (compoundPhrase' (cn "final relative") (QP.velocity ^. term))
                 (cn "between rigid bodies of A and B")) (phrase QP.velocity))
                 (sup (sub (eqSymb QP.velocity) QP.final) (Concat [lBodyA, lBodyB])) Real velU

massIRigidBody = ucs' (dccWDS "massj" (compoundPhrase' (QPP.mass ^. term) 
                (cn "of the j-th rigid body")) (phrase QPP.mass)) 
                (sub (eqSymb QPP.mass) lJ) Real kilogram
normalLen = ucs' (dccWDS "length of the normal vector" (
                  QPP.len `ofThe` QM.normalVect) 
                  (phrase QM.normalVect))
                  (Concat [label "||", eqSymb QM.normalVect, label "||"]) Real metre

rRot = ucs' (dccWDS "r_j" (compoundPhrase' (QP.distance ^. term)
                (cn "between the j-th particle and the axis of rotation")) (phrase QP.distance)) 
                (sub (eqSymb QP.distance) lJ) Real metre

timeT = ucs' (dccWDS "t" (cn "point in time") (phrase QP.time))
                (eqSymb QP.time) Real second

inittime = ucs' (dccWDS "t_0" (cn "denotes the initial time") 
                (phrase QP.time)) (sub (eqSymb QP.time) label0) Real second

pointOfCollision = ucs' (dccWDS "point_c" (cn "point of collision") 
                 (S "point")) cP Real metre

collisionImpulse = ucs' (dccWDS "collisionImp" (compoundPhrase' 
                (cn "collision") (QP.impulseS ^. term)) (phrase QP.impulseS)) 
                (eqSymb QP.impulseS) Real impulseU

forcej = ucs' (dccWDS "forcej" (compoundPhrase' 
      (QP.force ^. term) (cn "applied to the j-th body at time t")) 
      (phrase QP.force)) (sub (eqSymb QP.force) lJ) Real newton

velAP = ucs' (dccWDS "v^AP" (compoundPhrase' (QP.velocity ^. term)
              (cn "of the point of collision P in body A")) 
              (phrase QP.velocity)) (sup (eqSymb QP.velocity)(Concat [lBodyA, lPoint])) Real velU
velBP = ucs' (dccWDS "v^BP" (compoundPhrase' (QP.velocity ^. term)
              (cn "of the point of collision P in body B")) 
              (phrase QP.velocity)) (sup (eqSymb QP.velocity)(Concat [lBodyB, lPoint])) Real velU

force_1    = forceParam "1" "first"  label1
force_2    = forceParam "2" "second" label2
mass_1     = massParam  "1" "first"  label1
mass_2     = massParam  "2" "second" label2
velA       = velParam   "A" lBodyA
velB       = velParam   "B" lBodyB
velO       = velParam   "origin" lOrigin
angVelA    = angParam   "A" lBodyA
angVelB    = angParam   "B" lBodyB
perpLenA   = perpParam  "A" $ eqSymb contDispA
perpLenB   = perpParam  "B" $ eqSymb contDispB
momtInertA = momtParam  "A" lBodyA
momtInertB = momtParam  "B" lBodyB
momtInertK = momtParam  "k" lK
contDispA  = contParam  "A" "P" lBodyA lPoint
contDispB  = contParam  "B" "P" lBodyB lPoint
contDispK  = contParam  "k" "P" lK     lPoint
massA      = rigidParam "A" lBodyA
massB      = rigidParam "B" lBodyB
velo_1     = velBodyParam  "first"  label1
velo_2     = velBodyParam  "second" label2
time_1     = timeParam "1" "first"  label1
time_2     = timeParam "2" "second" label2

label0, label1, label2, lBodyA, lBodyB, lCMass, lColl, lOrigin, lPoint :: Symbol
label0  = Integ 0
label1  = Integ 1
label2  = Integ 2
lBodyA  = label "A"
lBodyB  = label "B"
lCMass  = label "CM"
lColl   = label "c"
lOrigin = label "O"
lPoint  = label "P"

--------------------------
-- CHUNKS WITHOUT UNITS --
--------------------------

unitless :: [QuantityDict]
unitless = qw QM.pi_ : [numParticles]

numParticles :: QuantityDict
numParticles = vc "n" (nounPhraseSP "number of particles in a rigid body") lN Integer

-----------------------
-- CONSTRAINT CHUNKS --
-----------------------

lengthCons, massCons, mmntOfInCons, gravAccelCons, posCons, orientCons,
  angVeloCons, forceCons, torqueCons, veloCons, restCoefCons, veloOutCons,
  angVeloOutCons, orientOutCons, posOutCons  :: ConstrConcept

inputConstraints :: [UncertQ]
inputConstraints = map (`uq` defaultUncrt)
  [lengthCons, massCons, mmntOfInCons, gravAccelCons, orientCons,
  veloCons, angVeloCons, forceCons, torqueCons, restCoefCons, posCons]

outputConstraints :: [UncertQ]
outputConstraints = map (`uq` defaultUncrt) 
  [posOutCons, veloOutCons, orientOutCons, angVeloOutCons]

lengthCons     = constrained' QPP.len               [gtZeroConstr] (dbl 44.2)
massCons       = constrained' QPP.mass              [gtZeroConstr] (dbl 56.2)
mmntOfInCons   = constrained' QP.momentOfInertia    [gtZeroConstr] (dbl 74.5)
gravAccelCons  = constrained' QP.gravitationalConst [] (QP.gravitationalConstValue ^. defnExpr)
posCons        = constrained' QP.position           [] (dbl 0.412) --FIXME: should be (0.412, 0.502) vector
veloCons       = constrained' QP.velocity           [] (dbl 2.51)
orientCons     = constrained' QM.orientation        [sfwrc $ Bounded (Inc, exactDbl 0) (Inc, exactDbl 2 `mulRe` sy QM.pi_)] (half $ sy QM.pi_) -- physical constraint not needed space is radians
angVeloCons    = constrained' QP.angularVelocity    [] (dbl 2.1)
forceCons      = constrained' QP.force              [] (dbl 98.1)
torqueCons     = constrained' QP.torque             [] (exactDbl 200)
restCoefCons   = constrained' QP.restitutionCoef    [physc $ Bounded (Inc, exactDbl 0) (Inc, exactDbl 1)] (dbl 0.8)

posOutCons        = constrained' QP.position           [] (exactDbl 0)
veloOutCons       = constrained' QP.velocity           [] (exactDbl 0)
orientOutCons     = constrained' QM.orientation        [] (exactDbl 0)
angVeloOutCons    = constrained' QP.angularVelocity    [] (exactDbl 0)

---------------------
-- INSTANCE MODELS --
---------------------

---------------------
-- GOAL STATEMENTS --
---------------------

