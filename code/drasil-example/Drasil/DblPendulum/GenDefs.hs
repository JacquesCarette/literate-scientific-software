{-# LANGUAGE PostfixOperators #-}
module Drasil.DblPendulum.GenDefs (genDefns, velocityIXGD, velocityIYGD,
         accelerationIXGD, accelerationIYGD, hForceOnPendulumGD, vForceOnPendulumGD,
         angFrequencyGD, periodPend, genDefRefs) where

import Prelude hiding (cos, sin, sqrt)
import qualified Data.List.NonEmpty as NE

import Language.Drasil
import Theory.Drasil (GenDefn, gdNoRefs,
    equationalModel', equationalModelU, equationalRealmU,
    MultiDefn, mkDefiningExpr, mkMultiDefnForQuant)
import Utils.Drasil
import Utils.Drasil.Concepts
import qualified Utils.Drasil.Sentence as S
import qualified Utils.Drasil.NounPhrase as NP

-- import Data.Drasil.Concepts.Documentation (coordinate, symbol_)
import Data.Drasil.Concepts.Math (xComp, yComp, equation, component, direction, angle)
import Data.Drasil.Quantities.Physics (velocity, acceleration, force,
    momentOfInertia, torque, angularDisplacement, angularFrequency,
    frequency, period, xAccel, xVel, yAccel, yVel)
import Data.Drasil.Concepts.Physics (pendulum, weight, shm)
import Data.Drasil.Quantities.PhysicalProperties (mass, len)
import Data.Drasil.Theories.Physics (newtonSLR)
import Drasil.DblPendulum.DataDefs (frequencyDD, periodSHMDD, angFrequencyDD)

-- import Drasil.Projectile.Assumptions (cartSyst, constAccel, pointMass, timeStartZero, twoDMotion)
import qualified Drasil.DblPendulum.Expressions as E
import Drasil.DblPendulum.Unitals (lenRod, pendDisplacementAngle)
import Drasil.DblPendulum.Concepts (arcLen, horizontalPos,
    verticalPos, horizontalVel, verticalVel, horizontalForce, verticalForce)

genDefns :: [GenDefn]
genDefns = [velocityIXGD, velocityIYGD, accelerationIXGD, accelerationIYGD,
       hForceOnPendulumGD, vForceOnPendulumGD, angFrequencyGD, periodPend]


---------------------
velocityIXGD :: GenDefn
velocityIXGD = gdNoRefs (equationalModel' velocityIXQD) (getUnit velocity)
           (Just velocityIXDeriv) "velocityIX" [{-Notes-}]

velocityIXQD :: QDefinition
velocityIXQD = mkQuantDef' xVel (the xComp `NP.of_` (velocity `ofThe` pendulum))
    E.velocityIXExpr

velocityIXDeriv :: Derivation
velocityIXDeriv = mkDerivName (phraseNP (NP.the (xComp `of_` velocity))) (weave [velocityIXDerivSents, velocityIXDerivEqns])

velocityIXDerivSents :: [Sentence]
velocityIXDerivSents = [velocityIDerivSent1,velocityIXDerivSent2,velocityIXDerivSent3,velocityIXDerivSent4,
                            velocityIXDerivSent5]

velocityIXDerivEqns :: [Sentence]
velocityIXDerivEqns = map eS [E.velocityIDerivEqn1, E.velocityIXDerivEqn2,
    E.velocityIXDerivEqn3, E.velocityIXDerivEqn4] ++ [eS velocityIXQD]

velocityIDerivSent1, velocityIXDerivSent2, velocityIXDerivSent3,
    velocityIXDerivSent4, velocityIXDerivSent5 :: Sentence


velocityIDerivSent1 = S "At a given point in time" `sC` phrase velocity +:+ S "may be defined as"
velocityIXDerivSent2 = S "We also know the" +:+ phrase horizontalPos
velocityIXDerivSent3 = S "Applying this,"
velocityIXDerivSent4 = eS lenRod `S.is` S "constant" `S.wrt` S  "time, so"
velocityIXDerivSent5 = S "Therefore, using the chain rule,"

---------------------
velocityIYGD :: GenDefn
velocityIYGD = gdNoRefs (equationalModel' velocityIYQD) (getUnit velocity)
           (Just velocityIYDeriv) "velocityIY" [{-Notes-}]

velocityIYQD :: QDefinition
velocityIYQD = mkQuantDef' yVel (the yComp `NP.of_` (velocity `ofThe` pendulum)) E.velocityIYExpr
 
velocityIYDeriv :: Derivation
velocityIYDeriv = mkDerivName (phraseNP (NP.the (yComp `of_` velocity))) (weave [velocityIYDerivSents, velocityIYDerivEqns])

velocityIYDerivSents :: [Sentence]
velocityIYDerivSents = [velocityIDerivSent1, velocityIYDerivSent2,
                        velocityIYDerivSent3, velocityIYDerivSent4,
                        velocityIYDerivSent5, velocityIYDerivSent5]

velocityIYDerivEqns :: [Sentence]
velocityIYDerivEqns = map eS [E.velocityIDerivEqn1, E.velocityIYDerivEqn2,
    E.velocityIYDerivEqn3, E.velocityIYDerivEqn4] ++ [eS velocityIYQD]

velocityIYDerivSent2,velocityIYDerivSent3,velocityIYDerivSent4,velocityIYDerivSent5 :: Sentence
velocityIYDerivSent2 = S "We also know the" +:+ phrase verticalPos
velocityIYDerivSent3 = S "Applying this again,"
velocityIYDerivSent4 = eS lenRod `S.is` S "constant" `S.wrt` S "time, so"
velocityIYDerivSent5 = S "Therefore, using the chain rule,"

-----------------------
accelerationIXGD :: GenDefn
accelerationIXGD = gdNoRefs (equationalModel' accelerationIXQD) (getUnit acceleration)
           (Just accelerationIXDeriv) "accelerationIX" [{-Notes-}]

accelerationIXQD :: QDefinition
accelerationIXQD = mkQuantDef' xAccel (the xComp `NP.of_` (acceleration `ofThe` pendulum))
    E.accelerationIXExpr

accelerationIXDeriv :: Derivation
accelerationIXDeriv = mkDerivName (phraseNP (NP.the (xComp `of_` acceleration))) (weave [accelerationIXDerivSents, accelerationIXDerivEqns])

accelerationIXDerivSents :: [Sentence]
accelerationIXDerivSents = [accelerationIDerivSent1, accelerationIXDerivSent2, accelerationIXDerivSent3,
    accelerationIXDerivSent4, accelerationIXDerivSent5]

accelerationIXDerivEqns :: [Sentence]
accelerationIXDerivEqns = eS E.accelerationIDerivEqn1 : eS velocityIXQD :
    map eS [E.accelerationIXDerivEqn3, E.accelerationIXDerivEqn4] ++ [eS accelerationIXQD]

accelerationIDerivSent1, accelerationIXDerivSent2, accelerationIXDerivSent3,
     accelerationIXDerivSent4, accelerationIXDerivSent5 :: Sentence

accelerationIDerivSent1 = S "Our" +:+ phrase acceleration +: S "is"
accelerationIXDerivSent2 = S "Earlier" `sC` S "we found the" +:+ phrase horizontalVel +:+ S "to be"
accelerationIXDerivSent3 = S "Applying this to our equation for" +:+ phrase acceleration
accelerationIXDerivSent4 = S "By the product and chain rules, we find"
accelerationIXDerivSent5 = S "Simplifying,"

-----------------------
accelerationIYGD :: GenDefn
accelerationIYGD = gdNoRefs (equationalModel' accelerationIYQD) (getUnit acceleration)
           (Just accelerationIYDeriv) "accelerationIY" [{-Notes-}]

accelerationIYQD :: QDefinition
accelerationIYQD = mkQuantDef' yAccel (the yComp `NP.of_` (acceleration `ofThe` pendulum)) E.accelerationIYExpr

accelerationIYDeriv :: Derivation
accelerationIYDeriv = mkDerivName (phraseNP (NP.the (yComp `of_` acceleration))) (weave [accelerationIYDerivSents, accelerationIYDerivEqns])

accelerationIYDerivSents :: [Sentence]
accelerationIYDerivSents = [accelerationIDerivSent1, accelerationIYDerivSent2, accelerationIYDerivSent3,
    accelerationIYDerivSent4, accelerationIYDerivSent5]

accelerationIYDerivEqns :: [Sentence]
accelerationIYDerivEqns = eS E.accelerationIDerivEqn1 : eS velocityIYQD :
    map eS [E.accelerationIYDerivEqn3, E.accelerationIYDerivEqn4] ++ [eS accelerationIYQD]

accelerationIYDerivSent2, accelerationIYDerivSent3, accelerationIYDerivSent4,
    accelerationIYDerivSent5 :: Sentence
accelerationIYDerivSent2 = S "Earlier" `sC` S "we found the" +:+ phrase verticalVel +:+ S "to be"
accelerationIYDerivSent3 = S "Applying this to our equation for" +:+ phrase acceleration
accelerationIYDerivSent4 = S "By the product and chain rules, we find"
accelerationIYDerivSent5 = S "Simplifying,"

-------------------------------------Horizontal force acting on the pendulum 
hForceOnPendulumGD :: GenDefn
hForceOnPendulumGD = gdNoRefs (equationalRealmU "hForceOnPendulum" hForceOnPendulumMD)
        (getUnit force) (Just hForceOnPendulumDeriv) "hForceOnPendulum" [{-Notes-}]

hForceOnPendulumMD :: MultiDefn
hForceOnPendulumMD = mkMultiDefnForQuant quant EmptyS defns
    where quant = mkQuant' "force" (horizontalForce `onThe` pendulum)
                    Nothing Real (symbol force) (getUnit force)
          defns = NE.fromList [
                    mkDefiningExpr "hForceOnPendulumViaComponent"
                      [] EmptyS E.hForceOnPendulumViaComponent,
                    mkDefiningExpr "hForceOnPendulumViaAngle"
                      [] EmptyS E.hForceOnPendulumViaAngle
                  ]

hForceOnPendulumDeriv :: Derivation
hForceOnPendulumDeriv = mkDerivName (phraseNP (force `onThe` pendulum)) [eS hForceOnPendulumMD]

----------------------------------------Vertical force acting on the pendulum 
vForceOnPendulumGD :: GenDefn
vForceOnPendulumGD = gdNoRefs (equationalRealmU "vForceOnPendulum" vForceOnPendulumMD)
        (getUnit force) (Just vForceOnPendulumDeriv) "vForceOnPendulum" [{-Notes-}]

vForceOnPendulumMD :: MultiDefn
vForceOnPendulumMD = mkMultiDefnForQuant quant EmptyS defns
    where quant = mkQuant' "force" (verticalForce `onThe` pendulum)
                    Nothing Real (symbol force) (getUnit force)
          defns = NE.fromList [
                    mkDefiningExpr "vForceOnPendulumViaComponent"
                      [] EmptyS E.vForceOnPendulumViaComponent ,
                    mkDefiningExpr "vForceOnPendulumViaAngle"    
                      [] EmptyS E.vForceOnPendulumViaAngle
                  ]

vForceOnPendulumDeriv :: Derivation
vForceOnPendulumDeriv = mkDerivName (phraseNP (force `onThe` pendulum)) [eS vForceOnPendulumMD]

--------------------------------------Angular Frequency Of Pendulum

angFrequencyGD :: GenDefn
angFrequencyGD = gdNoRefs (equationalModelU "angFrequencyGD" angFrequencyQD) (getUnit angularFrequency)
           (Just angFrequencyDeriv) "angFrequencyGD" [angFrequencyGDNotes]

angFrequencyQD :: QDefinition
angFrequencyQD = mkQuantDef' angularFrequency (angularFrequency `the_ofThe` pendulum) E.angFrequencyExpr

angFrequencyDeriv :: Derivation
angFrequencyDeriv = mkDerivName (phraseNP (angularFrequency `the_ofThe` pendulum)) (weave [angFrequencyDerivSents, map eS E.angFrequencyDerivEqns])


angFrequencyDerivSents :: [Sentence]
angFrequencyDerivSents = [angFrequencyDerivSent1, angFrequencyDerivSent2, angFrequencyDerivSent3,
                      angFrequencyDerivSent4, angFrequencyDerivSent5, angFrequencyDerivSent6, angFrequencyDerivSent7]

angFrequencyDerivSent1, angFrequencyDerivSent2, angFrequencyDerivSent3,
     angFrequencyDerivSent4, angFrequencyDerivSent5, angFrequencyDerivSent6, angFrequencyDerivSent7 :: Sentence
angFrequencyDerivSent1 = foldlSentCol [S "Consider the", phrase torque, S "on a", phrase pendulum +:+. definedIn'' newtonSLR,
                  S "The", phrase force, S "providing the restoring", phrase torque `S.is` phraseNP (the component `NP.of_`
                  (weight `ofThe` pendulum)), S "bob that acts along the" +:+. phrase arcLen,
                  (phrase torque `S.isThe` phrase len) `S.the_ofTheC` S "string", ch lenRod, S "multiplied by", phrase component
                  `S.the_ofThe` S "net", phrase force, S "that is perpendicular to", S "radius" `S.the_ofThe` (S "arc" !.),
                  S "The minus sign indicates the", phrase torque, S "acts in the opposite", phraseNP (direction `ofThe`angularDisplacement)]
angFrequencyDerivSent2 = S "So then"
angFrequencyDerivSent3 = S "Therefore,"
angFrequencyDerivSent4 = S "Substituting for" +:+ ch momentOfInertia
angFrequencyDerivSent5 = S "Crossing out" +:+ ch mass `S.and_` ch lenRod +:+ S "we have"
angFrequencyDerivSent6 = S "For small" +:+ plural angle `sC` S "we approximate" +:+ S "sin" +:+ ch pendDisplacementAngle +:+ S "to" +:+ ch pendDisplacementAngle
angFrequencyDerivSent7 = S "Because this" +:+ phrase equation `sC` S "has the same form as the" +:+ phraseNP (equation `for` shm) +:+. 
                        S "the solution is easy to find" +:+ S " The" +:+ phrase angularFrequency

angFrequencyGDNotes :: Sentence
angFrequencyGDNotes = S "The" +:+ phrase torque `S.is` definedIn'' newtonSLR  `S.and_` phrase frequency `S.is` definedIn frequencyDD

 -------------------------------- Period of Pendulum Motion 

periodPend :: GenDefn
periodPend = gdNoRefs (equationalModelU "periodPendGD" periodPendQD) (getUnit period)
           (Just periodPendDeriv) "periodPend" [periodPendNotes]

periodPendQD :: QDefinition
periodPendQD = mkQuantDef' period (NP.the (period `ofThe` pendulum)) E.periodPendExpr

periodPendDeriv :: Derivation
periodPendDeriv = mkDerivName (phraseNP (NP.the (period `ofThe` pendulum))) (weave [periodPendDerivSents, map eS E.periodPendDerivEqns])

periodPendDerivSents :: [Sentence]
periodPendDerivSents = [periodPendDerivSent1, periodPendDerivSent2]

periodPendDerivSent1, periodPendDerivSent2 :: Sentence
periodPendDerivSent1 = atStartNP (period `the_ofThe` pendulum) +:+ S "can be defined from" +:+
                refS angFrequencyGD +:+ phrase equation
periodPendDerivSent2 =  S "Therefore from the" +:+ phrase equation +:+ refS angFrequencyDD `sC` S "we have"

periodPendNotes :: Sentence
periodPendNotes = atStartNP (NP.the (frequency `and_` period)) +:+ S "are defined in" +:+ refS frequencyDD +:+
        refS periodSHMDD +:+ S "respectively"

-- References --
genDefRefs :: [Reference]
genDefRefs = map ref genDefns
