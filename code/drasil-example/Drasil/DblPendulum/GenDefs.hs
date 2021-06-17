{-# LANGUAGE PostfixOperators #-}
module Drasil.DblPendulum.GenDefs (genDefns, velocityIXGD, velocityIYGD,
         accelerationIXGD, accelerationIYGD, hForceOnPendulumGD, vForceOnPendulumGD,
         angFrequencyGD, periodPend, genDefRefs) where

import Prelude hiding (cos, sin, sqrt)
import qualified Data.List.NonEmpty as NE

import Language.Drasil
import Theory.Drasil (GenDefn, gdNoRefs, gdNoRefs',
    ModelKinds (EquationalModel, EquationalRealm),
    MultiDefn, mkDefiningExpr, mkMultiDefnForQuant)
import Utils.Drasil
import Utils.Drasil.Concepts
import qualified Utils.Drasil.Sentence as S
import qualified Utils.Drasil.NounPhrase as NP

-- import Data.Drasil.Concepts.Documentation (coordinate, symbol_)
import Data.Drasil.Concepts.Math (xComp, yComp, equation, component, direction, angle)
import Data.Drasil.Quantities.Physics(xPos, yPos, velocity, angularVelocity, xVel, yVel,
    angularAccel, xAccel, yAccel, acceleration, force, tension, gravitationalAccel,
    angularFrequency, torque, momentOfInertia, angularDisplacement, time,
    momentOfInertia, period, frequency, position)
import Data.Drasil.Concepts.Physics(pendulum, weight, shm)
import Data.Drasil.Quantities.PhysicalProperties(mass, len)
import Data.Drasil.Theories.Physics(newtonSLR)
import Drasil.DblPendulum.DataDefs(frequencyDD, periodSHMDD, angFrequencyDD)
import qualified Data.Drasil.Quantities.Math as QM (pi_)

-- import Drasil.Projectile.Assumptions (cartSyst, constAccel, pointMass, timeStartZero, twoDMotion)
import Drasil.DblPendulum.Unitals (lenRod, pendDisplacementAngle)
import Drasil.DblPendulum.Concepts (arcLen, horizontalPos,
    verticalPos, horizontalVel, verticalVel, horizontalForce, verticalForce)

genDefns :: [GenDefn]
genDefns = [velocityIXGD, velocityIYGD, accelerationIXGD, accelerationIYGD,
       hForceOnPendulumGD, vForceOnPendulumGD, angFrequencyGD, periodPend]


-- ----------
velocityIXGD :: GenDefn
velocityIXGD = gdNoRefs (EquationalModel velocityIXQD) (getUnit velocity)
           (Just velocityIXDeriv) "velocityIX" [{-Notes-}]

velocityIXQD :: QDefinition
velocityIXQD = mkQuantDef' xVel (the xComp `NP.of_` (velocity `ofThe` pendulum))
    velocityIXExpr

velocityIXExpr :: Expr
velocityIXExpr = sy angularVelocity `mulRe` sy lenRod `mulRe` cos (sy pendDisplacementAngle)

velocityIXDeriv :: Derivation
velocityIXDeriv = mkDerivName (phraseNP (NP.the (xComp `of_` velocity))) (weave [velocityIXDerivSents, map E velocityIXDerivEqns])

velocityIXDerivSents :: [Sentence]
velocityIXDerivSents = [velocityIDerivSent1,velocityIXDerivSent2,velocityIXDerivSent3,velocityIXDerivSent4,
                            velocityIXDerivSent5]

velocityIXDerivEqns :: [Expr]
velocityIXDerivEqns = [velocityIDerivEqn1,velocityIXDerivEqn2,velocityIXDerivEqn3,
                            velocityIXDerivEqn4, relat velocityIXQD]

velocityIDerivSent1,velocityIXDerivSent2,velocityIXDerivSent3,velocityIXDerivSent4,velocityIXDerivSent5 :: Sentence
velocityIDerivEqn1,velocityIXDerivEqn2,velocityIXDerivEqn3,velocityIXDerivEqn4 :: Expr

velocityIDerivSent1 = S "At a given point in time" `sC` phrase velocity +:+ S "may be defined as"
velocityIDerivEqn1 = sy velocity $= deriv (sy position) time
velocityIXDerivSent2 = S "We also know the" +:+ phrase horizontalPos
velocityIXDerivEqn2 = sy xPos $= sy lenRod `mulRe` sin (sy pendDisplacementAngle)
velocityIXDerivSent3 = S "Applying this,"
velocityIXDerivEqn3 = sy xVel $= deriv (sy lenRod `mulRe` sin (sy pendDisplacementAngle)) time
velocityIXDerivSent4 = E (sy lenRod) `S.is` S "constant" `S.wrt` S  "time, so"
velocityIXDerivEqn4 = sy xVel $= sy lenRod `mulRe` deriv (sin (sy pendDisplacementAngle)) time
velocityIXDerivSent5 = S "Therefore, using the chain rule,"

---------------------
velocityIYGD :: GenDefn
velocityIYGD = gdNoRefs (EquationalModel velocityIYQD) (getUnit velocity)
           (Just velocityIYDeriv) "velocityIY" [{-Notes-}]

velocityIYQD :: QDefinition
velocityIYQD = mkQuantDef' yVel (the yComp `NP.of_` (velocity `ofThe` pendulum)) velocityIYExpr
 
velocityIYExpr :: Expr
velocityIYExpr = sy angularVelocity `mulRe` sy lenRod `mulRe` sin (sy pendDisplacementAngle)

velocityIYDeriv :: Derivation
velocityIYDeriv = mkDerivName (phraseNP (NP.the (yComp `of_` velocity))) (weave [velocityIYDerivSents, map E velocityIYDerivEqns])

velocityIYDerivSents :: [Sentence]
velocityIYDerivSents = [velocityIDerivSent1, velocityIYDerivSent2,
                        velocityIYDerivSent3, velocityIYDerivSent4,
                        velocityIYDerivSent5, velocityIYDerivSent5]

velocityIYDerivEqns :: [Expr]
velocityIYDerivEqns = [velocityIDerivEqn1, velocityIYDerivEqn2,
                       velocityIYDerivEqn3, velocityIYDerivEqn4,
                       relat velocityIYQD]

velocityIYDerivSent2,velocityIYDerivSent3,velocityIYDerivSent4,velocityIYDerivSent5 :: Sentence
velocityIYDerivEqn2,velocityIYDerivEqn3,velocityIYDerivEqn4 :: Expr

velocityIYDerivSent2 = S "We also know the" +:+ phrase verticalPos
velocityIYDerivEqn2 = sy yPos $= neg (sy lenRod `mulRe` cos (sy pendDisplacementAngle))
velocityIYDerivSent3 = S "Applying this again,"
velocityIYDerivEqn3 = sy yVel $= neg (deriv (sy lenRod `mulRe` cos (sy pendDisplacementAngle)) time)
velocityIYDerivSent4 = E (sy lenRod) `S.is` S "constant" `S.wrt` S "time, so"
velocityIYDerivEqn4 = sy yVel $= neg (sy lenRod `mulRe` deriv (cos (sy pendDisplacementAngle)) time)
velocityIYDerivSent5 = S "Therefore, using the chain rule,"

-----------------------
accelerationIXGD :: GenDefn
accelerationIXGD = gdNoRefs (EquationalModel accelerationIXQD) (getUnit acceleration)
           (Just accelerationIXDeriv) "accelerationIX" [{-Notes-}]

accelerationIXQD :: QDefinition
accelerationIXQD = mkQuantDef' xAccel (the xComp `NP.of_` (acceleration `ofThe` pendulum)) accelerationIXExpr
 
accelerationIXExpr :: Expr
accelerationIXExpr = neg (square (sy angularVelocity) `mulRe` sy lenRod `mulRe` sin (sy pendDisplacementAngle))
                    `addRe` (sy angularAccel `mulRe` sy lenRod `mulRe` cos (sy pendDisplacementAngle))

accelerationIXDeriv :: Derivation
accelerationIXDeriv = mkDerivName (phraseNP (NP.the (xComp `of_` acceleration))) (weave [accelerationIXDerivSents, map E accelerationIXDerivEqns])

accelerationIXDerivSents :: [Sentence]
accelerationIXDerivSents = [accelerationIDerivSent1, accelerationIXDerivSent2, accelerationIXDerivSent3,
    accelerationIXDerivSent4, accelerationIXDerivSent5]

accelerationIXDerivEqns :: [Expr]
accelerationIXDerivEqns = [accelerationIDerivEqn1, accelerationIXDerivEqn2, accelerationIXDerivEqn3, accelerationIXDerivEqn4, relat accelerationIXQD]

accelerationIDerivSent1, accelerationIXDerivSent2, accelerationIXDerivSent3,
     accelerationIXDerivSent4, accelerationIXDerivSent5 :: Sentence
accelerationIDerivEqn1, accelerationIXDerivEqn2, accelerationIXDerivEqn3, accelerationIXDerivEqn4 :: Expr

accelerationIDerivSent1 = S "Our" +:+ phrase acceleration +: S "is"
accelerationIDerivEqn1 = sy acceleration $= deriv (sy velocity) time 
accelerationIXDerivSent2 = S "Earlier" `sC` S "we found the" +:+ phrase horizontalVel +:+ S "to be"
accelerationIXDerivEqn2 = relat velocityIXQD
accelerationIXDerivSent3 = S "Applying this to our equation for" +:+ phrase acceleration
accelerationIXDerivEqn3 = sy xAccel $= deriv (sy angularVelocity `mulRe` sy lenRod `mulRe` cos (sy pendDisplacementAngle)) time
accelerationIXDerivSent4 = S "By the product and chain rules, we find"
accelerationIXDerivEqn4 = sy xAccel $= deriv (sy angularVelocity) time `mulRe` sy lenRod `mulRe` cos (sy pendDisplacementAngle)
                        $- (sy angularVelocity `mulRe` sy lenRod `mulRe` sin (sy pendDisplacementAngle) `mulRe` deriv (sy pendDisplacementAngle) time)
accelerationIXDerivSent5 = S "Simplifying,"

-----------------------
accelerationIYGD :: GenDefn
accelerationIYGD = gdNoRefs (EquationalModel accelerationIYQD) (getUnit acceleration)
           (Just accelerationIYDeriv) "accelerationIY" [{-Notes-}]

accelerationIYQD :: QDefinition
accelerationIYQD = mkQuantDef' yAccel (the yComp `NP.of_` (acceleration `ofThe` pendulum)) accelerationIYExpr

accelerationIYExpr :: Expr
accelerationIYExpr = (square (sy angularVelocity) `mulRe` sy lenRod `mulRe` cos (sy pendDisplacementAngle))
                    `addRe` (sy angularAccel `mulRe` sy lenRod `mulRe` sin (sy pendDisplacementAngle))

accelerationIYDeriv :: Derivation
accelerationIYDeriv = mkDerivName (phraseNP (NP.the (yComp `of_` acceleration))) (weave [accelerationIYDerivSents, map E accelerationIYDerivEqns])

accelerationIYDerivSents :: [Sentence]
accelerationIYDerivSents = [accelerationIDerivSent1, accelerationIYDerivSent2, accelerationIYDerivSent3,
    accelerationIYDerivSent4, accelerationIYDerivSent5]

accelerationIYDerivEqns :: [Expr]
accelerationIYDerivEqns = [accelerationIDerivEqn1, accelerationIYDerivEqn2, accelerationIYDerivEqn3, accelerationIYDerivEqn4, relat accelerationIYQD]

accelerationIYDerivSent2, accelerationIYDerivSent3, accelerationIYDerivSent4,
    accelerationIYDerivSent5 :: Sentence
accelerationIYDerivEqn2, accelerationIYDerivEqn3, accelerationIYDerivEqn4 :: Expr

accelerationIYDerivSent2 = S "Earlier" `sC` S "we found the" +:+ phrase verticalVel +:+ S "to be"
accelerationIYDerivEqn2 = relat velocityIYQD
accelerationIYDerivSent3 = S "Applying this to our equation for" +:+ phrase acceleration
accelerationIYDerivEqn3 = sy yAccel $= deriv (sy angularVelocity `mulRe` sy lenRod `mulRe` sin (sy pendDisplacementAngle)) time
accelerationIYDerivSent4 = S "By the product and chain rules, we find"
accelerationIYDerivEqn4 = sy yAccel $= deriv (sy angularVelocity) time `mulRe` sy lenRod `mulRe` sin (sy pendDisplacementAngle)
                        `addRe` (sy angularVelocity `mulRe` sy lenRod `mulRe` cos (sy pendDisplacementAngle) `mulRe` deriv (sy pendDisplacementAngle) time)
accelerationIYDerivSent5 = S "Simplifying,"

-------------------------------------Horizontal force acting on the pendulum 
hForceOnPendulumGD :: GenDefn
hForceOnPendulumGD = gdNoRefs' "hForceOnPendulum" (EquationalRealm hForceOnPendulumMD)
        (getUnit force) (Just hForceOnPendulumDeriv) "hForceOnPendulum" [{-Notes-}]

hForceOnPendulumMD :: MultiDefn
hForceOnPendulumMD = mkMultiDefnForQuant quant EmptyS defns
    where quant = mkQuant' "force" (horizontalForce `onThe` pendulum)
                    Nothing Real (symbol force) (getUnit force)
          defns = NE.fromList [
                    mkDefiningExpr "hForceOnPendulumViaComponent" [] EmptyS $
                        sy mass `mulRe` sy xAccel,
                    mkDefiningExpr "hForceOnPendulumViaAngle"     [] EmptyS $
                        neg (sy tension `mulRe` sin (sy pendDisplacementAngle))
                  ]

hForceOnPendulumDeriv :: Derivation
hForceOnPendulumDeriv = mkDerivName (phraseNP (force `onThe` pendulum)) [E $ relat hForceOnPendulumMD]

----------------------------------------Vertical force acting on the pendulum 
vForceOnPendulumGD :: GenDefn
vForceOnPendulumGD = gdNoRefs' "vForceOnPendulum" (EquationalRealm vForceOnPendulumMD)
        (getUnit force) (Just vForceOnPendulumDeriv) "vForceOnPendulum" [{-Notes-}]

vForceOnPendulumMD :: MultiDefn
vForceOnPendulumMD = mkMultiDefnForQuant quant EmptyS defns
    where quant = mkQuant' "force" (verticalForce `onThe` pendulum)
                    Nothing Real (symbol force) (getUnit force)
          defns = NE.fromList [
                    mkDefiningExpr "vForceOnPendulumViaComponent" [] EmptyS $
                        sy mass `mulRe` sy yAccel,
                    mkDefiningExpr "vForceOnPendulumViaAngle"     [] EmptyS $
                        sy tension `mulRe` cos (sy pendDisplacementAngle)
                            $- (sy mass `mulRe` sy gravitationalAccel)
                  ]

vForceOnPendulumDeriv :: Derivation
vForceOnPendulumDeriv = mkDerivName (phraseNP (force `onThe` pendulum)) [E $ relat vForceOnPendulumMD]

--------------------------------------Angular Frequency Of Pendulum

angFrequencyGD :: GenDefn
angFrequencyGD = gdNoRefs' "angFrequencyGD" (EquationalModel angFrequencyQD) (getUnit angularFrequency)
           (Just angFrequencyDeriv) "angFrequencyGD" [angFrequencyGDNotes]

angFrequencyQD :: QDefinition
angFrequencyQD = mkQuantDef' angularFrequency (angularFrequency `the_ofThe` pendulum) angFrequencyExpr

angFrequencyExpr :: Expr
angFrequencyExpr = sqrt (sy gravitationalAccel $/ sy lenRod)

angFrequencyDeriv :: Derivation
angFrequencyDeriv = mkDerivName (phraseNP (angularFrequency `the_ofThe` pendulum)) (weave [angFrequencyDerivSents, map E angFrequencyDerivEqns])


angFrequencyDerivSents :: [Sentence]
angFrequencyDerivSents = [angFrequencyDerivSent1, angFrequencyDerivSent2, angFrequencyDerivSent3,
                      angFrequencyDerivSent4, angFrequencyDerivSent5, angFrequencyDerivSent6, angFrequencyDerivSent7]

angFrequencyDerivSent1, angFrequencyDerivSent2, angFrequencyDerivSent3,
     angFrequencyDerivSent4, angFrequencyDerivSent5, angFrequencyDerivSent6, angFrequencyDerivSent7 :: Sentence

angFrequencyDerivEqns :: [Expr]
angFrequencyDerivEqns = [angFrequencyDerivEqn1, angFrequencyDerivEqn2, angFrequencyDerivEqn3,
                     angFrequencyDerivEqn4, angFrequencyDerivEqn5, angFrequencyDerivEqn6, angFrequencyDerivEqn7]

angFrequencyDerivEqn1, angFrequencyDerivEqn2, angFrequencyDerivEqn3, angFrequencyDerivEqn4,
                   angFrequencyDerivEqn5, angFrequencyDerivEqn6, angFrequencyDerivEqn7 :: Expr

angFrequencyDerivSent1 = foldlSentCol [S "Consider the", phrase torque, S "on a", phrase pendulum +:+. definedIn'' newtonSLR,
                  S "The", phrase force, S "providing the restoring", phrase torque `S.is` phraseNP (the component `NP.of_`
                  (weight `ofThe` pendulum)), S "bob that acts along the" +:+. phrase arcLen,
                  (phrase torque `S.isThe` phrase len) `S.the_ofTheC` S "string", ch lenRod, S "multiplied by", phrase component
                  `S.the_ofThe` S "net", phrase force, S "that is perpendicular to", S "radius" `S.the_ofThe` (S "arc" !.),
                  S "The minus sign indicates the", phrase torque, S "acts in the opposite", phraseNP (direction `ofThe`angularDisplacement)]


angFrequencyDerivEqn1 = sy torque $= neg (sy lenRod) `mulRe` (sy mass `mulRe` sy gravitationalAccel `mulRe` sin (sy pendDisplacementAngle))
angFrequencyDerivSent2 = S "So then"
angFrequencyDerivEqn2 = sy momentOfInertia `mulRe` sy angularAccel $= neg (sy lenRod) `mulRe` (sy mass `mulRe` sy gravitationalAccel `mulRe` sin (sy pendDisplacementAngle))
angFrequencyDerivSent3 = S "Therefore,"
angFrequencyDerivEqn3 = sy momentOfInertia `mulRe` deriv (deriv (sy pendDisplacementAngle) time) time $= neg (sy lenRod)
             `mulRe` sy mass `mulRe` sy gravitationalAccel `mulRe` sin (sy pendDisplacementAngle)
angFrequencyDerivSent4 = S "Substituting for" +:+ ch momentOfInertia
angFrequencyDerivEqn4 = (sy mass `mulRe` square (sy lenRod)) `mulRe` deriv (deriv (sy pendDisplacementAngle) time) time $= neg (sy lenRod)
             `mulRe` sy mass `mulRe` sy gravitationalAccel `mulRe` sin (sy pendDisplacementAngle)
angFrequencyDerivSent5 = S "Crossing out" +:+ ch mass `S.and_` ch lenRod +:+ S "we have"
angFrequencyDerivEqn5 = deriv (deriv (sy pendDisplacementAngle) time) time $= neg (sy gravitationalAccel $/ sy lenRod) `mulRe` sin (sy pendDisplacementAngle)
angFrequencyDerivSent6 = S "For small" +:+ plural angle `sC` S "we approximate" +:+ S "sin" +:+ ch pendDisplacementAngle +:+ S "to" +:+ ch pendDisplacementAngle
angFrequencyDerivEqn6 = deriv (deriv (sy pendDisplacementAngle) time) time $= neg (sy gravitationalAccel $/ sy lenRod) `mulRe` sy pendDisplacementAngle
angFrequencyDerivSent7 = S "Because this" +:+ phrase equation `sC` S "has the same form as the" +:+ phraseNP (equation `for` shm) +:+. 
                        S "the solution is easy to find" +:+ S " The" +:+ phrase angularFrequency
angFrequencyDerivEqn7 = sy angularFrequency $= sqrt (sy gravitationalAccel $/ sy lenRod)
angFrequencyGDNotes :: Sentence
angFrequencyGDNotes = S "The" +:+ phrase torque `S.is` definedIn'' newtonSLR  `S.and_` phrase frequency `S.is` definedIn frequencyDD



 --------------------------------Period of Pendulum Motion 

periodPend :: GenDefn
periodPend = gdNoRefs' "periodPendGD" (EquationalModel periodPendQD) (getUnit period)
           (Just periodPendDeriv) "periodPend" [periodPendNotes]

periodPendQD :: QDefinition
periodPendQD = mkQuantDef' period (NP.the (period `ofThe` pendulum)) periodPendExpr

periodPendExpr :: Expr
periodPendExpr = exactDbl 2 `mulRe` sy QM.pi_ `mulRe` sqrt (sy lenRod $/ sy gravitationalAccel)

periodPendDeriv :: Derivation
periodPendDeriv = mkDerivName (phraseNP (NP.the (period `ofThe` pendulum))) (weave [periodPendDerivSents, map E periodPendDerivEqns])

periodPendDerivSents :: [Sentence]
periodPendDerivSents = [periodPendDerivSent1, periodPendDerivSent2]

periodPendDerivSent1, periodPendDerivSent2 :: Sentence

periodPendDerivEqns :: [Expr]
periodPendDerivEqns = [periodPendDerivEqn1, periodPendDerivEqn2]

periodPendDerivEqn1, periodPendDerivEqn2 :: Expr

periodPendDerivSent1 = atStartNP (period `the_ofThe` pendulum) +:+ S "can be defined from" +:+
                makeRef2S angFrequencyGD +:+ phrase equation
periodPendDerivEqn1 = sy angularFrequency $= sqrt (sy gravitationalAccel $/ sy lenRod)
periodPendDerivSent2 =  S "Therefore from the" +:+ phrase equation +:+ makeRef2S angFrequencyDD `sC` S "we have"

periodPendDerivEqn2 = sy period $= exactDbl 2 `mulRe` sy QM.pi_ `mulRe` sqrt (sy lenRod $/ sy gravitationalAccel)

periodPendNotes :: Sentence
periodPendNotes = atStartNP (NP.the (frequency `and_` period)) +:+ S "are defined in" +:+ makeRef2S frequencyDD +:+
        makeRef2S periodSHMDD +:+ S "respectively"

-- References --
genDefRefs :: [Reference]
genDefRefs = map rw genDefns