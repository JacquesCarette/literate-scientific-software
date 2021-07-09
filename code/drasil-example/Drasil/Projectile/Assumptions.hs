{-# LANGUAGE PostfixOperators #-}
module Drasil.Projectile.Assumptions (accelYGravity, accelXZero, cartSyst,
  assumptions, constAccel, gravAccelValue, launchOrigin, pointMass, 
  posXDirection, targetXAxis, timeStartZero, twoDMotion, yAxisGravity, assumpRefs) where

import Language.Drasil
import Utils.Drasil
import Utils.Drasil.Concepts
import qualified Utils.Drasil.NounPhrase as NP
import qualified Utils.Drasil.Sentence as S

import qualified Drasil.DocLang.SRS as SRS (valsOfAuxCons)

import Data.Drasil.Concepts.Documentation (assumpDom, value, consVals)
import Data.Drasil.Concepts.Math (cartesian, xAxis, xDir, yAxis, yDir, direction, positive)
import Data.Drasil.Concepts.PhysicalProperties (mass)
import Data.Drasil.Concepts.Physics (acceleration, collision, distance, gravity, time, twoD)

import Drasil.Projectile.Concepts (launcher, projectile, target, projMotion)

assumptions :: [ConceptInstance]
assumptions = [twoDMotion, cartSyst, yAxisGravity, launchOrigin, targetXAxis, 
  posXDirection, constAccel, accelXZero, accelYGravity, neglectDrag, pointMass, 
  freeFlight, neglectCurv, timeStartZero, gravAccelValue]

twoDMotion, cartSyst, yAxisGravity, launchOrigin, targetXAxis,
  posXDirection, constAccel, accelXZero, accelYGravity, neglectDrag,
  pointMass, freeFlight, neglectCurv, timeStartZero, 
  gravAccelValue :: ConceptInstance
twoDMotion      = cic "twoDMotion"      twoDMotionDesc      "twoDMotion"      assumpDom
cartSyst        = cic "cartSyst"        cartSystDesc        "cartSyst"        assumpDom
yAxisGravity    = cic "yAxisGravity"    yAxisGravityDesc    "yAxisGravity"    assumpDom
launchOrigin    = cic "launchOrigin"    launchOriginDesc    "launchOrigin"    assumpDom
targetXAxis     = cic "targetXAxis"     targetXAxisDesc     "targetXAxis"     assumpDom
posXDirection   = cic "posXDirection"   posXDirectionDesc   "posXDirection"   assumpDom
constAccel      = cic "constAccel"      constAccelDesc      "constAccel"      assumpDom
accelXZero      = cic "accelXZero"      accelXZeroDesc      "accelXZero"      assumpDom
accelYGravity   = cic "accelYGravity"   accelYGravityDesc   "accelYGravity"   assumpDom
neglectDrag     = cic "neglectDrag"     neglectDragDesc     "neglectDrag"     assumpDom
pointMass       = cic "pointMass"       pointMassDesc       "pointMass"       assumpDom
freeFlight      = cic "freeFlight"      freeFlightDesc      "freeFlight"      assumpDom
neglectCurv     = cic "neglectCurv"     neglectCurvDesc     "neglectCurv"     assumpDom
timeStartZero   = cic "timeStartZero"   timeStartZeroDesc   "timeStartZero"   assumpDom
gravAccelValue  = cic "gravAccelValue"  gravAccelValueDesc  "gravAccelValue"  assumpDom

twoDMotionDesc :: Sentence
twoDMotionDesc = atStartNP (NP.the (projMotion `is` twoD)) +:+. sParen (getAcc twoD)

cartSystDesc :: Sentence
cartSystDesc = atStartNP (a_ cartesian) `S.is` S "used" +:+. fromSource neglectCurv

yAxisGravityDesc :: Sentence
yAxisGravityDesc = atStartNP (direction `the_ofThe` yAxis) `S.is` S "directed opposite to" +:+. phrase gravity

launchOriginDesc :: Sentence
launchOriginDesc = (atStartNP (the launcher) `S.is` S "coincident with the origin" !.)

targetXAxisDesc :: Sentence
targetXAxisDesc = atStartNP (the target) +:+ S "lies on the" +:+ phrase xAxis +:+. fromSource neglectCurv

posXDirectionDesc :: Sentence
posXDirectionDesc = atStartNP (NP.the (combineNINI positive xDir)) `S.is` S "from the" +:+. phraseNP (launcher `toThe` target)

constAccelDesc :: Sentence
constAccelDesc = atStartNP (the acceleration) `S.is` S "constant" +:+.
                 fromSources [accelXZero, accelYGravity, neglectDrag, freeFlight]

accelXZeroDesc :: Sentence
accelXZeroDesc = atStartNP (NP.the (acceleration `inThe` xDir)) `S.is` (S "zero" !.)

accelYGravityDesc :: Sentence
accelYGravityDesc = atStartNP (NP.the (acceleration `inThe` yDir)) `S.isThe` phrase acceleration +:+
                    S "due to" +:+ phrase gravity +:+. fromSource yAxisGravity

neglectDragDesc :: Sentence
neglectDragDesc = (S "Air drag" `S.is` S "neglected" !.)

pointMassDesc :: Sentence
pointMassDesc = (S "size" `S.and_` S "shape") `S.the_ofTheC` phrase projectile `S.are`
                S "negligible" `sC` S "so that it can be modelled as a point" +:+. phrase mass

freeFlightDesc :: Sentence
freeFlightDesc = S "The flight" `S.is` S "free; there" `S.are` S "no" +:+ plural collision +:+
                 S "during" +:+. (S "trajectory" `S.the_ofThe` phrase projectile)

neglectCurvDesc :: Sentence
neglectCurvDesc = atStartNP (the distance) `S.is` S "small enough that" +:+.
                  (S "curvature" `S.the_ofThe` S "Earth can be neglected")

timeStartZeroDesc :: Sentence
timeStartZeroDesc = atStart time +:+. S "starts at zero"

gravAccelValueDesc :: Sentence
gravAccelValueDesc = atStartNP (the acceleration) +:+ S "due to" +:+
  phrase gravity +:+ S "is assumed to have the" +:+ phrase value +:+ 
  S "provided in the section for" +:+. namedRef (SRS.valsOfAuxCons [] []) (titleize consVals)

-- References --
assumpRefs :: [Reference]
assumpRefs = map ref assumptions
