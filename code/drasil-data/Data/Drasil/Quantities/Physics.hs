module Data.Drasil.Quantities.Physics where

import Language.Drasil
import Language.Drasil.Display
import Language.Drasil.ShortHands
import qualified Data.Drasil.Concepts.Physics as CP (acceleration, angAccel,
  angDisp, angVelo, chgInVelocity, constAccel, constAccelV, displacement,
  distance, energy, fSpeed, fVel, force, frequency, gravitationalAccel, gravitationalConst,
  height, iPos, iSpeed, iVel, impulseS, impulseV, ixPos, ixVel, iyPos, iyVel,
  kEnergy, linAccel, linDisp, linVelo, momentOfInertia, position, potEnergy,
  pressure, restitutionCoef, scalarAccel, scalarPos, speed, time, torque,
  velocity, weight, xAccel, xConstAccel, xDist, xPos, xVel, yAccel, yConstAccel,
  yDist, yPos, yVel, momentum, moment, fOfGravity, positionVec, tension, angFreq, period, frequency, chgMomentum)

import Data.Drasil.SI_Units (joule, metre, newton, pascal, radian, second, hertz)
import Data.Drasil.Units.Physics (accelU, angAccelU, angVelU, gravConstU, 
    impulseU, momtInertU, torqueU, velU)

restitutionCoef :: DefinedQuantityDict
restitutionCoef = dqdNoUnit CP.restitutionCoef (sub cC (label "R")) Real

physicscon :: [UnitalChunk]
physicscon = [acceleration, angularAccel, angularDisplacement, angularVelocity,
  chgInVelocity, constAccel, constAccelV, displacement, distance, energy, frequency,
  fSpeed, fVel, force, gravitationalAccel, gravitationalConst, height, iPos,
  iSpeed, iVel, impulseS, impulseV, ixPos, ixVel, iyPos, iyVel, kEnergy,
  linearAccel, linearDisplacement, linearVelocity, momentOfInertia, position,
  potEnergy, pressure, scalarAccel, scalarPos, speed, time, torque, velocity,
  weight, xAccel, xConstAccel, xDist, xPos, xVel, yAccel, yConstAccel, yDist,
  yPos, yVel,momentum, moment, moment2D, fOfGravity, positionVec, tension,
  angularFrequency, period, frequency, chgMomentum]

acceleration, angularAccel, angularDisplacement, angularVelocity, chgInVelocity,
  constAccel, constAccelV, displacement, distance, energy, fSpeed, fVel, force,
  gravitationalAccel, gravitationalConst, height, iPos, iSpeed, iVel, impulseS,
  impulseV, ixPos, ixVel, iyPos, iyVel, kEnergy, linearAccel, linearDisplacement,
  linearVelocity, momentOfInertia, position, potEnergy, pressure, scalarAccel,
  scalarPos, speed, time, torque, velocity, weight, xAccel, xConstAccel, xDist,
  xPos, xVel, yAccel, yConstAccel, yDist, yPos, yVel, momentum, moment, moment2D,
  fOfGravity, positionVec, tension, angularFrequency, period, frequency, chgMomentum :: UnitalChunk


acceleration         = uc CP.acceleration (vec lA) accelU
angularAccel         = uc CP.angAccel lAlpha angAccelU
angularDisplacement  = uc CP.angDisp lTheta radian
angularFrequency     = uc CP.angFreq cOmega second
angularVelocity      = uc CP.angVelo lOmega angVelU
chgInVelocity        = uc CP.chgInVelocity (Concat [cDelta, vec lV]) velU
constAccel           = uc CP.constAccel (sup lA lC) accelU
displacement         = uc CP.displacement (vec lU) metre
distance             = uc CP.distance lD metre
energy               = uc CP.energy cE joule
force                = uc CP.force (vec cF) newton
frequency            = uc CP.frequency lF hertz
gravitationalAccel   = uc CP.gravitationalAccel (vec lG) accelU
gravitationalConst   = uc CP.gravitationalConst cG gravConstU
height               = uc CP.height lH metre
impulseS             = uc CP.impulseS lJ impulseU
impulseV             = uc CP.impulseV (vec cJ) impulseU
kEnergy              = uc CP.kEnergy  (Concat [cK, cE]) joule
linearAccel          = uc CP.linAccel (Concat [vec lA, label "(", lT, label ")"]) accelU
linearDisplacement   = uc CP.linDisp  (Concat [vec lU, label "(", lT, label ")"]) metre
linearVelocity       = uc CP.linVelo  (Concat [vec lV, label "(", lT, label ")"]) velU
momentOfInertia      = uc CP.momentOfInertia (vec cI) momtInertU
chgMomentum          = uc CP.chgMomentum (Concat [cDelta,vec cP]) impulseU
momentum             = uc CP.momentum (vec cP) impulseU
moment               = uc CP.moment   (vec cM) torqueU
moment2D             = uc CP.moment   cM       torqueU
-- FIXME: moment2D should eventually be a specialization of moment, not separately defined
period               = uc CP.period cT second
position             = uc CP.position (vec lP) metre
positionVec          = uc CP.positionVec (vec lR) metre
potEnergy            = uc CP.potEnergy (Concat [cP, cE]) joule
pressure             = uc CP.pressure lP pascal
speed                = uc CP.speed lV velU
scalarAccel          = uc CP.scalarAccel lA accelU
scalarPos            = uc CP.scalarPos lP metre
tension              = uc CP.tension (vec cT) newton
time                 = uc CP.time lT second
torque               = uc CP.torque (vec lTau) torqueU
velocity             = uc CP.velocity (vec lV) velU
weight               = uc CP.weight cW newton
fOfGravity           = uc CP.fOfGravity (sub (vec cF) (vec lG)) newton

-- Variants of distance, speed, and scalar acceleration
-- FIXME: Add variants of vector forms?
-- FIXME: Pull out commonalities?

xDist = uc CP.xDist (subX lR) metre
yDist = uc CP.yDist (subY lR) metre

iPos = uc CP.iPos (sup lP initial) metre
xPos = uc CP.xPos (subX lP) metre
yPos = uc CP.yPos (subY lP) metre

ixPos = uc CP.ixPos (sup (subX lP) initial) metre
iyPos = uc CP.iyPos (sup (subY lP) initial) metre

fSpeed = uc CP.fSpeed (sup lV final) velU
iSpeed = uc CP.iSpeed (sup lV initial) velU

fVel = uc CP.fVel (sup (vec lV) final) velU
iVel = uc CP.iVel (sup (vec lV) initial) velU
xVel = uc CP.xVel (subX lV) velU
yVel = uc CP.yVel (subY lV) velU

ixVel = uc CP.ixVel (sup (subX lV) initial) velU
iyVel = uc CP.iyVel (sup (subY lV) initial) velU

xAccel = uc CP.xAccel (subX lA) accelU
yAccel = uc CP.yAccel (subY lA) accelU

constAccelV = uc CP.constAccelV (sup (vec  lA) constant) accelU
xConstAccel = uc CP.xConstAccel (sup (subX lA) constant) accelU
yConstAccel = uc CP.yConstAccel (sup (subY lA) constant) accelU

constant, final, initial :: Symbol
constant = label "c"
final    = label "f"
initial  = label "i"

-- Helpers for common modifiers
subMax, subMin, subX, subY, subZ, supMax, supMin :: Symbol -> Symbol
subMax s = sub s (label "max")
subMin s = sub s (label "min")
subX   s = sub s (label "x")
subY   s = sub s (label "y")
subZ   s = sub s (label "z")
supMax s = sup s (label "max")
supMin s = sup s (label "min")

---------------Constants-----------------------------

gravitationalConstValue :: QDefinition
gravitationalConstValue = mkQuantDef gravitationalConst (dbl 6.6743E-11)
--(dbl 6.673 * 10E-11)
--(dbl 0.00000000006673)
gravitationalAccelConst :: QDefinition
gravitationalAccelConst = mkQuantDef gravitationalAccel (dbl 9.8)
