module Drasil.Projectile.IMods (iMods, landPosIM, messageIM, offsetIM, timeIM, iModRefs) where

import Prelude hiding (cos, sin)

import Language.Drasil
import Theory.Drasil (InstanceModel, imNoDerivNoRefs, imNoRefs, qwC, equationalModelN)
import Utils.Drasil
import Utils.Drasil.Concepts
import qualified Utils.Drasil.Sentence as S

import qualified Drasil.DocLang.SRS as SRS (valsOfAuxCons)

import Data.Drasil.Concepts.Documentation (value)
import Data.Drasil.Concepts.Math (constraint, equation, xAxis)

import Data.Drasil.Quantities.Math (pi_)
import Data.Drasil.Quantities.Physics (gravitationalAccelConst, iSpeed, ixPos,
  ixVel, iyPos, iyVel, time, xConstAccel, xPos, yConstAccel, yPos)

import Drasil.Projectile.Assumptions (accelXZero, accelYGravity, gravAccelValue,
  launchOrigin, posXDirection, targetXAxis, timeStartZero, yAxisGravity)
import Drasil.Projectile.Concepts (projectile, target)
import Drasil.Projectile.DataDefs (speedIX, speedIY)
import qualified Drasil.Projectile.Expressions as E (flightDur', iyPos, yConstAccel,
  timeDerivEqn1, timeDerivEqn2, timeDerivEqn3, timeDerivEqn4, landPosExpr, iSpeed,
  landPosDerivEqn1, landPosDerivEqn2, landPosDerivEqn3,
  offset', message)
import Drasil.Projectile.Figures (figLaunch)
import Drasil.Projectile.GenDefs (posVecGD)
import Drasil.Projectile.Unitals (flightDur, landPos, launAngle, launSpeed,
  message, offset, targPos, tol)

iMods :: [InstanceModel]
iMods = [timeIM, landPosIM, offsetIM, messageIM]
---
timeIM :: InstanceModel
timeIM = imNoRefs (equationalModelN (nounPhraseSP "calculation of landing time") timeQD)
  [qwC launSpeed $ UpFrom (Exc, exactDbl 0)
  ,qwC launAngle $ Bounded (Exc, exactDbl 0) (Exc, half $ sy pi_)]
  (qw flightDur) [UpFrom (Exc, exactDbl 0)]
  (Just timeDeriv) "calOfLandingTime" [angleConstraintNote, gravitationalAccelConstNote, timeConsNote]

timeQD :: QDefinition 
timeQD =  mkQuantDef flightDur E.flightDur'

timeDeriv :: Derivation
timeDeriv = mkDerivName (phrase flightDur) (weave [timeDerivSents, map eS timeDerivEqns])

timeDerivSents :: [Sentence]
timeDerivSents = [timeDerivSent1, timeDerivSent2, timeDerivSent3, timeDerivSent4, timeDerivSent5]

timeDerivSent1, timeDerivSent2, timeDerivSent3, timeDerivSent4, timeDerivSent5 :: Sentence
timeDerivSent1 = foldlSentCol [S "We know that" +:+.
  foldlList Comma List
    [eqnWSource (sy iyPos $= E.iyPos) launchOrigin,
     eqnWSource (sy yConstAccel $= E.yConstAccel) accelYGravity],
  S "Substituting these", plural value, S "into the y-direction" `S.of_`
  refS posVecGD, S "gives us"]
timeDerivSent2 = foldlSentCol [S "To find the", phrase time, S "that the",
  phrase projectile, S "lands" `sC` S "we want to find the", ch time, phrase value,
  sParen (ch flightDur), S "where", eS (sy yPos $= exactDbl 0) +:+. sParen (S "since the" +:+
  phrase target `S.is` S "on the" +:+ phrase xAxis +:+ S "from" +:+ refS targetXAxis),
  S "From the", phrase equation, S "above we get"]
timeDerivSent3 = foldlSentCol [S "Dividing by", ch flightDur,
  sParen (S "with the" +:+ phrase constraint +:+ eS (sy flightDur $> exactDbl 0)),
  S "gives us"]
timeDerivSent4 = S "Solving for" +:+ ch flightDur +: S "gives us"
timeDerivSent5 = foldlSentCol [S "From", refS speedIY,
  sParen (S "with" +:+ eS (sy iSpeed $= E.iSpeed)), S "we can replace", ch iyVel]

timeDerivEqns :: [Expr]
timeDerivEqns = [E.timeDerivEqn1, E.timeDerivEqn2, E.timeDerivEqn3, E.timeDerivEqn4, sy flightDur $= E.flightDur']

---
landPosIM :: InstanceModel
landPosIM = imNoRefs (equationalModelN (nounPhraseSP "calculation of landing position") landPosQD)
  [qwC launSpeed $ UpFrom (Exc, exactDbl 0),
   qwC launAngle $ Bounded (Exc, exactDbl 0) (Exc, half $ sy pi_)]
  (qw landPos) [UpFrom (Exc, exactDbl 0)]
  (Just landPosDeriv) "calOfLandingDist" [angleConstraintNote, gravitationalAccelConstNote, landPosConsNote]

landPosQD :: QDefinition
landPosQD = mkQuantDef landPos E.landPosExpr

landPosDeriv :: Derivation
landPosDeriv = mkDerivName (phrase landPos) (weave [landPosDerivSents, map eS landPosDerivEqns])

landPosDerivSents :: [Sentence]
landPosDerivSents = [landPosDerivSent1, landPosDerivSent2, landPosDerivSent3, landPosDerivSent4]

landPosDerivSent1, landPosDerivSent2, landPosDerivSent3, landPosDerivSent4 :: Sentence
landPosDerivSent1 = foldlSentCol [S "We know that" +:+.
  foldlList Comma List
    [eqnWSource (sy ixPos $= exactDbl 0) launchOrigin,
     eqnWSource (sy xConstAccel $= exactDbl 0) accelXZero],
  S "Substituting these", plural value, S "into the x-direction" `S.of_`
  refS posVecGD, S "gives us"]
landPosDerivSent2 = foldlSentCol [S "To find the", phrase landPos `sC`
  S "we want to find the", ch xPos, phrase value, sParen (ch landPos),
  S "at", phrase flightDur, fromSource timeIM]
landPosDerivSent3 = foldlSentCol [S "From", refS speedIX,
  sParen (S "with" +:+ E (defines iSpeed launSpeed)), S "we can replace", ch ixVel]
landPosDerivSent4 = S "Rearranging this gives us the required" +: phrase equation

landPosDerivEqns :: [Expr]
landPosDerivEqns = [E.landPosDerivEqn1, E.landPosDerivEqn2, E.landPosDerivEqn3, sy landPos $= E.landPosExpr]

---
offsetIM :: InstanceModel
offsetIM = imNoDerivNoRefs (equationalModelN (nounPhraseSP "offset") offsetQD)
  [qwC landPos $ UpFrom (Exc, exactDbl 0)
  ,qwC targPos $ UpFrom (Exc, exactDbl 0)]
  (qw offset) [] "offsetIM" [landPosNote, landAndTargPosConsNote]

offsetQD :: QDefinition
offsetQD = mkQuantDef offset E.offset'
---
messageIM :: InstanceModel
messageIM = imNoDerivNoRefs (equationalModelN (nounPhraseSP "output message") messageQD)
  [qwC offset $ UpFrom (Exc, neg (sy landPos))
  ,qwC targPos $ UpFrom (Exc, exactDbl 0)]
  (qw message)
  [] "messageIM" [offsetNote, targPosConsNote, offsetConsNote, tolNote]

messageQD :: QDefinition 
messageQD = mkQuantDef message E.message

--- Notes

angleConstraintNote, gravitationalAccelConstNote, landAndTargPosConsNote, landPosNote,
  landPosConsNote, offsetNote, offsetConsNote, targPosConsNote,
  timeConsNote, tolNote :: Sentence

angleConstraintNote = foldlSent [atStartNP (the constraint),
  eS (realInterval launAngle (Bounded (Exc, exactDbl 0) (Exc, half $ sy pi_))) `S.is` S "from",
  refS posXDirection `S.and_` refS yAxisGravity `sC`
  S "and is shown" `S.in_` refS figLaunch]

gravitationalAccelConstNote = ch gravitationalAccelConst `S.is`
  S "defined in" +:+. refS gravAccelValue

landAndTargPosConsNote = atStartNP' (the constraint) +:+
  eS (sy landPos $> exactDbl 0) `S.and_` eS (sy targPos $> exactDbl 0) `S.are` S "from" +:+. refS posXDirection

landPosNote = ch landPos `S.is` S "from" +:+. refS landPosIM

landPosConsNote = atStartNP (the constraint) +:+
  eS (sy landPos $> exactDbl 0) `S.is` S "from" +:+. refS posXDirection

offsetNote = ch offset `S.is` S "from" +:+. refS offsetIM

offsetConsNote = foldlSent [atStartNP (the constraint), eS (sy offset $> neg (sy landPos)) `S.is`
  S "from the fact that", eS (sy landPos $> exactDbl 0) `sC` S "from", refS posXDirection]

targPosConsNote = atStartNP (the constraint) +:+
  eS (sy targPos $> exactDbl 0) `S.is` S "from" +:+. refS posXDirection

timeConsNote = atStartNP (the constraint) +:+
  eS (sy flightDur $> exactDbl 0) `S.is` S "from" +:+. refS timeStartZero

tolNote = ch tol `S.is` S "defined in" +:+. refS (SRS.valsOfAuxCons ([]::[Contents]) ([]::[Section]))

-- References -- 
iModRefs :: [Reference]
iModRefs = map ref iMods