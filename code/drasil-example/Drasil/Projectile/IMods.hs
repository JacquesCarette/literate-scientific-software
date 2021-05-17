module Drasil.Projectile.IMods (iMods, landPosIM, messageIM, offsetIM, timeIM) where

import Prelude hiding (cos, sin)

import Language.Drasil
import Theory.Drasil (InstanceModel, imNoDerivNoRefs, imNoRefs, qwC, ModelKinds (OthModel))
import Utils.Drasil
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
timeIM = imNoRefs (OthModel timeRC) 
  [qwC launSpeed $ UpFrom (Exc, dbl 0)
  ,qwC launAngle $ Bounded (Exc, dbl 0) (Exc, sy pi_ $/ dbl 2)]
  (qw flightDur) [UpFrom (Exc, dbl 0)]
  (Just timeDeriv) "calOfLandingTime" [angleConstraintNote, gravitationalAccelConstNote, timeConsNote]

timeRC :: RelationConcept
timeRC = makeRC "timeRC" (nounPhraseSP "calculation of landing time")
  EmptyS $ sy flightDur $= E.flightDur'

timeDeriv :: Derivation
timeDeriv = mkDerivName (phrase flightDur) (weave [timeDerivSents, map E timeDerivEqns])

timeDerivSents :: [Sentence]
timeDerivSents = [timeDerivSent1, timeDerivSent2, timeDerivSent3, timeDerivSent4, timeDerivSent5]

timeDerivSent1, timeDerivSent2, timeDerivSent3, timeDerivSent4, timeDerivSent5 :: Sentence
timeDerivSent1 = foldlSentCol [S "We know that" +:+.
  foldlList Comma List 
    [eqnWSource (sy iyPos $= E.iyPos) launchOrigin,
     eqnWSource (sy yConstAccel $= E.yConstAccel) accelYGravity],
  S "Substituting these", plural value, S "into the y-direction" `S.sOf`
  makeRef2S posVecGD, S "gives us"]
timeDerivSent2 = foldlSentCol [S "To find the", phrase time, S "that the",
  phrase projectile, S "lands" `sC` S "we want to find the", ch time, phrase value,
  sParen (ch flightDur), S "where", E (sy yPos $= int 0) +:+. sParen (S "since the" +:+
  phrase target `S.sIs` S "on the" +:+ phrase xAxis +:+ S "from" +:+ makeRef2S targetXAxis),
  S "From the", phrase equation, S "above",S "we get"]
timeDerivSent3 = foldlSentCol [S "Dividing by", ch flightDur,
  sParen (S "with the" +:+ phrase constraint +:+ E (sy flightDur $> dbl 0)),
  S "gives us"]
timeDerivSent4 = S "Solving for" +:+ ch flightDur +: S "gives us"
timeDerivSent5 = foldlSentCol [S "From", makeRef2S speedIY,
  sParen (S "with" +:+ E (sy iSpeed $= E.iSpeed)), S "we can replace", ch iyVel]

timeDerivEqns :: [Expr]
timeDerivEqns = [E.timeDerivEqn1, E.timeDerivEqn2, E.timeDerivEqn3, E.timeDerivEqn4, sy flightDur $= E.flightDur']

---
landPosIM :: InstanceModel
landPosIM = imNoRefs (OthModel landPosRC) 
  [qwC launSpeed $ UpFrom (Exc, dbl 0), 
   qwC launAngle $ Bounded (Exc, dbl 0) (Exc, sy pi_ $/ dbl 2)]
  (qw landPos) [UpFrom (Exc, dbl 0)]
  (Just landPosDeriv) "calOfLandingDist" [angleConstraintNote, gravitationalAccelConstNote, landPosConsNote]

landPosRC :: RelationConcept
landPosRC = makeRC "landPosRC" (nounPhraseSP "calculation of landing position")
  landPosConsNote (sy landPos $= E.landPosExpr)

landPosDeriv :: Derivation
landPosDeriv = mkDerivName (phrase landPos) (weave [landPosDerivSents, map E landPosDerivEqns])

landPosDerivSents :: [Sentence]
landPosDerivSents = [landPosDerivSent1, landPosDerivSent2, landPosDerivSent3, landPosDerivSent4]

landPosDerivSent1, landPosDerivSent2, landPosDerivSent3, landPosDerivSent4 :: Sentence
landPosDerivSent1 = foldlSentCol [S "We know that" +:+.
  foldlList Comma List 
    [eqnWSource (sy ixPos $= int 0) launchOrigin,
     eqnWSource (sy xConstAccel $= int 0) accelXZero],
  S "Substituting these", plural value, S "into the x-direction" `S.sOf`
  makeRef2S posVecGD, S "gives us"]
landPosDerivSent2 = foldlSentCol [S "To find the", phrase landPos `sC`
  S "we want to find the", ch xPos, phrase value, sParen (ch landPos),
  S "at", phrase flightDur, sParen (S "from" +:+ makeRef2S timeIM)]
landPosDerivSent3 = foldlSentCol [S "From", makeRef2S speedIX,
  sParen (S "with" +:+ E (sy iSpeed $= sy launSpeed)), S "we can replace", ch ixVel]
landPosDerivSent4 = S "Rearranging this gives us the required" +: phrase equation

landPosDerivEqns :: [Expr]
landPosDerivEqns = [E.landPosDerivEqn1, E.landPosDerivEqn2, E.landPosDerivEqn3, sy landPos $= E.landPosExpr]

---
offsetIM :: InstanceModel
offsetIM = imNoDerivNoRefs (OthModel offsetRC)
  [qwC landPos $ UpFrom (Exc, dbl 0)
  ,qwC targPos $ UpFrom (Exc, dbl 0)]
  (qw offset) [] "offsetIM" [landPosNote, landAndTargPosConsNote]

offsetRC :: RelationConcept
offsetRC = makeRC "offsetRC" (nounPhraseSP "offset") EmptyS $ sy offset $= E.offset'

---
messageIM :: InstanceModel
messageIM = imNoDerivNoRefs (OthModel messageRC) 
  [qwC offset $ UpFrom (Exc, neg (sy landPos))
  ,qwC targPos $ UpFrom (Exc, dbl 0)]
  (qw message)
  [] "messageIM" [offsetNote, targPosConsNote, offsetConsNote, tolNote]

messageRC :: RelationConcept
messageRC = makeRC "messageRC" (nounPhraseSP "output message") 
  EmptyS $ sy message $= E.message

--- Notes

angleConstraintNote, gravitationalAccelConstNote, landAndTargPosConsNote, landPosNote,
  landPosConsNote, offsetNote, offsetConsNote, targPosConsNote,
  timeConsNote, tolNote :: Sentence

angleConstraintNote = foldlSent [S "The", phrase constraint,
  E (int 0 $< sy launAngle $< (sy pi_ $/ int 2)) `S.sIs` S "from",
  makeRef2S posXDirection `S.sAnd` makeRef2S yAxisGravity `sC`
  S "and is shown" `S.sIn` makeRef2S figLaunch]

gravitationalAccelConstNote = ch gravitationalAccelConst `S.sIs`
  S "defined in" +:+. makeRef2S gravAccelValue

landAndTargPosConsNote = S "The" +:+ plural constraint +:+
  E (sy landPos $> int 0) `S.sAnd` E (sy targPos $> int 0) `S.sAre` S "from" +:+. makeRef2S posXDirection

landPosNote = ch landPos `S.sIs` S "from" +:+. makeRef2S landPosIM

landPosConsNote = S "The" +:+ phrase constraint +:+
  E (sy landPos $> int 0) `S.sIs` S "from" +:+. makeRef2S posXDirection

offsetNote = ch offset `S.sIs` S "from" +:+. makeRef2S offsetIM

offsetConsNote = foldlSent [S "The", phrase constraint, E (sy offset $> neg (sy landPos)) `S.sIs`
  S "from the fact that", E (sy landPos $> int 0) `sC` S "from", makeRef2S posXDirection]

targPosConsNote = S "The" +:+ phrase constraint +:+
  E (sy targPos $> int 0) `S.sIs` S "from" +:+. makeRef2S posXDirection

timeConsNote = S "The" +:+ phrase constraint +:+
  E (sy flightDur $> int 0) `S.sIs` S "from" +:+. makeRef2S timeStartZero

tolNote = ch tol `S.sIs` S "defined in" +:+. makeRef2S (SRS.valsOfAuxCons ([]::[Contents]) ([]::[Section]))
