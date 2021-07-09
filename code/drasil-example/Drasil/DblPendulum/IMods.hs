{-# LANGUAGE PostfixOperators #-}
module Drasil.DblPendulum.IMods (iMods, angularDisplacementIM, iModRefs) where

import Prelude hiding (cos, sin)

import Language.Drasil
import Theory.Drasil (InstanceModel, imNoRefs, qwC, ModelKinds (OthModel))
  --imNoDerivNoRefs, )
import Utils.Drasil
import Utils.Drasil.Concepts
import qualified Utils.Drasil.Sentence as S
import qualified Utils.Drasil.NounPhrase as NP
import Data.Drasil.Quantities.Physics (gravitationalAccel,
         angularAccel, momentOfInertia,
         time, angularDisplacement, angularFrequency, torque, angularDisplacement, time)
--import Data.Drasil.Theories.Physics (newtonSL)
import Data.Drasil.Quantities.PhysicalProperties (mass)
import Drasil.DblPendulum.Unitals (lenRod, pendDisplacementAngle, initialPendAngle)
import Data.Drasil.Concepts.Math (constraint, equation, amplitude, iAngle, angle)
import Data.Drasil.Concepts.Physics (pendulum, motion, shm)
import Data.Drasil.Theories.Physics (newtonSLR)
import Drasil.DblPendulum.GenDefs (angFrequencyGD)


iMods :: [InstanceModel]
iMods = [angularDisplacementIM]

---
angularDisplacementIM :: InstanceModel
angularDisplacementIM = imNoRefs (OthModel angularDisplacementRC)
  [qwC lenRod $ UpFrom (Exc, exactDbl 0)
  ,qwC initialPendAngle $ UpFrom (Exc, exactDbl 0)
  , qwC gravitationalAccel $ UpFrom (Exc, exactDbl 0)]
  (qw pendDisplacementAngle) [UpFrom (Exc, exactDbl 0)]
  (Just angularDisplacementDeriv) "calOfAngularDisplacement" [angularDispConstraintNote]


angularDisplacementRC :: RelationConcept
angularDisplacementRC = makeRC "angularDisplacementRC" (nounPhraseSP "calculation of angular displacement")
  EmptyS $ apply1 pendDisplacementAngle time $= sy initialPendAngle `mulRe` cos ( sy angularFrequency `mulRe` sy time)


angularDisplacementDeriv :: Derivation
angularDisplacementDeriv = mkDerivName (phrase angularDisplacement) (weave [angularDisplacementDerivSents, map eS angularDisplacementDerivEqns])

angularDisplacementDerivSents :: [Sentence]
angularDisplacementDerivSents = [angularDisplacementDerivSent1, angularDisplacementDerivSent2, angularDisplacementDerivSent3,
                             angularDisplacementDerivSent4, angularDisplacementDerivSent5]

angularDisplacementDerivSent1, angularDisplacementDerivSent2, angularDisplacementDerivSent3,
  angularDisplacementDerivSent4, angularDisplacementDerivSent5 :: Sentence

angularDisplacementDerivSent1 = foldlSentCol [S "When", phraseNP (the pendulum) `S.is` S "displaced to an", phrase iAngle `S.and_` S "released" `sC`
                                       phraseNP (the pendulum), S "swings back and forth with periodic" +:+. phrase motion,
                                       S "By applying", namedRef newtonSLR (phrase newtonSLR) `sC`
                                       phraseNP (NP.the (equation `of_` motion) `NP.for` the pendulum), S "may be obtained"]
       
 
angularDisplacementDerivSent2 = foldlSentCol [S "Where", ch torque `S.denotes` phrase torque `sC`
                                    ch momentOfInertia `S.denotes` phrase momentOfInertia `S.and_` ch angularAccel `S.denotes`
                                    (phrase angularAccel !.), S "This implies"]


angularDisplacementDerivSent3 = foldlSentCol [S "And rearranged as" ]

angularDisplacementDerivSent4 = foldlSentCol [S "If", phraseNP (NP.the (amplitude `of_` angularDisplacement)), S "is small enough" `sC`
  S "we can approximate", eS (sin (sy pendDisplacementAngle) $= sy pendDisplacementAngle), S "for the purpose of a simple", phrase pendulum,
  S "at very small" +:+. plural angle,
  S "Then", phraseNP (NP.the (equation `of_` motion)), S "reduces to", phraseNP (NP.the (equation `of_` shm))]                                       

angularDisplacementDerivSent5 = foldlSentCol [S "Thus the", phrase shm, S "is" ]

angularDisplacementDerivEqns :: [Expr]
angularDisplacementDerivEqns = [angularDisplacementDerivEqn1, angularDisplacementDerivEqn2, angularDisplacementDerivEqn3,
                                 angularDisplacementDerivEqn4, angularDisplacementDerivEqn5]

angularDisplacementDerivEqn1, angularDisplacementDerivEqn2, angularDisplacementDerivEqn3,
 angularDisplacementDerivEqn4, angularDisplacementDerivEqn5 :: Expr

angularDisplacementDerivEqn1 = sy torque $= sy momentOfInertia `mulRe` sy angularAccel

angularDisplacementDerivEqn2 = neg (sy mass `mulRe` sy gravitationalAccel `mulRe` sin (sy pendDisplacementAngle) `mulRe` sy lenRod) $= (sy mass `mulRe` square (sy lenRod))
                                `mulRe` deriv (deriv (sy pendDisplacementAngle) time) time

angularDisplacementDerivEqn3 = deriv (deriv (sy pendDisplacementAngle) time) time `addRe` ((sy gravitationalAccel $/ sy lenRod) `mulRe` sin (sy pendDisplacementAngle)) $= exactDbl 0

angularDisplacementDerivEqn4 = deriv (deriv (sy pendDisplacementAngle) time) time `addRe` ((sy gravitationalAccel $/ sy lenRod) `mulRe` sy pendDisplacementAngle) $= exactDbl 0

angularDisplacementDerivEqn5 = apply1 pendDisplacementAngle time $= sy initialPendAngle `mulRe` cos ( sy angularFrequency `mulRe` sy time)

----------------------




-- Notes

angularDispConstraintNote :: Sentence


angularDispConstraintNote = foldlSent [atStartNP (the constraint),
     eS (sy initialPendAngle $> exactDbl 0) `S.is` (S "required" !.),
     atStartNP (the angularFrequency) `S.is` definedIn'' angFrequencyGD]

--gravitationalAccelConstNote, landAndTargPosConsNote, landPosNote,
--   landPosConsNote, offsetNote, offsetConsNote, targPosConsNote,
--   timeConsNote, tolNote :: Sentence

-- References -- 
iModRefs :: [Reference]
iModRefs = map ref iMods