{-# LANGUAGE PostfixOperators #-}
module Drasil.GamePhysics.IMods (iMods, instModIntro, iModRefs) where

import Language.Drasil
import Language.Drasil.ShortHands (lJ)
import Theory.Drasil
import Utils.Drasil
import Utils.Drasil.Concepts
import qualified Utils.Drasil.Sentence as S

import Drasil.GamePhysics.Assumptions (assumpDI, assumpCAJI)
import Drasil.GamePhysics.Concepts (centreMass)
import Drasil.GamePhysics.DataDefs (ctrOfMassDD, linDispDD, linVelDD, linAccDD,
  angDispDD, angVelDD, angAccelDD, collisionAssump, rightHandAssump,
  rigidTwoDAssump)
import Drasil.GamePhysics.Expressions
import Drasil.GamePhysics.GenDefs (accelGravityGD, impulseGD)
import Drasil.GamePhysics.Goals (linearGS, angularGS)
import Drasil.GamePhysics.TMods (newtonSL, newtonSLR)
import Drasil.GamePhysics.Unitals (accj, forcej, massA, massj, normalVect,
  timeC, torquej, velA, velj, angAccj)

import Data.Drasil.TheoryConcepts (inModel)

import Data.Drasil.Concepts.Documentation (condition, goal, output_)
import Data.Drasil.Concepts.Math (equation, ode)
import Data.Drasil.Concepts.Physics (rigidBody, motion)
import Data.Drasil.Quantities.Math (orientation)
import Data.Drasil.Quantities.Physics (acceleration, angularAccel, angularVelocity,
  force, gravitationalAccel, impulseS, momentOfInertia, position, time, velocity)

iMods :: [InstanceModel]
iMods = [transMot, rotMot, col2D]

{-- Force on the translational motion  --}
transMot :: InstanceModel
transMot = imNoRefs (equationalModel' transMotQD) 
  [ qwC velj               $ UpFrom (Exc, exactDbl 0)
  , qwC time               $ UpFrom (Exc, exactDbl 0)
  , qwC gravitationalAccel $ UpFrom (Exc, exactDbl 0)
  , qwC forcej             $ UpFrom (Exc, exactDbl 0)
  , qwC massj              $ UpFrom (Exc, exactDbl 0)
  ]
  (qw accj) [] (Just transMotDeriv)
  "transMot" [transMotDesc, transMotOutputs, rigidTwoDAssump, noDampConsAssumps]

transMotQD :: QDefinition
transMotQD = mkQuantDef' accj transMotNP transMotExpr

transMotNP :: NP
transMotNP = nounPhraseSP "force on the translational motion of a set of 2D rigid bodies"

transMotDesc, transMotOutputs :: Sentence
transMotDesc = foldlSent [S "The above", phrase equation, S "expresses the total",
  phraseNP (acceleration `ofThe` rigidBody), P lJ,
  S "as the sum" `S.of_` phrase gravitationalAccel, fromSource accelGravityGD `S.and_`
  phrase acceleration, S "due to applied", phrase force, eS (apply1 forcej time) +:+.
  fromSource newtonSL, S "The resultant", plural output_ `S.are`
  S "then obtained from this", phrase equation, S "using",
  foldlList Comma List (map refS [linDispDD, linVelDD, linAccDD])]
transMotOutputs = foldlSent [atStartNP (output_ `the_ofThe` inModel),
 S "will be the functions" `S.of_` phraseNP (position `and_` velocity),
 S "over time that satisfy the", getAcc ode `S.for` phraseNP (the acceleration) `sC`
 S "with the given initial", (plural condition `S.for` phraseNP (position `and_`
 velocity) !.), atStartNP (the motion), S "is translational" `sC` S "so the",
 phraseNP (position `and_` velocity), S "functions are for the",
 phrase centreMass, fromSource ctrOfMassDD]

transMotDeriv :: Derivation 
transMotDeriv = mkDerivName (phrase transMot)
      (weave [transMotDerivStmts, transMotDerivEqns])

transMotDerivStmts :: [Sentence]
transMotDerivStmts = [
    foldlSent [S "We may calculate the total acceleration of rigid body", 
      P lJ, S "by calculating the derivative of it's velocity with respect to time", fromSource linAccDD],
    S "Performing the derivative, we obtain:"
  ]

transMotDerivEqns :: [Sentence]
transMotDerivEqns = map eS [transMotExprDeriv1, toDispExpr transMotQD]

{-- Rotational Motion --}

rotMot :: InstanceModel
rotMot = imNoRefs (equationalModel' rotMotQD) 
  [ qwC angularVelocity $ UpFrom (Exc, exactDbl 0)
  , qwC time            $ UpFrom (Exc, exactDbl 0)
  , qwC torquej         $ UpFrom (Exc, exactDbl 0)
  , qwC momentOfInertia $ UpFrom (Exc, exactDbl 0)
  ]
  (qw angAccj) [UpFrom (Exc, exactDbl 0)] 
  (Just rotMotDeriv) "rotMot"
  [rotMotDesc, rigidTwoDAssump, rightHandAssump]

rotMotQD :: QDefinition
rotMotQD = mkQuantDef' angAccj rotMotNP rotMotExpr

rotMotNP :: NP
rotMotNP =  nounPhraseSP "Force on the rotational motion of a set of 2D rigid body"

rotMotDesc :: Sentence
rotMotDesc = foldlSent [S "The above", phrase equation, S "for the total",
  phraseNP (angularAccel `ofThe` rigidBody), P lJ `S.is`
  S "derived from", refS newtonSLR `sC` EmptyS `S.andThe` S "resultant",
  plural output_ `S.are` S "then obtained from this", phrase equation, S "using",
  foldlList Comma List (map refS [angDispDD, angVelDD, angAccelDD])]

rotMotDeriv :: Derivation 
rotMotDeriv = mkDerivName (phrase rotMot)
      (weave [rotMotDerivStmts, rotMotDerivEqns])

rotMotDerivStmts :: [Sentence]
rotMotDerivStmts = [
    foldlSent [S "We may calculate the total angular acceleration of rigid body", 
      P lJ, S "by calculating the derivative of its angular velocity with respect to time", fromSource angAccelDD],
    S "Performing the derivative, we obtain:"
  ]

rotMotDerivEqns :: [Sentence]
rotMotDerivEqns = map eS [rotMotExprDeriv1, toDispExpr rotMotQD]

{-- 2D Collision --}

col2D :: InstanceModel
col2D = imNoDerivNoRefs (othModel' col2DRC)
  [qwC time $ UpFrom (Exc, exactDbl 0)
  ,qwC impulseS $ UpFrom (Exc, exactDbl 0)
  ,qwC massA $ UpFrom (Exc, exactDbl 0)
  ,qwC normalVect $ UpFrom (Exc, exactDbl 0)
  ]
  -- why a constraint on velA if velA is not an output?
  -- (qw timeC) [sy velA $> 0, sy timeC $> 0] "col2D"
  (qw timeC) [UpFrom (Exc, exactDbl 0)] "col2D"
  [col2DOutputs, rigidTwoDAssump, rightHandAssump, collisionAssump,
    noDampConsAssumps, impulseNote]

col2DRC :: RelationConcept
col2DRC = makeRC "col2DRC" col2DNP EmptyS col2DRel

col2DNP :: NP
col2DNP =  nounPhraseSP "Collisions on 2D rigid bodies"

col2DRel {-, im3Rel2, im3Rel3, im3Rel4 -} :: Relation -- FIXME: add proper equation
col2DRel = apply1 velA timeC $= apply1 velA time `addRe`
  ((sy impulseS $/ sy massA) `mulRe` sy normalVect)


col2DOutputs, impulseNote :: Sentence
col2DOutputs = foldlSent [atStartNP (output_ `the_ofThe` inModel),
  S "will be the functions" `S.of_` vals,  S "over time that satisfy the",
  plural equation, S "for the", phraseNP (velocity `and_` angularAccel) `sC`
  S "with the given initial", plural condition, S "for" +:+. vals, atStartNP (the motion),
  S "is translational" `sC` S "so the", vals, S "functions are for the",
  phrase centreMass, fromSource ctrOfMassDD]
    where vals = foldlList Comma List (map phrase [position, velocity,
                                                   orientation, angularAccel])
impulseNote = ch impulseS `S.is` definedIn'' impulseGD

{--S "Ik is the moment of inertia of the k-th rigid body (kg m2)",
  S "t is a point in time, t0 denotes the initial time" `sC` 
  S "and tc denotes the time at collision (s)",
  S "P is the point of collision (m)"
--}

{--displaceVectBtw  = cvR (ddcWDS "dispBtwVect" (compoundPhrase' 
  (displacement ^. term) (cn "vector between the centre of mass of the k-th
  body and point P"))) (sub (displacement ^. symbol) ) 
--}

{- Intro -}

instModIntro :: Sentence
instModIntro = foldlSent [atStartNP (the goal), refS linearGS, 
  S "is met by" +:+. (refS transMot `S.and_` refS col2D),
  atStartNP (the goal), refS angularGS, S "is met by",
  refS rotMot `S.and_` refS col2D]

{- Notes -}
noDampConsAssumps :: Sentence
noDampConsAssumps = foldlSent [S "It is currently assumed that no damping",
  S "occurs during the simulation", fromSource assumpDI `S.and_` S "that no", 
  S "constraints are involved", fromSource assumpCAJI]

-- References -- 
iModRefs :: [Reference]
iModRefs = map ref iMods
