{-# LANGUAGE PostfixOperators #-}
module Drasil.Projectile.GenDefs (genDefns, posVecGD, genDefRefs) where

import Prelude hiding (cos, sin)
import Language.Drasil
import Theory.Drasil (GenDefn, TheoryModel, gd, gdNoRefs, ModelKinds(EquationalModel))
import Utils.Drasil
import Utils.Drasil.Concepts
import qualified Utils.Drasil.Sentence as S

import Data.Drasil.Concepts.Documentation (coordinate, symbol_)
import Data.Drasil.Concepts.Math (cartesian, equation, vector)
import Data.Drasil.Concepts.Physics (oneD, rectilinear, twoD, motion)

import Data.Drasil.Quantities.Physics (acceleration, constAccelV, iPos, iSpeed,
  iVel, ixVel, iyVel, position, scalarAccel, scalarPos,
  time, velocity, positionVec, speed)
import qualified Data.Drasil.Quantities.Physics as QP (constAccel)
import Data.Drasil.Theories.Physics (accelerationTM, velocityTM)

import Drasil.Projectile.Assumptions (cartSyst, constAccel, pointMass, timeStartZero, twoDMotion)
import Drasil.Projectile.Concepts (rectVel)
import qualified Drasil.Projectile.Expressions as E (speed', rectVelDerivEqn1, rectVelDerivEqn2,
  scalarPos', rectPosDerivEqn1, rectPosDerivEqn2, rectPosDerivEqn3, velVecExpr, posVecExpr,
  positionXY, velocityXY, accelerationXY, constAccelXY)
import Drasil.Projectile.References (hibbeler2004)
import Drasil.Projectile.Unitals (projSpeed)

genDefns :: [GenDefn]
genDefns = [rectVelGD, rectPosGD, velVecGD, posVecGD]

----------
rectVelGD :: GenDefn
rectVelGD = gd (EquationalModel rectVelQD) (getUnit projSpeed) (Just rectVelDeriv)
  [dRefInfo hibbeler2004 $ Page [8]] "rectVel" [{-Notes-}]

rectVelQD :: QDefinition 
rectVelQD = mkQuantDef' projSpeed (nounPhraseSent $ foldlSent_ 
            [atStart rectilinear, sParen $ getAcc oneD, phrase velocity,
             S "as a function" `S.of_` phraseNP (time `for` QP.constAccel)])
            E.speed'

rectVelDeriv :: Derivation
rectVelDeriv = mkDerivName (phrase rectVel)
               (weave [rectVelDerivSents, rectVelDerivEqns])

rectVelDerivSents :: [Sentence]
rectVelDerivSents = [rectDeriv velocity acceleration motSent iVel accelerationTM, rearrAndIntSent, performIntSent]
  where
    motSent = foldlSent [atStartNP (the motion) `S.in_` refS accelerationTM `S.is` S "now", phrase oneD,
                         S "with a", phrase QP.constAccel `sC` S "represented by", eS QP.constAccel]

rectVelDerivEqns :: [Sentence]
rectVelDerivEqns = map eS [E.rectVelDerivEqn1, E.rectVelDerivEqn2]
                ++ [eS rectVelQD]

----------
rectPosGD :: GenDefn
rectPosGD = gd (EquationalModel rectPosQD) (getUnit scalarPos) (Just rectPosDeriv)
  [dRefInfo hibbeler2004 $ Page [8]] "rectPos" [{-Notes-}]

rectPosQD :: QDefinition
rectPosQD = mkQuantDef' scalarPos (nounPhraseSent $ foldlSent_ 
            [atStart rectilinear, sParen $ getAcc oneD, phrase position,
             S "as a function" `S.of_` phraseNP (time `for` QP.constAccel)])
            E.scalarPos'

rectPosDeriv :: Derivation
rectPosDeriv = mkDerivName (phrase rectilinear +:+ phrase position)
               (weave [rectPosDerivSents, map eS rectPosDerivEqns])

rectPosDerivSents :: [Sentence]
rectPosDerivSents = [rectDeriv position velocity motSent iPos velocityTM,
  rearrAndIntSent, fromReplace rectVelGD speed, performIntSent]
    where
      motSent = atStartNP (the motion) `S.in_` refS velocityTM `S.is` S "now" +:+. phrase oneD

rectPosDerivEqns :: [Expr]
rectPosDerivEqns = [E.rectPosDerivEqn1, E.rectPosDerivEqn2, E.rectPosDerivEqn3, sy scalarPos $= E.scalarPos']

----------
velVecGD :: GenDefn
velVecGD = gdNoRefs (EquationalModel velVecQD) (getUnit velocity)
           (Just velVecDeriv) "velVec" [{-Notes-}]

velVecQD :: QDefinition 
velVecQD = mkQuantDef' velocity (nounPhraseSent $ foldlSent_ 
           [atStart velocity, S "vector as a function" `S.of_` phrase time `S.for`
            getAcc twoD, S "motion under", phrase QP.constAccel]) E.velVecExpr

velVecDeriv :: Derivation
velVecDeriv = mkDerivName (phrase velocity +:+ phrase vector) [velVecDerivSent, 
  E $ defines velocity E.velVecExpr]

velVecDerivSent :: Sentence
velVecDerivSent = vecDeriv [(velocity, E.velocityXY), (acceleration, E.accelerationXY)] rectVelGD

----------
posVecGD :: GenDefn
posVecGD = gdNoRefs (EquationalModel posVecQD) (getUnit position) 
           (Just posVecDeriv) "posVec" [{-Notes-}]

posVecQD :: QDefinition
posVecQD = mkQuantDef' position (nounPhraseSent $ foldlSent_ 
           [atStart position, S "vector as a function" `S.of_` phrase time `S.for`
            getAcc twoD, S "motion under", phrase QP.constAccel]) E.posVecExpr

posVecDeriv :: Derivation
posVecDeriv = mkDerivName (phrase positionVec) [posVecDerivSent, eS posVecQD]

posVecDerivSent :: Sentence
posVecDerivSent =
  vecDeriv [(position, E.positionXY), (velocity, E.velocityXY), (acceleration, E.accelerationXY)] rectPosGD

-- Helper for making rectilinear derivations
rectDeriv :: UnitalChunk -> UnitalChunk -> Sentence -> UnitalChunk -> TheoryModel -> Sentence
rectDeriv c1 c2 motSent initc ctm = foldlSent_ [
  S "Assume we have", phraseNP (combineNINI rectilinear motion) `S.ofA` S "particle",
  sParen (S "of negligible size" `S.and_` S "shape" `sC` S "from" +:+ refS pointMass) :+:
  S ";" +:+. (S "that is" `sC` S "motion" `S.in_` S "a straight line"),
  (atStartNP (the c1) `S.is` getScalar c1 `S.andThe` phrase c2 `S.is` getScalar c2 !.), motSent,
  atStartNP (the initc), sParen (S "at" +:+ eS (sy time $= exactDbl 0) `sC` S "from" +:+
  refS timeStartZero) `S.is` S "represented by" +:+. getScalar initc,
  S "From", refS ctm `sC` S "using the above", plural symbol_ +: S "we have"]
  where
    getScalar c
      | c == position     = eS scalarPos
      | c == velocity     = eS speed
      | c == acceleration = eS scalarAccel
      | c == iPos         = eS iPos
      | c == iVel         = eS iSpeed
      | otherwise         = error "Not implemented in getScalar"

rearrAndIntSent, performIntSent :: Sentence
rearrAndIntSent = S "Rearranging" `S.and_` S "integrating" `sC` S "we" +: S "have"
performIntSent  = S "Performing the integration" `sC` S "we have the required" +: phrase equation

-- Helper for making vector derivations
vecDeriv :: [(UnitalChunk, Expr)] -> GenDefn -> Sentence
vecDeriv vecs gdef = foldlSentCol [
  S "For a", phraseNP (combineNINI twoD cartesian), sParen (refS twoDMotion `S.and_` refS cartSyst) `sC`
  S "we can represent" +:+. foldlList Comma List 
  (map (\(c, e) -> foldlSent_ [phraseNP (the c), phrase vector, S "as", eS e]) vecs),
  atStartNP (the acceleration) `S.is` S "assumed to be constant", sParen (refS constAccel) `S.andThe`
  phrase constAccelV `S.is` S "represented as" +:+. eS E.constAccelXY, 
  atStartNP (the iVel) +:+ sParen (S "at" +:+ eS (sy time $= exactDbl 0) `sC` S "from" +:+ refS timeStartZero) `S.is`
  S "represented by" +:+. eS (sy iVel $= vec2D (sy ixVel) (sy iyVel)), 
  S "Since we have a",
  phrase cartesian `sC` refS gdef, S "can be applied to each", phraseNP (coordinate `ofThe`
  (fst . head) vecs), phrase vector, S "to yield the required", phrase equation]

-- References --
genDefRefs :: [Reference]
genDefRefs = map ref genDefns
