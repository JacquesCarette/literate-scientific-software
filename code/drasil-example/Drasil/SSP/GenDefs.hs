{-# LANGUAGE PostfixOperators #-}
module Drasil.SSP.GenDefs (normForcEq, bsShrFEq, resShr, mobShr,
  normShrR, momentEql, generalDefinitions,
  normForcEqGD, bsShrFEqGD, resShrGD, mobShrGD, normShrRGD, momentEqlGD,
  mobShearWOGD, resShearWOGD, srfWtrFGD, genDefRefs) where

import Control.Lens ((^.))
import Prelude hiding (sin, cos, tan)
import qualified Data.List.NonEmpty as NE
import Language.Drasil
import Theory.Drasil (GenDefn, gd, ModelKinds(EquationalConstraints, OthModel, EquationalModel), mkConstraintSet, )
import Utils.Drasil
import Utils.Drasil.Concepts
import qualified Utils.Drasil.NounPhrase as NP
import qualified Utils.Drasil.Sentence as S
import Drasil.DocLang.SRS as SRS (physSyst)
import Data.Drasil.SI_Units (metre, newton)
import Data.Drasil.Concepts.Documentation (analysis, assumption, component,
  constant, definition, method_, value)
import Data.Drasil.Concepts.Math (area, equation, zDir)
import Data.Drasil.Concepts.PhysicalProperties (len)
import Data.Drasil.Concepts.Physics (twoD, weight)
import Data.Drasil.Concepts.SolidMechanics (normForce, shearForce)
import Data.Drasil.Quantities.PhysicalProperties (specWeight)
import Data.Drasil.Quantities.Physics (displacement, force, height,
  pressure, torque)
import Data.Drasil.Theories.Physics (weightGD, hsPressureGD, torqueDD)
import Drasil.SSP.Assumptions (assumpFOSL, assumpSLH, assumpSP, assumpSLI,
  assumpINSFL, assumpPSC, assumpSBSBISL, assumpWIBE, assumpWISE, assumpNESSS,
  assumpHFSM)
import Drasil.SSP.BasicExprs (eqlExpr, eqlExprN, momExpr)
import Drasil.SSP.DataDefs (intersliceWtrF, angleA, angleB, lengthB, lengthLb,
  lengthLs, slcHeight, normStressDD, tangStressDD, ratioVariation)
import Drasil.SSP.Defs (intrslce, slice, slope, slopeSrf, slpSrf, soil,
  soilPrpty, waterTable)
import Drasil.SSP.Figures (figForceActing)
import Drasil.SSP.References (chen2005, fredlund1977, karchewski2012)
import Drasil.SSP.TMods (factOfSafety, equilibrium, mcShrStrgth, effStress)
import Drasil.SSP.Unitals (baseAngle, baseHydroForce, baseLngth, baseWthX,
  dryWeight, earthqkLoadFctr, effCohesion, fricAngle, fs, genericA, genericM,
  genericSpWght, impLoadAngle, intNormForce, intShrForce, index, inxi, inxiM1,
  midpntHght, mobShrI, momntArm, normToShear, nrmFSubWat, rotForce, satWeight,
  scalFunc, shearFNoIntsl, shrResI, shrStress, totNrmForce, shearRNoIntsl,
  slcWght, sliceHght, sliceHghtW, slipHght, slopeHght, surfHydroForce,
  surfAngle, surfLngth, surfLoad, watrForce, waterHght, waterWeight, dryVol,
  satVol, yi, zcoord)

---------------------------
--  General Definitions  --
---------------------------
generalDefinitions :: [GenDefn]
generalDefinitions = [normForcEqGD, bsShrFEqGD, resShrGD, mobShrGD,
 effNormFGD, resShearWOGD, mobShearWOGD, normShrRGD, momentEqlGD, weightGD,
 sliceWghtGD, hsPressureGD, baseWtrFGD, srfWtrFGD]

normForcEqGD, bsShrFEqGD, resShrGD, mobShrGD, effNormFGD, resShearWOGD,
  mobShearWOGD, normShrRGD, momentEqlGD, sliceWghtGD, baseWtrFGD,
  srfWtrFGD :: GenDefn
normForcEqGD = gd (OthModel normForcEq) (getUnit totNrmForce)   (Just nmFEqDeriv)
  [dRef chen2005]                      "normForcEq"  [nmFEqDesc]
bsShrFEqGD   = gd (OthModel bsShrFEq)   (getUnit mobShrI)       (Just bShFEqDeriv)
  [dRef chen2005]                      "bsShrFEq"    [bShFEqDesc]
resShrGD     = gd (OthModel resShr)     (getUnit shrResI)       (Just resShrDeriv)
  [dRef chen2005]                      "resShr"      [resShrDesc]
mobShrGD     = gd (OthModel mobShr)     (getUnit mobShrI)       (Just mobShrDeriv)
  [dRef chen2005]                      "mobShr"      [mobShrDesc]
effNormFGD   = gd (OthModel effNormF)   (getUnit nrmFSubWat)    (Just effNormFDeriv)
  [dRef chen2005]                      "effNormF"    [effNormFDesc]
resShearWOGD = gd (OthModel resShearWO) (getUnit shearRNoIntsl) Nothing
  (map dRef [chen2005, karchewski2012]) "resShearWO"  [resShearWODesc]
mobShearWOGD = gd (OthModel mobShearWO) (getUnit shearFNoIntsl) Nothing
  (map dRef [chen2005, karchewski2012]) "mobShearWO"  [mobShearWODesc]
normShrRGD   = gd (EquationalModel normShrR)   (getUnit intShrForce)   Nothing
  [dRef chen2005]                      "normShrR"    [nmShrRDesc]
momentEqlGD  = gd momentEqlModel        (Just newton)            (Just momEqlDeriv)
  [dRef chen2005]                      "momentEql"   [momEqlDesc]
sliceWghtGD  = gd (OthModel sliceWght)  (getUnit slcWght)       (Just sliceWghtDeriv)
  [dRef fredlund1977]                  "sliceWght"   [sliceWghtNotes]
baseWtrFGD   = gd (OthModel baseWtrF)   (getUnit baseHydroForce) (Just bsWtrFDeriv)
  [dRef fredlund1977]                  "baseWtrF"    [bsWtrFNotes]
srfWtrFGD    = gd (OthModel srfWtrF)    (getUnit surfHydroForce) (Just srfWtrFDeriv)
  [dRef fredlund1977]                  "srfWtrF"     [srfWtrFNotes]
--
normForcEq :: RelationConcept
normForcEq = makeRC "normForcEq" (nounPhraseSP "normal force equilibrium")
  nmFEqDesc nmFEqRel

nmFEqRel :: Relation
nmFEqRel = inxi totNrmForce $= eqlExprN cos sin
  (\x y -> x $- inxiM1 intShrForce `addRe` inxi intShrForce `addRe` y)

nmFEqDesc :: Sentence
nmFEqDesc = foldlSent [S "This equation satisfies", refS equilibrium +:+.
  S "in the normal direction", foldlList Comma List
  [slcWght `definedIn'''` sliceWghtGD, surfHydroForce `definedIn'''` srfWtrFGD,
  surfAngle `definedIn'''` angleB, baseAngle `definedIn'''` angleA]]

nmFEqDeriv :: Derivation
nmFEqDeriv = mkDerivNoHeader [foldlSent [atStart normForcEq `S.is`
  S "derived from the free body diagram" `S.of_`
  (refS figForceActing `S.in_` refS (SRS.physSyst [] []))]]

--
bsShrFEq :: RelationConcept
bsShrFEq = makeRC "bsShrFEq" (nounPhraseSP "base shear force equilibrium")
  bShFEqDesc bShFEqRel

bShFEqRel :: Relation
bShFEqRel = inxi mobShrI $= eqlExpr sin cos
  (\x y -> x $- inxiM1 intShrForce `addRe` inxi intShrForce `addRe` y)

bShFEqDesc :: Sentence
bShFEqDesc = foldlSent [S "This equation satisfies", refS equilibrium +:+.
  S "in the shear direction", foldlList Comma List [slcWght `definedIn'''` sliceWghtGD,
  surfHydroForce `definedIn'''` srfWtrFGD, surfAngle `definedIn'''` angleB, 
  baseAngle `definedIn'''` angleA]]

bShFEqDeriv :: Derivation
bShFEqDeriv = mkDerivNoHeader [foldlSent [atStart bsShrFEq `S.is`
  S "derived from the free body diagram" `S.of_`
  (refS figForceActing `S.in_` refS (SRS.physSyst [] []))]]
--
shrResEqn :: Expr
shrResEqn = inxi nrmFSubWat `mulRe` tan (inxi fricAngle) `addRe` (inxi effCohesion `mulRe`
  inxi baseLngth)

resShr :: RelationConcept
resShr = makeRC "resShr" (nounPhraseSP "resistive shear force")
  resShrDesc resShrRel -- genDef3Label

resShrRel :: Relation
resShrRel = inxi shrResI $= shrResEqn

resShrDesc :: Sentence
resShrDesc = foldlSent [baseLngth `definedIn'''` lengthLb]

resShrDeriv :: Derivation
resShrDeriv = mkDerivNoHeader [foldlSent [S "Derived by substituting",
  refS normStressDD `S.and_` refS tangStressDD, S "into the Mohr-Coulomb", phrase shrStress `sC`
  refS mcShrStrgth `sC` S "and multiplying both sides of the",
  phrase equation, S "by",  phraseNP (genericA `the_ofThe` slice) `S.in_`
  S "the shear-" :+: ch zcoord +:+. S "plane", S "Since", phraseNP (the slope),
  S "is assumed to extend infinitely in", phraseNP (the zDir),
  sParen (refS assumpPSC) `sC` S "the resulting", plural force,
  S "are expressed per", phrase metre, S "in the" +:+. phrase zDir, S "The",
  getTandS fricAngle `S.andThe` getTandS effCohesion, S "are not indexed by",
  ch index, S "because they are assumed to be isotropic",
  sParen (refS assumpSLI) `S.andThe` phrase soil, S "is assumed to be",
  S "homogeneous, with", phrase constant, plural soilPrpty, S "throughout",
  sParen (refS assumpSLH `sC` refS assumpSP)]]

--
mobShr :: RelationConcept
mobShr = makeRC "mobShr"
  (nounPhraseSP "mobilized shear force") mobShrDesc mobShrRel -- genDef4Label

mobShrRel :: Relation
mobShrRel = inxi mobShrI $= inxi shrResI $/ sy fs $= shrResEqn $/ sy fs

mobShrDesc :: Sentence
mobShrDesc = (baseLngth `definedIn'''` lengthLb !.)

mobShrDeriv :: Derivation
mobShrDeriv = mkDerivNoHeader [foldlSent_ [atStart mobShrI `S.is` S "derived by dividing",
  phrase definition `S.the_ofThe` ch shrResI, S "from", refS resShrGD,
  S "by", phraseNP (definition `the_ofThe` fs), S "from" +:+. refS factOfSafety,
  S "The", getTandS fs, S "is not indexed by", ch index,
  S "because it is assumed to be", phrase constant, S "for the entire",
  phrase slpSrf +:+. sParen (refS assumpFOSL)]]

--
effNormF :: RelationConcept
effNormF = makeRC "effNormF"
  (nounPhraseSP "effective normal force") effNormFDesc effNormFRel

effNormFRel :: Relation
effNormFRel = inxi nrmFSubWat $= inxi totNrmForce $- inxi baseHydroForce

effNormFDesc :: Sentence
effNormFDesc = (baseHydroForce `definedIn'''` baseWtrFGD !.)

effNormFDeriv :: Derivation
effNormFDeriv = mkDerivNoHeader [foldlSent [
  S "Derived by substituting", refS normStressDD, S "into",
  refS effStress `S.and_` S "multiplying both sides of", phraseNP (the equation),
  S "by", phraseNP (genericA `the_ofThe` slice), S "in the shear-" :+:
  ch zcoord +:+. S "plane", S "Since", phraseNP (the slope),
  S "is assumed to extend infinitely in", phraseNP (the zDir),
  sParen (refS assumpPSC) `sC` S "the resulting", plural force,
  S "are expressed per", phrase metre, S "in", phraseNP (the zDir)]]

-- 

normShrR :: QDefinition 
normShrR = mkQuantDef intShrForce nmShrRRel


nmShrRRel :: Relation
nmShrRRel = sy normToShear `mulRe` sy scalFunc `mulRe` sy intNormForce

nmShrRDesc :: Sentence
nmShrRDesc = foldlSent [S "Mathematical representation of the primary",
  phrase assumption, S "for the Morgenstern-Price", phrase method_ +:+.
  sParen (refS assumpINSFL), scalFunc `definedIn'''` ratioVariation]

--
resShearWO :: RelationConcept
resShearWO = makeRC "resShearWO"
  (nounPhraseSP "resistive shear force, without interslice normal and shear forces") resShearWODesc resShearWORel

resShearWORel :: Relation
resShearWORel = inxi shearRNoIntsl $=
  ((inxi slcWght `addRe` (inxi surfHydroForce `mulRe` cos (inxi surfAngle))) `mulRe`
  cos (inxi baseAngle) `addRe` (neg (inxi watrForce) `addRe` inxiM1 watrForce `addRe`
  (inxi surfHydroForce `mulRe` sin (inxi surfAngle)) `mulRe` sin (inxi baseAngle) $-
  inxi baseHydroForce) `mulRe` tan (inxi fricAngle) `addRe` (inxi effCohesion `mulRe`
  inxi baseLngth))

resShearWODesc :: Sentence
resShearWODesc = (foldlList Comma List [slcWght `definedIn'''` sliceWghtGD,
  surfHydroForce `definedIn'''` srfWtrFGD,
  surfAngle `definedIn'''` angleB,
  baseAngle `definedIn'''` angleA,
  watrForce `definedIn'''` intersliceWtrF,
  baseHydroForce `definedIn'''` baseWtrFGD,
  baseLngth `definedIn'''` lengthLb] !.)

--
--
mobShearWO :: RelationConcept
mobShearWO = makeRC "mobShearWO"
  (nounPhraseSP "mobilized shear force, without interslice normal and shear forces") mobShearWODesc mobShearWORel

mobShearWORel :: Relation
mobShearWORel = inxi shearFNoIntsl $= ((inxi slcWght `addRe` (inxi surfHydroForce `mulRe`
  cos (inxi surfAngle))) `mulRe` sin (inxi baseAngle) $- ((neg (inxi watrForce) `addRe`
  inxiM1 watrForce `addRe` (inxi surfHydroForce `mulRe` sin (inxi surfAngle))) `mulRe` cos (inxi baseAngle)))

mobShearWODesc :: Sentence
mobShearWODesc = (foldlList Comma List [slcWght `definedIn'''` sliceWghtGD,
  surfHydroForce `definedIn'''` srfWtrFGD,
  surfAngle `definedIn'''` angleB,
  baseAngle `definedIn'''` angleA,
  watrForce `definedIn'''` intersliceWtrF] !.)

--

momentEqlModel :: ModelKinds
momentEqlModel = EquationalConstraints $
  mkConstraintSet (dccWDS "momentEql" (nounPhraseSP "moment equilibrium") momEqlDesc) $
  NE.fromList [momEqlRel]

momentEql :: RelationConcept
momentEql = makeRC "momentEql" (nounPhraseSP "moment equilibrium")
  momEqlDesc momEqlRel -- genDef6Label

momEqlRel :: Relation
momEqlRel = exactDbl 0 $= momExpr (\ x y -> x `addRe`
  (half (inxi baseWthX) `mulRe` (inxi intShrForce `addRe` inxiM1 intShrForce)) `addRe` y)

momEqlDesc :: Sentence
momEqlDesc = foldlSent [S "This", phrase equation, S "satisfies",
  refS equilibrium, S "for the net" +:+. phrase genericM,
  foldlList Comma List [baseWthX `definedIn'''` lengthB,
  baseAngle `definedIn'''` angleA,
  slcWght `definedIn'''` sliceWghtGD,
  midpntHght `definedIn'''` slcHeight,
  surfHydroForce `definedIn'''` srfWtrFGD,
  surfAngle `definedIn'''` angleB]]

momEqlDeriv :: Derivation
momEqlDeriv = mkDerivNoHeader (weave [momEqlDerivSentences, momEqlDerivEqns])

momEqlDerivSentences :: [Sentence]
momEqlDerivSentences = map foldlSentCol [momEqlDerivTorqueSentence,
  momEqlDerivMomentSentence, momEqlDerivNormaliSentence,
  momEqlDerivNormaliM1Sentence, momEqlDerivWateriSentence,
  momEqlDerivWateriM1Sentence, momEqlDerivSheariSentence,
  momEqlDerivSheariM1Sentence, momEqlDerivSeismicIntSentence,
  momEqlDerivSeismicSentence, momEqlDerivSeismicWSentence,
  momEqlDerivHydroSentence, momEqlDerivExtSentence, momEqlDerivFinalSentence]

momEqlDerivEqns :: [Sentence]
momEqlDerivEqns = map eS [momEqlDerivTorqueEqn, momEqlDerivMomentEqn,
  momEqlDerivNormaliEqn, momEqlDerivNormaliM1Eqn, momEqlDerivWateriEqn,
  momEqlDerivWateriM1Eqn, momEqlDerivSheariEqn,
  momEqlDerivSheariM1Eqn, momEqlDerivSeismicIntEqn,
  momEqlDerivSeismicEqn, momEqlDerivSeismicWEqn,
  momEqlDerivHydroEqn, momEqlDerivExtEqn,
  momEqlDerivFinalEqn]

momEqlDerivTorqueSentence, momEqlDerivMomentSentence,
  momEqlDerivNormaliSentence, momEqlDerivNormaliM1Sentence,
  momEqlDerivWateriSentence, momEqlDerivWateriM1Sentence,
  momEqlDerivSheariSentence, momEqlDerivSheariM1Sentence,
  momEqlDerivSeismicIntSentence, momEqlDerivSeismicSentence,
  momEqlDerivSeismicWSentence, momEqlDerivHydroSentence,
  momEqlDerivExtSentence, momEqlDerivFinalSentence :: [Sentence]

momEqlDerivTorqueEqn, momEqlDerivMomentEqn,
  momEqlDerivNormaliEqn, momEqlDerivNormaliM1Eqn, momEqlDerivWateriEqn,
  momEqlDerivWateriM1Eqn, momEqlDerivSheariEqn,
  momEqlDerivSheariM1Eqn, momEqlDerivSeismicIntEqn,
  momEqlDerivSeismicEqn, momEqlDerivSeismicWEqn,
  momEqlDerivHydroEqn, momEqlDerivExtEqn,
  momEqlDerivFinalEqn :: Expr

momEqlDerivTorqueSentence = [atStart genericM, S "is equal to",
  phrase torque `sC` S "so", phraseNP (the equation), S "from", refS torqueDD,
  S "will be used to calculate", plural genericM]

momEqlDerivMomentSentence = [S "Considering one dimension, with",
  plural genericM, S "in the clockwise direction as positive and",
  plural genericM, S "in the counterclockwise direction as negative" `sC`
  S "and replacing", phraseNP (the torque), S "symbol with", phraseNP (the genericM),
  S "symbol,", phraseNP (the equation), S "simplifies to"]

momEqlDerivNormaliSentence = [S "where", ch rotForce `S.isThe`
  phrase rotForce `S.and_` ch momntArm `S.isThe` phrase momntArm `sC`
  S "or the distance between", phraseNP (the force) `S.andThe` S "axis about" +:+.
  S "which the rotation acts",
  S "To represent", phraseNP (the momentEqlGD) `sC` pluralNP (the genericM),
  S "from each", phrase force, S "acting on a", phrase slice +:+.
  S "must be considered and added together", atStartNP' (the force),
  S "acting on a", phrase slice, S "are all shown in" +:+.
  refS figForceActing,
  S "The midpoint of the base of a", phrase slice, S "is considered as the",
  S "axis of rotation, from which", phraseNP (the momntArm) +:+. S "is measured",
  S "Considering first", phraseNP (NP.the (combineNINI intrslce normForce)),
  S "acting on", phrase slice, S "interface", ch index `sC`
  phraseNP (the genericM), S "is negative because", phraseNP (the force),
  S "tends to rotate", phraseNP (the slice), S "in a counterclockwise",
  S "direction" `sC` S "and", phraseNP (the momntArm) `S.is` phraseNP (height `the_ofThe`
  force), S "plus the difference in height between the base at",
  phrase slice, S "interface", ch index `S.andThe` S "base at the midpoint of",
  phrase slice +:+. ch index,
  S "Thus,", phraseNP (the genericM), S "is expressed as"]

momEqlDerivNormaliM1Sentence = [S "For the", eS (sy index $- int 1) :+: S "th",
  phrase slice, S "interface" `sC` phraseNP (the genericM) `S.is`
  S "similar but in the opposite direction"]

momEqlDerivWateriSentence = [S "Next,", phraseNP (the intrslce), S "normal water",
  phrase force +:+. S "is considered", S "This", phrase force, S "is zero at",
  phraseNP (height `the_ofThe` waterTable) `sC` S "then increases linearly towards",
  S "base" `S.the_ofThe` phrase slice, S "due to the increasing water" +:+.
  phrase pressure, S "For such a triangular distribution, the resultant",
  phrase force +:+. S "acts at one-third of the height", S "Thus, for the",
  phrase intrslce, S "normal water", phrase force, S "acting on", phrase slice,
  S "interface", ch index `sC` phraseNP (the genericM), S "is"]

momEqlDerivWateriM1Sentence = [atStartNP (the genericM), S "for the",
  phrase intrslce, S "normal water", phrase force, S "acting on", phrase slice,
  S "interface", eS (sy index $- int 1), S "is"]

momEqlDerivSheariSentence = [atStartNP (the intrslce), phrase shearForce,
  S "at", phrase slice, S "interface", ch index, S "tends to rotate in the",
  S "clockwise direction, and", phraseNP (NP.the (momntArm `isThe` len)),
  S "from", phraseNP (the slice), S "edge to", phraseNP (the slice), S "midpoint" `sC`
  S "equivalent to half of", S "width" `S.the_ofThe` phrase slice `sC` S "so the",
  phrase genericM, S "is"]

momEqlDerivSheariM1Sentence = [atStartNP (NP.the (combineNINI intrslce shearForce)),
  S "at", phrase slice, S "interface", eS (sy index $- int 1), S "also tends to",
  S "rotate in the clockwise direction, and has the same", phrase momntArm `sC`
  S "so", phraseNP (the genericM), S "is"]

-- FIXME: Once differentials are expressible in Expr (issue #1407), change "sy yi" to the differential dy. "ch yi" actually means y and should stay as-is.
momEqlDerivSeismicIntSentence = [S "Seismic", plural force, S "act over the",
  S "entire height of the" +:+. phrase slice, S "For each horizontal segment"
  `S.ofThe` phrase slice `sC` S "the seismic", phrase force `S.is`
  eS (sy earthqkLoadFctr `mulRe` inxi slcWght), S "where", eS (inxi slcWght),
  S "can be expressed as", eS (sy genericSpWght `mulRe` inxi baseWthX `mulRe` sy yi),
  S "using", refS weightGD, S "where", eS yi, S "is the height of" +:+.
  S "the segment under consideration", S "The corresponding", phrase momntArm `S.is`
  ch yi `sC` S "the height from the base of", phraseNP (the slice) +:+.
  S "to the segment under consideration", S "In reality,", pluralNP (the force),
  S "near the surface of", phraseNP (the soil), S "mass are slightly different",
  S "due to the slope of the surface, but this difference is assumed to be",
  S "negligible" +:+. sParen (refS assumpNESSS), S "The resultant",
  phrase genericM, S "from", pluralNP (the force), S "on all of the segments",
  S "with an equivalent resultant", phrase momntArm, S "is determined by",
  S "taking the integral over" +:+. phraseNP (NP.the (combineNINI slice height)), atStartNP' (the force), 
  S "tend to rotate in the counterclockwise direction, so the",
  phrase genericM, S "is negative"]

momEqlDerivSeismicSentence = [S "Solving the definite integral yields"]

momEqlDerivSeismicWSentence = [S "Using", refS weightGD,
  S "again to express", eS (sy genericSpWght `mulRe` inxi baseWthX `mulRe` inxi midpntHght),
  S "as", eS (inxi slcWght) `sC` phraseNP (the genericM), S "is"]

momEqlDerivHydroSentence = [S "The surface hydrostatic", phrase force,
  S "acts into the midpoint of the surface of", phraseNP (the slice) +:+.
  sParen (refS assumpHFSM),
  S "Thus, the vertical", phraseNP (component `ofThe` force),
  S "acts directly towards the point of rotation, and has a",
  phrase genericM +:+. S "of zero", S "The horizontal", phraseNP (component
  `ofThe` force), S "tends to rotate in a clockwise direction" `S.and_`
  phraseNP (the momntArm), S "is the entire height of the" +:+. phrase slice,
  S "Thus,", phraseNP (the genericM), S "is"]

momEqlDerivExtSentence = [S "The external", phrase force, S "again acts into",
  S "midpoint" `S.the_ofThe` phrase slice, S "surface, so the vertical",
  phrase component, S "does not contribute to", phraseNP (the genericM) `sC`
  S "and", phraseNP (the momntArm), S "is again the entire height of the" +:+.
  phrase slice, atStartNP (the genericM), S "is"]

momEqlDerivFinalSentence = [S "The base hydrostatic", phraseNP (force `and_`
  slice), phrase weight, S "both act in the direction of the point of",
  S "rotation", sParen (refS assumpHFSM) `sC` S "therefore both have",
  plural genericM +:+. S "of zero", S "Thus, all of", pluralNP (the genericM) +:+.
  S "have been determined", atStartNP (the momentEqlGD) `S.is`
  S "then represented by the sum of all", plural genericM]

momEqlDerivTorqueEqn = sy torque $= cross (sy displacement) (sy force)

momEqlDerivMomentEqn = sy genericM $= sy rotForce `mulRe` sy momntArm

momEqlDerivNormaliEqn = neg (inxi intNormForce) `mulRe` (inxi sliceHght `addRe`
  (half (inxi baseWthX) `mulRe` tan (inxi baseAngle)))

momEqlDerivNormaliM1Eqn = inxiM1 intNormForce `mulRe` (inxiM1 sliceHght $-
  (half (inxi baseWthX) `mulRe` tan (inxi baseAngle)))

momEqlDerivWateriEqn = neg (inxi watrForce) `mulRe` (frac 1 3 `mulRe` inxi sliceHghtW `addRe`
  (half (inxi baseWthX) `mulRe` tan (inxi baseAngle)))

momEqlDerivWateriM1Eqn = inxiM1 watrForce `mulRe` (frac 1 3 `mulRe` inxiM1 sliceHghtW `addRe`
  (half (inxi baseWthX) `mulRe` tan (inxi baseAngle)))

momEqlDerivSheariEqn = inxi intShrForce `mulRe` half (inxi baseWthX)

momEqlDerivSheariM1Eqn = inxiM1 intShrForce `mulRe` half (inxi baseWthX)

momEqlDerivSeismicIntEqn = neg $ defint (eqSymb yi) (exactDbl 0) (inxi midpntHght)
  (sy earthqkLoadFctr `mulRe` sy genericSpWght `mulRe` inxi baseWthX `mulRe` sy yi)

momEqlDerivSeismicEqn = neg $ sy earthqkLoadFctr `mulRe` sy genericSpWght `mulRe`
  inxi baseWthX `mulRe` half (square (inxi midpntHght))

momEqlDerivSeismicWEqn = neg $ sy earthqkLoadFctr `mulRe` inxi slcWght `mulRe`
  half (inxi midpntHght)

momEqlDerivHydroEqn = inxi surfHydroForce `mulRe` sin (inxi surfAngle) `mulRe`
  inxi midpntHght

momEqlDerivExtEqn = inxi surfLoad `mulRe` sin (inxi impLoadAngle) `mulRe` inxi midpntHght

momEqlDerivFinalEqn = exactDbl 0 $= momExpr (\ x y -> x `addRe`
  (half (inxi baseWthX) `mulRe` (inxi intShrForce `addRe` inxiM1 intShrForce)) `addRe` y)

--

sliceWght :: RelationConcept
sliceWght = makeRC "sliceWght" (nounPhraseSP "slice weight") sliceWghtNotes
  sliceWghtEqn

sliceWghtEqn :: Expr
sliceWghtEqn = inxi slcWght $= inxi baseWthX `mulRe` oneHalf `mulRe` completeCase [case1, case2, case3]
  where case1 = (((inxi slopeHght $- inxi slipHght) `addRe`
          (inxiM1 slopeHght $- inxiM1 slipHght)) `mulRe` sy satWeight,
          (inxi waterHght $> inxi slopeHght) $||
          (inxiM1 waterHght $> inxiM1 slopeHght))
        case2 = (((inxi slopeHght $- inxi waterHght) `addRe`
          (inxiM1 slopeHght $- inxiM1 waterHght)) `mulRe` sy dryWeight `addRe`
          (((inxi waterHght $- inxi slipHght) `addRe`
          (inxiM1 waterHght $- inxiM1 slipHght)) `mulRe` sy satWeight),
          (inxi slopeHght $>= inxi waterHght $>= inxi slipHght) $&&
          (inxiM1 slopeHght $>= inxiM1 waterHght $>= inxiM1 slipHght))
        case3 = (((inxi slopeHght $- inxi slipHght) `addRe`
          (inxiM1 slopeHght $- inxiM1 slipHght)) `mulRe` sy dryWeight,
          (inxi waterHght $< inxi slipHght) $||
          (inxiM1 waterHght $< inxiM1 slipHght))

sliceWghtNotes :: Sentence
sliceWghtNotes = foldlSent [S "This", phrase equation, S "is based on the",
  phrase assumption, S "that the surface and the base of a", phrase slice,
  S "are straight lines" +:+. sParen (refS assumpSBSBISL), S "The",
  getTandS dryWeight `S.andThe` getTandS satWeight, S "are not indexed by",
  ch index, S "because", phraseNP (the soil), S "is assumed to be homogeneous" `sC`
  S "with", pluralNP (combineNINI constant soilPrpty), S "throughout" +:+.
  sParen (refS assumpSLH), baseWthX `definedIn'''` lengthB]

sliceWghtDeriv :: Derivation
sliceWghtDeriv = mkDerivNoHeader (weave [sliceWghtDerivSentences, sliceWghtDerivEqns])

sliceWghtDerivEqns :: [Sentence]
sliceWghtDerivEqns = map eS [sliceWghtDerivSatCaseWeightEqn,
  sliceWghtDerivSatCaseSliceEqn, sliceWghtDerivDryCaseWeightEqn,
  sliceWghtDerivDryCaseSliceEqn, sliceWghtDerivMixCaseWeightEqn,
  sliceWghtDerivMixCaseSliceEqn]

sliceWghtDerivSentences :: [Sentence]
sliceWghtDerivSentences = map foldlSentCol [sliceWghtDerivSatCaseIntroSentence,
  sliceWghtDerivSatCase2DSentence, sliceWghtDerivDryCaseIntroSentence,
  sliceWghtDerivDryCase2DSentence, sliceWghtDerivMixCaseIntroSentence,
  sliceWghtDerivMixCase2DSentence]

sliceWghtDerivSatCaseIntroSentence, sliceWghtDerivSatCase2DSentence,
  sliceWghtDerivDryCaseIntroSentence, sliceWghtDerivDryCase2DSentence,
  sliceWghtDerivMixCaseIntroSentence,
  sliceWghtDerivMixCase2DSentence :: [Sentence]

sliceWghtDerivSatCaseWeightEqn, sliceWghtDerivSatCaseSliceEqn,
  sliceWghtDerivDryCaseWeightEqn, sliceWghtDerivDryCaseSliceEqn,
  sliceWghtDerivMixCaseWeightEqn, sliceWghtDerivMixCaseSliceEqn :: Expr

sliceWghtDerivSatCaseIntroSentence = [S "For the case where the",
  phrase waterTable, S "is above", phraseNP (the slopeSrf) `sC`
  phraseNP (the slcWght), S "come from", phrase weight `S.the_ofThe` S "saturated" +:+.
  phrase soil,
  S "Substituting", plural value, S "for saturated", phrase soil, S "into the",
  phraseNP (equation `for` weight), S "from", refS weightGD,
  S "yields"]

sliceWghtDerivSatCase2DSentence = [S "Due to", refS assumpPSC `sC`
  S "only two dimensions are considered, so", pluralNP (the area) `S.of_`
  S "saturated", phrase soil, S "are considered instead of the" +:+.
  phrase satVol,
  S "Any given", phrase slice +:+. S "has a trapezoidal shape",
  atStartNP (the area), S "of a trapezoid is the average of",
  plural len `S.the_ofThe` S "parallel sides multiplied by", phraseNP (the len) +:+.
  S "between the parallel sides", S "The parallel sides in this case are the",
  phrase intrslce, S "edges and", phraseNP (the len), S "between them" `S.isThe`
  S "width of the" +:+. phrase slice, S "Thus" `sC` phraseNP (the slcWght),
  S "are defined as"]

sliceWghtDerivSatCaseWeightEqn = inxi slcWght $= inxi satVol `mulRe` sy satWeight

sliceWghtDerivSatCaseSliceEqn = inxi slcWght $= inxi baseWthX `mulRe` oneHalf `mulRe`
  ((inxi slopeHght $- inxi slipHght) `addRe` (inxiM1 slopeHght $- inxiM1 slipHght)) `mulRe` sy satWeight

sliceWghtDerivDryCaseIntroSentence = [S "For the case where the",
  phrase waterTable, S "is below", phraseNP (the slpSrf) `sC`
  phraseNP (the slcWght), S "come from", phrase weight `S.the_ofThe` S "dry" +:+. phrase soil,
  S "Substituting", plural value, S "for dry", phrase soil, S "into the",
  phraseNP (equation `for` weight), S "from", refS weightGD,
  S "yields"]

sliceWghtDerivDryCase2DSentence = [refS assumpPSC, S "again allows for",
  phraseNP (combineNINI twoD analysis), S "so", pluralNP (the area) `S.of_` S "dry",
  phrase soil, S "are considered instead of the" +:+. phrase dryVol,
  S "The trapezoidal", phrase slice,
  S "shape is the same as in the previous case" `sC` S "so", phraseNP (the slcWght),
  S "are defined as"]

sliceWghtDerivDryCaseWeightEqn = inxi slcWght $= inxi dryVol `mulRe` sy dryWeight

sliceWghtDerivDryCaseSliceEqn = inxi slcWght $= inxi baseWthX `mulRe` oneHalf `mulRe`
  ((inxi slopeHght $- inxi slipHght) `addRe` (inxiM1 slopeHght $- inxiM1 slipHght)) `mulRe` sy dryWeight

sliceWghtDerivMixCaseIntroSentence = [S "For the case where the",
  phrase waterTable, S "is between", phraseNP (NP.the (slopeSrf `and_`
  slpSrf)) `sC` phraseNP (the slcWght), S "are the sums of",
  plural weight `S.the_ofThe` S "dry portions"  `S.and_` plural weight `S.ofThe`
  S "saturated portions of the" +:+. phrase soil,
  S "Substituting", plural value, S "for dry and saturated", phrase soil,
  S "into", phraseNP (NP.the (equation `for` weight)),
  S "from", refS weightGD, S "and adding them together yields"]

sliceWghtDerivMixCase2DSentence = [refS assumpPSC, S "again allows for",
  phraseNP (combineNINI twoD analysis), S "so", pluralNP (the area) `S.of_` S "dry",
  phrase soil `S.and_` plural area `S.of_` S "saturated", phrase soil,
  S "are considered instead of the" +:+. phraseNP (dryVol `and_` satVol),
  atStartNP (the waterTable), S "is assumed to only intersect a", phrase slice,
  S "surface or base at a", phrase slice, S "edge",
  sParen (refS assumpWISE `sC` refS assumpWIBE) `sC` S "so the" +:+.
  S "dry and saturated portions each have trapezoidal shape", S "For the dry",
  S "portion, the parallel sides of the trapezoid are", pluralNP (the len),
  S "between", phraseNP (NP.the (slopeSrf `and_` waterTable)), S "at the",
  phrase slice +:+. S "edges", S "For the saturated portion, the parallel",
  S "sides of the trapezoid are", pluralNP (the len), S "between the",
  phraseNP (waterTable `and_` slpSrf), S "at", phraseNP (the slice) +:+.
  S "edges", S "Thus" `sC` phraseNP (the slcWght),  S "are defined as"]

sliceWghtDerivMixCaseWeightEqn = inxi slcWght $= inxi dryVol `mulRe` sy dryWeight `addRe`
  (inxi satVol `mulRe` sy satWeight)

sliceWghtDerivMixCaseSliceEqn = inxi slcWght $= (inxi baseWthX `mulRe` oneHalf `mulRe`
  (((inxi slopeHght $- inxi waterHght) `addRe`
  (inxiM1 slopeHght $- inxiM1 waterHght)) `mulRe` sy dryWeight `addRe`
  (((inxi waterHght $- inxi slipHght) `addRe`
  (inxiM1 waterHght $- inxiM1 slipHght)) `mulRe` sy satWeight)))

-- 

baseWtrF :: RelationConcept
baseWtrF = makeRC "baseWtrF" (nounPhraseSP "base hydrostatic force")
  bsWtrFNotes bsWtrFEqn

bsWtrFEqn :: Expr
bsWtrFEqn = inxi baseHydroForce $= inxi baseLngth `mulRe` sy waterWeight `mulRe` oneHalf `mulRe`
  completeCase [case1, case2]
  where case1 = ((inxi waterHght $- inxi slipHght) `addRe`
          (inxiM1 waterHght $- inxiM1 slipHght),
          (inxi waterHght $> inxi slipHght) $||
          (inxiM1 waterHght $> inxiM1 slipHght))
        case2 = (exactDbl 0, (inxi waterHght $<= inxi slipHght) $&&
          (inxiM1 waterHght $<= inxiM1 slipHght))

bsWtrFNotes :: Sentence
bsWtrFNotes = foldlSent [S "This", phrase equation, S "is based on the",
  phrase assumption, S "that the base of a", phrase slice,
  S "is a straight line" +:+. sParen (refS assumpSBSBISL),
  baseLngth `definedIn'''` lengthLb]

bsWtrFDeriv :: Derivation
bsWtrFDeriv = mkDerivNoHeader (weave [bsWtrFDerivSentences, bsWtrFDerivEqns] ++
  bsWtrFDerivEndSentence)

bsWtrFDerivEqns :: [Sentence]
bsWtrFDerivEqns = map eS [bsWtrFDerivWeightEqn, bsWtrFDerivHeightEqn,
  bsWtrFDerivSliceEqn]

bsWtrFDerivSentences :: [Sentence]
bsWtrFDerivSentences = map foldlSentCol [bsWtrFDerivIntroSentence,
  bsWtrFDerivHeightSentence, bsWtrFDeriv2DSentence]

bsWtrFDerivIntroSentence, bsWtrFDerivHeightSentence, bsWtrFDeriv2DSentence,
  bsWtrFDerivEndSentence :: [Sentence]

bsWtrFDerivWeightEqn, bsWtrFDerivHeightEqn, bsWtrFDerivSliceEqn :: Expr

bsWtrFDerivIntroSentence = [atStartNP (the baseHydroForce), S "come from the",
  S "hydrostatic", phrase pressure, S "exerted by the water above the base of",
  S "each" +:+. phrase slice,
  atStartNP (the equation), S "for hydrostatic", phrase pressure,
  S "from", refS hsPressureGD, S "is"]

bsWtrFDerivHeightSentence = [atStartNP (the specWeight), S "in this case is",
  S "the" +:+. getTandS waterWeight,
  atStartNP (the height), S "in this case is the height from", phraseNP (the slice),
  S "base to the" +:+. phrase waterTable,
  S "This", phrase height, S "is measured from", S "midpoint" `S.the_ofThe`
  phrase slice, S "because the resultant hydrostatic", phrase force,
  S "is assumed to act at", phraseNP (the slice), S "midpoint" +:+.
  sParen (refS assumpHFSM),
  atStartNP (the height), S "at the midpoint is the average of the",
  phrase height, S "at", phrase slice, S "interface", ch index `S.andThe`
  phrase height, S "at", phrase slice, S "interface", eS (sy index $- int 1)]

bsWtrFDeriv2DSentence = [S "Due to", refS assumpPSC `sC`
  S "only two dimensions are considered, so", phraseNP (the baseHydroForce),
  S "are expressed as", plural force +:+. S "per meter",
  atStartNP' (the pressure), S "acting on", pluralNP (the slice), S "can thus be converted",
  S "to", phrase baseHydroForce, S "by multiplying by the corresponding",
  phraseNP (len `ofThe` slice), S "base", eS (inxi baseLngth) `sC`
  S "assuming", phraseNP (the waterTable), S "does not intersect a", phrase slice,
  S "base except at a", phrase slice, S "edge" +:+.
  sParen (refS assumpWIBE),
  S "Thus, in the case where", phraseNP (height `the_ofThe` waterTable),
  S "is above", phraseNP (height `the_ofThe` slpSrf) `sC`
  phraseNP (the baseHydroForce), S "are defined as"]

bsWtrFDerivEndSentence = [foldlSent [S "This", phrase equation `S.is`
  S "the non-zero case of" +:+. refS baseWtrFGD,
  S "The zero case is when", phraseNP (height `the_ofThe` waterTable),
  S "is below", phraseNP (height `the_ofThe` slpSrf) `sC` S "so there is no",
  S "hydrostatic", phrase force]]

bsWtrFDerivWeightEqn = sy pressure $= sy specWeight `mulRe` sy height

bsWtrFDerivHeightEqn = oneHalf `mulRe` ((inxi waterHght $- inxi slipHght) `addRe` (inxiM1 waterHght $- inxiM1 slipHght))

bsWtrFDerivSliceEqn = inxi baseHydroForce $= inxi baseLngth `mulRe` sy waterWeight `mulRe`
  bsWtrFDerivHeightEqn

--

srfWtrF :: RelationConcept
srfWtrF = makeRC "srfWtrF" (nounPhraseSP "surface hydrostatic force")
  srfWtrFNotes srfWtrFEqn

srfWtrFEqn :: Relation
srfWtrFEqn = inxi surfHydroForce $= inxi surfLngth `mulRe` sy waterWeight `mulRe` oneHalf `mulRe`
  completeCase [case1, case2]
  where case1 = ((inxi waterHght $- inxi slopeHght) `addRe`
          (inxiM1 waterHght $- inxiM1 slopeHght),
          (inxi waterHght $> inxi slopeHght) $||
          (inxiM1 waterHght $> inxiM1 slopeHght))
        case2 = (exactDbl 0, (inxi waterHght $<= inxi slopeHght) $&&
          (inxiM1 waterHght $<= inxiM1 slopeHght))

srfWtrFNotes :: Sentence
srfWtrFNotes = foldlSent [S "This", phrase equation, S "is based on the",
  phrase assumption, S "that the surface of a", phrase slice,
  S "is a straight line" +:+. sParen (refS assumpSBSBISL), 
  surfLngth `definedIn'''` lengthLs]

srfWtrFDeriv :: Derivation
srfWtrFDeriv = mkDerivNoHeader (weave [srfWtrFDerivSentences, srfWtrFDerivEqns] ++
  srfWtrFDerivEndSentence)

srfWtrFDerivEqns :: [Sentence]
srfWtrFDerivEqns = map eS [srfWtrFDerivWeightEqn, srfWtrFDerivHeightEqn,
  srfWtrFDerivSliceEqn]

srfWtrFDerivSentences :: [Sentence]
srfWtrFDerivSentences = map foldlSentCol [srfWtrFDerivIntroSentence,
  srfWtrFDerivHeightSentence, srfWtrFDeriv2DSentence]

srfWtrFDerivIntroSentence, srfWtrFDerivHeightSentence, srfWtrFDeriv2DSentence,
  srfWtrFDerivEndSentence :: [Sentence]

srfWtrFDerivWeightEqn, srfWtrFDerivHeightEqn, srfWtrFDerivSliceEqn :: Expr

srfWtrFDerivIntroSentence = [atStartNP (the surfHydroForce), S "come from the",
  S "hydrostatic", phrase pressure, S "exerted by the water above the surface",
  S "of each" +:+. phrase slice,
  atStartNP (the equation), S "for hydrostatic", phrase pressure, S "from",
  refS hsPressureGD, S "is"]

srfWtrFDerivHeightSentence = [atStartNP (the specWeight), S "in this case is",
  S "the" +:+. getTandS waterWeight,
  atStartNP (the height), S "in this case is the height from", phraseNP (the slice),
  S "surface to the" +:+. phrase waterTable,
  S "This", phrase height, S "is measured from", S "midpoint" `S.the_ofThe`
  phrase slice, S "because the resultant hydrostatic", phrase force,
  S "is assumed to act at", phraseNP (the slice), S "midpoint" +:+.
  sParen (refS assumpHFSM),
  atStartNP (the height), S "at the midpoint is the average of the",
  phrase height, S "at", phrase slice, S "interface", ch index `S.andThe`
  phrase height, S "at", phrase slice, S "interface", eS (sy index $- int 1)]

srfWtrFDeriv2DSentence = [S "Due to", refS assumpPSC `sC`
  S "only two dimensions are considered, so", phraseNP (the surfHydroForce),
  S "are expressed as", plural force +:+. S "per meter", S "The",
  plural pressure, S "acting on", pluralNP (the slice), S "can thus be converted",
  S "to", phrase surfHydroForce, S "by multiplying by the corresponding",
  phrase len, S "of", phraseNP (the slice), S "surface", eS (inxi surfLngth) `sC`
  S "assuming", phraseNP (the waterTable), S "does not intersect a", phrase slice,
  S "surface except at a", phrase slice, S "edge" +:+.
  sParen (refS assumpWISE),
  S "Thus, in the case where", phraseNP (height `the_ofThe` waterTable),
  S "is above", phraseNP (height `the_ofThe` slopeSrf) `sC`
  phraseNP (the surfHydroForce), S "are defined as"]

srfWtrFDerivEndSentence = [foldlSent [S "This" +:+ phrase equation `S.is`
  S "the non-zero case of" +:+. refS srfWtrFGD,
  S "The zero case is when", phraseNP (height `the_ofThe` waterTable),
  S "is below", phraseNP (height `the_ofThe` slopeSrf) `sC` S "so there is no",
  S "hydrostatic", phrase force]]

srfWtrFDerivWeightEqn = sy pressure $= sy specWeight `mulRe` sy height

srfWtrFDerivHeightEqn = oneHalf `mulRe` ((inxi waterHght $- inxi slopeHght) `addRe` (inxiM1 waterHght $- inxiM1 slopeHght))

srfWtrFDerivSliceEqn = inxi surfHydroForce $= inxi surfLngth `mulRe` sy waterWeight `mulRe`
  srfWtrFDerivHeightEqn

-- References --
genDefRefs :: [Reference]
genDefRefs = map ref generalDefinitions ++ concatMap (\x -> map ref $ x ^. getDecRefs) generalDefinitions 