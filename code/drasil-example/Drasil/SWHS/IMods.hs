module Drasil.SWHS.IMods (iMods, eBalanceOnWtr, eBalanceOnWtrDerivDesc1,
  eBalanceOnWtrDerivDesc3, eBalanceOnPCM, heatEInWtr, heatEInPCM, instModIntro, iModRefs) where

import Language.Drasil
import Theory.Drasil (InstanceModel, im, imNoDeriv, qwUC, qwC, deModel', othModel')
import Utils.Drasil
import Utils.Drasil.Concepts
import qualified Utils.Drasil.NounPhrase as NP
import qualified Utils.Drasil.Sentence as S
import Control.Lens((^.))

import Data.Drasil.Concepts.Documentation (assumption, condition, constraint,
  goal, input_, solution, output_)
import Data.Drasil.Concepts.Math (change, equation, ode, rightSide, rOfChng, surArea)
import Data.Drasil.Concepts.PhysicalProperties (liquid, mass, solid, vol)
import Data.Drasil.Concepts.Thermodynamics (boilPt, boiling, heat, heatCapSpec,
  heatTrans, htFlux, latentHeat, melting, phaseChange, sensHeat, temp)
import Data.Drasil.Quantities.Physics (energy, time)

import Drasil.SWHS.Assumptions (assumpCTNOD, assumpSITWP, assumpPIS, assumpWAL,
  assumpPIT, assumpNIHGBWP, assumpVCMPN, assumpNGSP, assumpAPT, assumpTHCCoL,
  assumpCWTAT, assumpTPCAV)
import Drasil.SWHS.Concepts (coil, phsChgMtrl, tank, water)
import Drasil.SWHS.DataDefs (ddHtFusion, ddMeltFrac, balanceDecayRate,
  balanceDecayTime, balanceSolidPCM, balanceLiquidPCM)
import Drasil.SWHS.GenDefs (htFluxWaterFromCoil, htFluxPCMFromWater, rocTempSimp)
import Drasil.SWHS.Goals (waterTempGS, pcmTempGS, waterEnergyGS, pcmEnergyGS)
import Drasil.SWHS.References (koothoor2013)
import Drasil.SWHS.TMods (sensHtE, latentHtE)
import Drasil.SWHS.Unitals (coilHTC, coilSA, eta, htFluxC, htFluxP, htCapLP,
  htCapSP, htCapW, htFusion, latentEP, meltFrac, pcmE, pcmHTC, pcmInitMltE,
  pcmMass, pcmSA, pcmVol, tInitMelt, tauLP, tauSP, tauW, tempC, tempInit,
  tempMeltP, tempPCM, tempW, timeFinal, volHtGen, watE, wMass, wVol)

iMods :: [InstanceModel]
iMods = [eBalanceOnWtr, eBalanceOnPCM, heatEInWtr, heatEInPCM]

---------
-- IM1 --
---------
eBalanceOnWtr :: InstanceModel
eBalanceOnWtr = im (deModel' eBalanceOnWtrRC)
  [qwUC wMass ,qwUC htCapW, qwUC coilHTC, qwUC pcmSA, qwUC pcmHTC, qwUC coilSA
  ,qwUC tempPCM, qwUC timeFinal, qwC tempC $ UpFrom (Exc, sy tempInit)
  ,qwUC tempInit]
  -- [sy tempInit $< sy tempC] 
  (qw tempW) []
  -- [0 $<= sy time $<= sy timeFinal]
  [ref koothoor2013] (Just eBalanceOnWtrDeriv) "eBalanceOnWtr" balWtrDesc

eBalanceOnWtrRC :: RelationConcept
eBalanceOnWtrRC = makeRC "eBalanceOnWtrRC" (nounPhraseSP $ "Energy balance on " ++
  "water to find the temperature of the water") (tempW ^. defn) balWtrRel
  -- eBalanceOnWtrL

balWtrRel :: Relation
balWtrRel = deriv (sy tempW) time $= recip_ (sy tauW) `mulRe`
  ((sy tempC $- apply1 tempW time) `addRe`
  (sy eta `mulRe` (apply1 tempPCM time $- apply1 tempW time)))

balWtrDesc :: [Sentence]
balWtrDesc = map foldlSent [
  [eS tempPCM `S.is` S "defined by", refS eBalanceOnPCM],
  [atStartNP (the input_), phrase constraint, eS $ sy tempInit $<= sy tempC,
   S "comes from", refS assumpCTNOD],
  [ch tauW `S.is` S "calculated from", refS balanceDecayRate],
  [ch eta  `S.is` S "calculated from", refS balanceDecayTime],
  [S "The initial", plural condition, S "for the", getAcc ode `S.are`
   eS (apply tempW [exactDbl 0] $= apply tempPCM [exactDbl 0] $= sy tempInit) `follows` assumpSITWP],
  [S "The", getAcc ode, S "applies as long as the", phrase water `S.is` EmptyS `S.in_`
   phrase liquid, S "form" `sC` eS (realInterval tempW (Bounded (Exc, exactDbl 0) (Exc, exactDbl 100))),
   sParen (unwrap $ getUnit tempW), S "where", eS (exactDbl 0), sParen (unwrap $ getUnit tempW) `S.and_`
   eS (exactDbl 100), sParen (unwrap $ getUnit tempW) `S.are` pluralNP (NP.the ((melting `and_`
   boilPt) `of_PSNPNI` water)) `sC` S "respectively",
   fromSources [assumpWAL, assumpAPT]]]

----------------------------------------------
--    Derivation of eBalanceOnWtr           --
----------------------------------------------
-- type Derivation = [Sentence]
eBalanceOnWtrDeriv :: Derivation
eBalanceOnWtrDeriv = mkDerivName (phraseNP (the energy) +:+ S "balance on water")
  (weave [eBalanceOnWtrDerivSentences, map eS eBalanceOnWtrDerivEqnsIM1])

eBalanceOnWtrDerivSentences :: [Sentence]
eBalanceOnWtrDerivSentences = [eBalanceOnWtrDerivDesc1 htTransEnd overAreas extraAssumps assumpNIHGBWP,
  eBalanceOnWtrDerivDesc2, eBalanceOnWtrDerivDesc3, eBalanceOnWtrDerivDesc4,
  eBalanceOnWtrDerivDesc5, eBalanceOnWtrDerivDesc6, eBalanceOnWtrDerivDesc7 eq2]

eBalanceOnWtrDerivDesc1 :: Sentence -> Sentence-> Sentence -> ConceptInstance -> Sentence
eBalanceOnWtrDerivDesc1 htEnd oa ea htA = foldlSentCol [
  S "To find the", phrase rOfChng `S.of_` eS tempW `sC`
  S "we look at the", phrase energy, S "balance on" +:+. phrase water, atStartNP (the vol),
  S "being considered" `S.isThe` phraseNP (vol `of_` water) `S.inThe`
  phrase tank, eS wVol `sC` S "which has", phrase mass +:+. (eS wMass `S.and_`
  phrase heatCapSpec `sC` eS htCapW), atStart heatTrans, S "occurs in the",
  phrase water, S "from the", phrase coil, S "as", eS htFluxC,
  sParen (refS htFluxWaterFromCoil) +:+ htEnd `sC` EmptyS +:+. oa, ea, S "No", phrase heatTrans, S "occurs to", S "outside" `S.the_ofThe`
  phrase tank `sC` S "since it has been assumed to be perfectly insulated" +:+.
  sParen (refS assumpPIT), S "Since the", phrase assumption,
  S "is made that no internal heat is generated" +:+. (sParen (refS htA) `sC`
  eS (sy volHtGen $= exactDbl 0)), S "Therefore" `sC` phraseNP (the equation) `S.for`
  refS rocTempSimp, S "can be written as"]

htTransEnd :: Sentence
htTransEnd = foldlSent_ [S "and from the", phrase water, S "into the",
  getAcc phsChgMtrl, S "as", ch htFluxP, sParen (refS htFluxPCMFromWater)]

overAreas :: Sentence
overAreas = S "over areas" +:+ ch coilSA `S.and_` ch pcmSA `sC` S "respectively"

extraAssumps :: Sentence
extraAssumps = foldlSent [S "The thermal flux is constant over", ch coilSA `sC`
  S "since", phraseNP (temp `the_ofThe` coil) `S.is` S "assumed to not vary along its length",
  sParen (refS assumpTHCCoL) `sC` EmptyS `S.andThe` S "thermal flux is constant over",
  ch pcmSA `sC` S "since", phrase temp `S.the_ofThe` getAcc phsChgMtrl `S.isThe`
  S "same throughout its", phrase vol, sParen (refS assumpTPCAV) `S.andThe`
  phrase water `S.is` S "fully mixed", sParen (refS assumpCWTAT)]

eBalanceOnWtrDerivDesc2 :: Sentence
eBalanceOnWtrDerivDesc2 = foldlSentCol [S "Using", refS htFluxWaterFromCoil `S.for`
  ch htFluxC `S.and_` refS htFluxPCMFromWater `S.for` ch htFluxP `sC` S "this can be written as"]

eBalanceOnWtrDerivDesc3 :: Sentence
eBalanceOnWtrDerivDesc3 = foldlSentCol [S "Dividing", eqN 2, S "by", eS eq1 `sC` S "we obtain"]

eBalanceOnWtrDerivDesc4 :: Sentence
eBalanceOnWtrDerivDesc4 = foldlSentCol [S "Factoring the negative sign out" `S.of_`
  (S "second term" `S.the_ofThe` short rightSide) `S.of_` eqN 3 `S.and_`
  S "multiplying it by", ch coilHTC, ch coilSA, S "/", ch coilHTC, ch coilSA, S "yields"]

eBalanceOnWtrDerivDesc5 :: Sentence
eBalanceOnWtrDerivDesc5 = S "Rearraging this" +:+ phrase equation +: S "gives us"

eBalanceOnWtrDerivDesc6 :: Sentence
eBalanceOnWtrDerivDesc6 = substitute [balanceDecayRate, balanceDecayTime]

eBalanceOnWtrDerivDesc7 :: Expr -> Sentence
eBalanceOnWtrDerivDesc7 eq22 = foldlSentCol [S "Finally, factoring out", eS eq22 `sC`
  S "we are left with the governing", getAcc ode `S.for` refS eBalanceOnWtr]

eq1, eq2 :: Expr
eq1 = sy wMass `mulRe` sy htCapW
eq2 = recip_ $ sy tauW

eBalanceOnWtrDerivEqn1, eBalanceOnWtrDerivEqn2, eBalanceOnWtrDerivEqn3,
 eBalanceOnWtrDerivEqn4, eBalanceOnWtrDerivEqn5, eBalanceOnWtrDerivEqn6, eBalanceOnWtrDerivEqn7 :: Expr

eBalanceOnWtrDerivEqn1 = sy wMass `mulRe` sy htCapW `mulRe` deriv (sy tempW) time $=
  sy htFluxC `mulRe` sy coilSA $- (sy htFluxP `mulRe` sy pcmSA)

eBalanceOnWtrDerivEqn2 = sy wMass `mulRe` sy htCapW `mulRe` deriv (sy tempW) time $=
  sy coilHTC `mulRe` sy coilSA `mulRe` (sy tempC $- sy tempW) $-
  (sy pcmHTC `mulRe` sy pcmSA `mulRe` (sy tempW $- sy tempPCM))

eBalanceOnWtrDerivEqn3 = deriv (sy tempW) time $=
  (sy coilHTC `mulRe` sy coilSA $/
  (sy wMass `mulRe` sy htCapW)) `mulRe` (sy tempC $- sy tempW) $-
  ((sy pcmHTC `mulRe` sy pcmSA $/
  (sy wMass `mulRe` sy htCapW)) `mulRe` (sy tempW $- sy tempPCM))

eBalanceOnWtrDerivEqn4 = deriv (sy tempW) time $=
  (sy coilHTC `mulRe` sy coilSA $/
  (sy wMass `mulRe` sy htCapW)) `mulRe`  (sy tempC $- sy tempW) `addRe`
  ((sy coilHTC `mulRe` sy coilSA $/
  (sy coilHTC `mulRe` sy coilSA)) `mulRe` (sy pcmHTC `mulRe` sy pcmSA $/
  (sy wMass `mulRe` sy htCapW)) `mulRe` (sy tempPCM $- sy tempW))

eBalanceOnWtrDerivEqn5 = deriv (sy tempW) time $=
  (sy coilHTC `mulRe` sy coilSA $/
  (sy wMass `mulRe` sy htCapW)) `mulRe`  (sy tempC $- sy tempW) `addRe`
  ((sy pcmHTC `mulRe` sy pcmSA $/
  (sy coilHTC `mulRe` sy coilSA)) `mulRe` (sy coilHTC `mulRe` sy coilSA $/
  (sy wMass `mulRe` sy htCapW)) `mulRe` (sy tempPCM $- sy tempW))

eBalanceOnWtrDerivEqn6 = deriv (sy tempW) time $=
  recip_ (sy tauW) `mulRe` (sy tempC $- sy tempW) `addRe` ((sy eta $/ sy tauW) `mulRe` (sy tempPCM $- sy tempW))

eBalanceOnWtrDerivEqn7 = deriv (sy tempW) time $=
  recip_ (sy tauW) `mulRe` ((sy tempC $- sy tempW) `addRe` (sy eta `mulRe` (sy tempPCM $- sy tempW)))

eBalanceOnWtrDerivEqnsIM1 :: [Expr]
eBalanceOnWtrDerivEqnsIM1 = [eBalanceOnWtrDerivEqn1, eBalanceOnWtrDerivEqn2,
 eBalanceOnWtrDerivEqn3, eBalanceOnWtrDerivEqn4, eBalanceOnWtrDerivEqn5,
 eBalanceOnWtrDerivEqn6, eBalanceOnWtrDerivEqn7]

---------
-- IM2 --
---------
eBalanceOnPCM :: InstanceModel
eBalanceOnPCM = im (deModel' eBalanceOnPCMRC) [qwC tempMeltP $ UpFrom (Exc, sy tempInit)
  , qwUC timeFinal, qwUC tempInit, qwUC pcmSA
  , qwUC pcmHTC, qwUC pcmMass, qwUC htCapSP, qwUC htCapLP]
  (qw tempPCM) []
  [ref koothoor2013] (Just eBalanceOnPCMDeriv) "eBalanceOnPCM" balPCMNotes

eBalanceOnPCMRC :: RelationConcept
eBalanceOnPCMRC = makeRC "eBalanceOnPCMRC" (nounPhraseSP
  "Energy Balance on PCM to find temperature of PCM")
  (tempPCM ^. defn) balPCMRel -- eBalanceOnPCML

balPCMRel :: Relation
balPCMRel = deriv (sy tempPCM) time $= completeCase [case1, case2, case3]
  where case1 = (recip_ (sy tauSP) `mulRe` (apply1 tempW time $-
          apply1 tempPCM time), realInterval tempPCM (UpTo (Exc, sy tempMeltP)))
        case2 = (recip_ (sy tauLP) `mulRe` (apply1 tempW time $-
          apply1 tempPCM time), realInterval tempPCM (UpFrom (Exc,sy tempMeltP)))
        case3 = (exactDbl 0, sy tempPCM $= sy tempMeltP $&& realInterval meltFrac (Bounded (Exc, exactDbl 0) (Exc, exactDbl 1)))

balPCMNotes :: [Sentence]
balPCMNotes = map foldlSent [
  [ch tempW `S.is` S "defined by", refS eBalanceOnWtr],
  [atStartNP (the input_), phrase constraint, eS $ sy tempInit $<= sy tempMeltP,
   S "comes from", refS assumpPIS],
  [atStartNP (the temp), S "remains constant at", ch tempMeltP `sC`
   S "even with the heating", sParen (S "or cooling") `sC` S "until the",
   phrase phaseChange, S "has occurred for all" `S.of_` S "the material; that" `S.is`
   S "as long as" +:+. eS (realInterval meltFrac $ Bounded (Exc, exactDbl 0) (Exc, exactDbl 1)), ch meltFrac,
   fromSource ddMeltFrac `S.is`
   S "determined as part" `S.ofThe` phrase heat, phrase energy `S.inThe`
   getAcc phsChgMtrl `sC` S "as given" `S.in_` sParen (refS heatEInPCM)],
  [ch tauSP `S.is` S "calculated" `S.in_` refS balanceSolidPCM],
  [ch tauLP `S.is` S "calculated" `S.in_` refS balanceLiquidPCM],
  [S "The initial", plural condition, S "for the", getAcc ode `S.are`
   eS (apply tempW [exactDbl 0] $= apply tempPCM [exactDbl 0] $= sy tempInit) `follows` assumpSITWP]] -- TODO: fix typing

 ----------------------------------------------
--    Derivation of eBalanceOnPCM          --
----------------------------------------------
eBalanceOnPCMDeriv :: Derivation
eBalanceOnPCMDeriv = mkDerivName (phraseNP (the energy) +:+
  S "balance on the PCM during sensible heating phase")
  (weave [eBalanceOnPCMDerivSentences, map eS eBalanceOnPCMDerivEqnsIM2]
  ++ [eBalanceOnPCMDerivDesc5, eBalanceOnPCMDerivDesc6, eBalanceOnPCMDerivDesc7])

eBalanceOnPCMDerivSentences :: [Sentence]
eBalanceOnPCMDerivSentences = [eBalanceOnPCMDerivDesc1, eBalanceOnPCMDerivDesc2,
  eBalanceOnPCMDerivDesc3, eBalanceOnPCMDerivDesc4]

eBalanceOnPCMDerivDesc1 :: Sentence
eBalanceOnPCMDerivDesc1 = foldlSentCol [
  S "To find the", phrase rOfChng `S.of_` ch tempPCM `sC` S "we look at the",
  phrase energy, S "balance on the" +:+. getAcc phsChgMtrl, S "The", phrase vol,
  S "being considered" `S.isThe` phrase pcmVol +:+. sParen (ch pcmVol),
  S "The derivation that follows is initially for the solid" +:+. getAcc phsChgMtrl,
  S "The" +:+. (phrase pcmMass `S.is` ch pcmMass `S.andThe` phrase htCapSP `S.is` ch htCapSP),
  atStartNP (the htFluxP) `S.is` ch htFluxP, sParen (refS htFluxPCMFromWater),
  S "over", phrase pcmSA +:+. ch pcmSA, S "The thermal flux is constant over",
  ch pcmSA `sC` S "since", phrase temp `S.the_ofThe` getAcc phsChgMtrl `S.isThe`
  S "same throughout its", phrase vol, sParen (refS assumpTPCAV) `S.andThe`
  phrase water `S.is` S "fully mixed" +:+. sParen (refS assumpCWTAT),
  S "There is no", phrase htFlux, phrase output_, S "from the" +:+. getAcc phsChgMtrl,
  S "Assuming no volumetric", phrase heat, S "generation per unit", phrase vol,
  sParen (refS assumpNIHGBWP) `sC` eS (sy volHtGen $= exactDbl 0) `sC`
  S "the equation for", refS rocTempSimp, S "can be written as"]

eBalanceOnPCMDerivDesc2 :: Sentence
eBalanceOnPCMDerivDesc2 = foldlSentCol [S "Using", refS htFluxPCMFromWater `S.for`
  ch htFluxP `sC` S "this", phrase equation, S "can be written as"]

eBalanceOnPCMDerivDesc3 :: Sentence
eBalanceOnPCMDerivDesc3 = foldlSentCol [S "Dividing by", ch pcmMass, ch htCapSP, S "we obtain"]

eBalanceOnPCMDerivDesc4 :: Sentence
eBalanceOnPCMDerivDesc4 = substitute [balanceSolidPCM]

eBalanceOnPCMDerivDesc5 :: Sentence
eBalanceOnPCMDerivDesc5 = foldlSent [
  eqN 4, S "applies for the", phrase solid +:+. getAcc phsChgMtrl, S "In the case where all of the",
  getAcc phsChgMtrl `S.is` S "melted" `sC` S "the same derivation applies" `sC` S "except that",
  htCapSP `isReplacedBy` htCapLP `sC` S "and thus" +:+. (tauSP `isReplacedBy` tauLP),
  S "Although a small change in", phrase surArea, S "would be expected with", phrase melting `sC`
  S "this is not included" `sC` S "since the", phrase vol, S "change of the", getAcc phsChgMtrl,
  S "with", phrase melting, S "is assumed to be negligible", sParen (refS assumpVCMPN)]
  where isReplacedBy a b = ch a `S.is` S "replaced by" +:+ ch b

eBalanceOnPCMDerivDesc6 :: Sentence
eBalanceOnPCMDerivDesc6 = foldlSent [
  S "In the case where", eS eq6_1 `S.and_` S "not all of the", getAcc phsChgMtrl `S.is`
  S "melted" `sC` S "the", phrase tempPCM +:+. S "does not change", S "Therefore" `sC` eq6_2]

eBalanceOnPCMDerivDesc7 :: Sentence
eBalanceOnPCMDerivDesc7 = foldlSent [
  S "This derivation does not consider", phrase boiling `S.the_ofThe` getAcc phsChgMtrl `sC`
  S "as the PCM is assumed to either be in a", phrase solid, S "state or a",
  phrase liquid, S "state", sParen (refS assumpNGSP)]

eq6_1 :: Expr
eq6_1 = sy tempPCM $= sy tempMeltP
eq6_2 :: Sentence
eq6_2 = foldlSent_ [S "d", ch tempPCM, S "/ d", ch time, S "= 0"]
{-
eq6_2 :: Expr
eq6_2 = (deriv (sy tempPCM) time) $= 0
-}

eBalanceOnPCMEqn1, eBalanceOnPCMEqn2, eBalanceOnPCMEqn3, eBalanceOnPCMEqn4 :: Expr

eBalanceOnPCMEqn1 = sy pcmMass `mulRe` sy htCapSP `mulRe` deriv (sy tempPCM) time $=
  sy htFluxP `mulRe` sy pcmSA

eBalanceOnPCMEqn2 = sy pcmMass `mulRe` sy htCapSP `mulRe` deriv (sy tempPCM) time $=
  sy pcmHTC `mulRe` sy pcmSA `mulRe` (sy tempW $- sy tempPCM)

eBalanceOnPCMEqn3 = deriv (sy tempPCM) time $=
  ((sy pcmHTC `mulRe` sy pcmSA) $/ (sy pcmMass `mulRe` sy htCapSP)) `mulRe`  (sy tempW $- sy tempPCM)

eBalanceOnPCMEqn4 = deriv (sy tempPCM) time $=
  recip_ (sy tauSP) `mulRe` (sy tempW $- sy tempPCM)

eBalanceOnPCMDerivEqnsIM2 :: [Expr]
eBalanceOnPCMDerivEqnsIM2 = [eBalanceOnPCMEqn1, eBalanceOnPCMEqn2,
 eBalanceOnPCMEqn3, eBalanceOnPCMEqn4]

---------
-- IM3 --
---------
heatEInWtr :: InstanceModel
heatEInWtr = imNoDeriv (othModel'  heatEInWtrRC)
  [qwUC tempInit, qwUC wMass, qwUC htCapW, qwUC wMass]
  (qw watE) [] [ref koothoor2013]
  "heatEInWtr" htWtrNotes

heatEInWtrRC :: RelationConcept
heatEInWtrRC = makeRC "heatEInWtrRC" (nounPhraseSP "Heat energy in the water")
  (watE ^. defn) htWtrRel -- heatEInWtrL

htWtrRel :: Relation
htWtrRel = apply1 watE time $= sy htCapW `mulRe` sy wMass `mulRe`
  (apply1 tempW time $- sy tempInit)

htWtrNotes :: [Sentence]
htWtrNotes = map foldlSent [
  [S "The above", phrase equation, S "is derived using", refS sensHtE],
  [atStartNP (NP.the (change `in_`temp)) `S.isThe` S "difference between the",
   phrase temp, S "at", phrase time, ch time, sParen (unwrap $ getUnit tInitMelt) `sC`
  ch tempW `S.andThe` phrase tempInit `sC` ch tempInit, sParen (unwrap $ getUnit tempInit)],
  [S "This", phrase equation, S "applies as long as",
   eS (realInterval tempW (Bounded (Exc, exactDbl 0) (Exc, exactDbl 100))) :+:
  unwrap (getUnit tempW), sParen $ refS assumpWAL `sC` refS assumpAPT]]

---------
-- IM4 --
---------
heatEInPCM :: InstanceModel
heatEInPCM = imNoDeriv (deModel' heatEInPCMRC) [qwC tempMeltP $ UpFrom (Exc, sy tempInit)
  , qwUC timeFinal, qwUC tempInit, qwUC pcmSA, qwUC pcmHTC
  , qwUC pcmMass, qwUC htCapSP, qwUC htCapLP, qwUC tempPCM, qwUC htFusion, qwUC tInitMelt]
  (qw pcmE)
  [] [ref koothoor2013]
  "heatEInPCM" htPCMNotes

heatEInPCMRC :: RelationConcept
heatEInPCMRC = makeRC "heatEInPCMRC" (nounPhraseSP "Heat energy in the PCM")
  (pcmE ^. defn) htPCMRel

htPCMRel :: Relation
htPCMRel = sy pcmE $= completeCase [case1, case2, case3]
  where case1 = (sy htCapSP `mulRe` sy pcmMass `mulRe` (apply1 tempPCM time $-
          sy tempInit), realInterval tempPCM (UpTo (Exc, sy tempMeltP)))

        case2 = (sy pcmInitMltE `addRe` (sy htFusion `mulRe` sy pcmMass) `addRe`
          (sy htCapLP `mulRe` sy pcmMass `mulRe` (apply1 tempPCM time $-
          sy tempMeltP)), realInterval tempPCM (UpFrom (Exc, sy tempMeltP)))

        case3 = (sy pcmInitMltE `addRe` apply1 latentEP time,
          sy tempPCM $= sy tempMeltP $&& realInterval meltFrac (Bounded (Exc, exactDbl 0) (Exc, exactDbl 1)))

htPCMNotes :: [Sentence]
htPCMNotes = map foldlSent [
  [S "The above", phrase equation `S.is` S "derived using",
   refS sensHtE `S.and_` refS latentHtE],
  [ch pcmE, S "for the", phrase solid, short phsChgMtrl, S "is found using",
   refS sensHtE `S.for` phrase sensHeat :+: S "ing, with",
   phraseNP (heatCapSpec `the_ofThe` solid), short phsChgMtrl `sC` ch htCapSP,
   sParen (unwrap $ getUnit htCapSP) `S.andThe` phrase change `S.inThe`
   short phsChgMtrl, phrase temp, S "from the", phrase tempInit, sParen (unwrap $ getUnit tempInit)],
  [ch pcmE, S "for the melted", short phsChgMtrl, sParen (eS (sy tempPCM $> sy pcmInitMltE)),
   S "is found using", refS sensHtE `S.for` phraseNP (sensHeat `ofThe` liquid),
   short phsChgMtrl, S "plus the", phrase energy, S "when", phrase melting, S "starts" `sC`
   S "plus", (phrase energy +:+ S "required to melt all") `S.the_ofThe` short phsChgMtrl],
  [atStartNP (the energy), S "required to melt all of the", short phsChgMtrl `S.is`
   eS (sy htFusion `mulRe` sy pcmMass), sParen (unwrap $ getUnit pcmInitMltE),
   fromSource ddHtFusion],
  [atStartNP (NP.the (change `in_` temp)) `S.is` eS (sy tempPCM $- sy tempMeltP),
   sParen (unwrap $ getUnit tempMeltP)],
  [ch pcmE, S "during", phrase melting `S.ofThe` short phsChgMtrl,
   S "is found using the", phrase energy, S "required at", S "instant" +:+
   phrase melting `S.the_ofThe` short phsChgMtrl, S "begins" `sC` ch pcmInitMltE, S "plus the",
   phrase latentHeat, phrase energy, S "added" `S.toThe` short phsChgMtrl `sC`
   ch latentEP, sParen (unwrap $ getUnit latentEP), S "since the", phrase time, S "when",
   phrase melting, S "began", ch tInitMelt, sParen (unwrap $ getUnit tInitMelt)],
  [atStartNP (NP.the (combineNINI heat energy)) `S.for` phrase boiling `S.ofThe` short phsChgMtrl,
   S "is not detailed" `sC` S "since the", short phsChgMtrl, S "is assumed to either be in a",
   phrase solid `S.or_` phrase liquid, S "state", sParen (refS assumpNGSP),
   sParen (refS assumpPIS)]]

-----------
-- Intro --
-----------

instModIntro :: Sentence
instModIntro = foldlSent [atStartNP' (the goal), foldlList Comma List
  (map refS [waterTempGS, pcmTempGS, waterEnergyGS, pcmEnergyGS]) `S.are`
  S "solved by" +:+. foldlList Comma List (map refS
  [eBalanceOnWtr, eBalanceOnPCM, heatEInWtr, heatEInPCM]), atStartNP' (the solution)
  `S.for` refS eBalanceOnWtr `S.and_`
  refS eBalanceOnPCM `S.are` S "coupled since the", plural solution
  `S.for` ch tempW `S.and_` ch tempPCM +:+. S "depend on one another",
  refS heatEInWtr, S "can be solved once", refS eBalanceOnWtr +:+.
  S "has been solved", atStartNP' (the solution) `S.of_` refS eBalanceOnPCM `S.and_`
  refS heatEInPCM `S.are` S "also coupled" `sC` S "since the",
  phrase tempPCM `S.andThe` phrase pcmE,S "depend on the", phrase phaseChange]

-- References --
iModRefs :: [Reference]
iModRefs = ref waterTempGS :map ref iMods
