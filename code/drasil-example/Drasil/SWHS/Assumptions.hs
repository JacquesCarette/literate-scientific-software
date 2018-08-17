module Drasil.SWHS.Assumptions where --all of this file is exported

import Language.Drasil
import Control.Lens ((^.))

import Data.Drasil.Concepts.Documentation (system, simulation, model, 
  problem)

import Data.Drasil.Quantities.PhysicalProperties (vol)
import Data.Drasil.Quantities.Physics (energy, time)
import Data.Drasil.Quantities.Thermodynamics (boil_pt, melt_pt, temp)

import Data.Drasil.Concepts.Thermodynamics as CT (heat, melting,
  law_conv_cooling, heat_trans, thermal_energy)
import Data.Drasil.Concepts.PhysicalProperties (solid, liquid, gaseous)
import Data.Drasil.Concepts.Math (change)
import Data.Drasil.Concepts.Physics (mech_energy)

import Data.Drasil.SentenceStructures (foldlSent, ofThe, ofThe', sAnd, isThe)

import Drasil.SWHS.Concepts (coil, tank, phsChgMtrl, water, perfect_insul,
  charging, discharging)
import Drasil.SWHS.DataDefs (dd1HtFluxC, dd2HtFluxP)
import Drasil.SWHS.Labels (thermalEnergyOnlyL, lawConvectiveCoolingWtrPCML, 
  waterAlwaysLiquidL, noGaseousStatePCML, atmosphericPressureTankL, 
  nwtnCoolingL, rocTempSimpL, eBalanceOnWtrL, eBalanceOnPCML, heatEInWtrL, 
  heatEInPCML, likeChg1L, likeChg2L, likeChg3L, likeChg4L, likeChg5L,
  likeChg6L,inputInitQuantsL, thermalEnergyOnlyL, heatTransferCoeffL,
  contantWaterTempL, tempPcmConsL, densityWaterL, specificHeatL,
  newtoLawConvecL, tempOverTimeL, tempOverLengthL, lawConvectiveCoolingWtrPCML,
  chargeTankL, sameInitialL, pcmInitialSolidL, waterAlwaysLiquidL,
  perfectInsulationL, noInternalHeatL, volumeChangeMeltL, noGaseousStatePCML,
  atmosphericPressureTankL, volumeCoilL)
import Drasil.SWHS.References (swhsCitations)
import Drasil.SWHS.TMods (t1ConsThermE)
import Drasil.SWHS.Unitals (w_vol, vol_ht_gen, temp_C, temp_init, temp_W,
  temp_PCM, htCap_L_P, htCap_W, htCap_S_P, w_density, pcm_density, pcm_vol)

-------------------------
-- 4.2.1 : Assumptions --
-------------------------
swhsRefDB :: ReferenceDB
swhsRefDB = rdb [] [] newAssumptions [] [] swhsCitations []

newAssumptions :: [AssumpChunk]
newAssumptions = [newA1, newA2, newA3, newA4, newA5, newA6, newA7, newA8, newA9, newA10,
  newA11, newA12, newA13, newA14, newA15, newA16, newA17, newA18, newA19, newA20]

newA1, newA2, newA3, newA4, newA5, newA6, newA7, newA8, newA9, newA10,
  newA11, newA12, newA13, newA14, newA15, newA16, newA17, newA18, newA19, newA20 :: AssumpChunk

newA1  = assump "Thermal-Energy-Only"                       assumpS1  thermalEnergyOnlyL
newA2  = assump "Heat-Transfer-Coeffs-Constant"             assumpS2  heatTransferCoeffL
newA3  = assump "Constant-Water-Temp-Across-Tank"           assumpS3  contantWaterTempL
newA4  = assump "Temp-PCM-Constant-Across-Volume"           assumpS4  tempPcmConsL
newA5  = assump "Density-Water-PCM-Constant-over-Volume"    assumpS5  densityWaterL 
newA6  = assump "Specific-Heat-Energy-Constant-over-Volume" assumpS6  specificHeatL
newA7  = assump "Law-Convective-Cooling-Coil-Water"         assumpS7  newtoLawConvecL
newA8  = assump "Temp-Heating-Coil-Constant-over-Time"      assumpS8  tempOverTimeL
newA9  = assump "Temp-Heating-Coil-Constant-over-Length"    assumpS9  tempOverLengthL
newA10 = assump "Law-Convective-Cooling-Water-PCM"          assumpS10 lawConvectiveCoolingWtrPCML
newA11 = assump "Charging-Tank-No-Temp-Discharge"           assumpS11 chargeTankL
newA12 = assump "Same-Initial-Temp-Water-PCM"               assumpS12 sameInitialL
newA13 = assump "PCM-Initially-Solid"                       assumpS13 pcmInitialSolidL
newA14 = assump "Water-Always-Liquid"                       assumpS14 waterAlwaysLiquidL
newA15 = assump "Perfect-Insulation-Tank"                   assumpS15 perfectInsulationL
newA16 = assump "No-Internal-Heat-Generation-By-Water-PCM"  assumpS16 noInternalHeatL
newA17 = assump "Volume-Change-Melting-PCM-Negligible"      assumpS17 volumeChangeMeltL
newA18 = assump "No-Gaseous-State-PCM"                      assumpS18 noGaseousStatePCML
newA19 = assump "Atmospheric-Pressure-Tank"                 assumpS19 atmosphericPressureTankL
newA20 = assump "Volume-Coil-Negligible"                    assumpS20 volumeCoilL

swhsAssumptionsS:: [Sentence]
swhsAssumptionsS = [assumpS1, assumpS2, assumpS3, assumpS4, assumpS5,
  assumpS6, assumpS7, assumpS8, assumpS9, assumpS10, assumpS11, assumpS12, assumpS13, assumpS14,
  assumpS15, assumpS16, assumpS17, assumpS18, assumpS19, assumpS20]

assumpS1, assumpS2, assumpS3, assumpS4, assumpS5, assumpS6, assumpS7,
  assumpS8, assumpS9, assumpS10, assumpS11, assumpS12, assumpS13, assumpS14,
  assumpS15, assumpS16, assumpS17, assumpS18, assumpS19, assumpS20 :: Sentence

assumpS1 = foldlSent [
  S "The only form of", phrase energy, S "that is",
  S "relevant for this", phrase problem, S "is" +:+. 
  phrase CT.thermal_energy, S "All other forms of", phrase energy `sC`
  S "such as", phrase mech_energy `sC` S "are assumed to be negligible",
  sSqBr $ makeRef t1ConsThermE]
assumpS2 = foldlSent [
  S "All", phrase CT.heat_trans, S "coefficients are constant over",
  phrase time, sSqBr $ makeRef nwtnCoolingL]
assumpS3 = foldlSent [
  S "The", phrase water, S "in the", phrase tank,
  S "is fully mixed, so the", phrase temp_W `isThe` 
  S "same throughout the entire", phrase tank,
  sSqBr $ makeRef rocTempSimpL `sC` makeRef dd2HtFluxP]
assumpS4 = foldlSent [
  S "The", phrase temp_PCM `isThe` S "same throughout the", phrase pcm_vol,
  sSqBr $ makeRef rocTempSimpL `sC` makeRef dd2HtFluxP `sC` makeRef likeChg1L]
  --FIXME `sC` makeRef likeChg1]
assumpS5 = foldlSent [
  S "The", phrase w_density `sAnd` phrase pcm_density,
  S "have no spatial variation; that is" `sC`
  S "they are each constant over their entire", phrase vol,
  sSqBr $ makeRef rocTempSimpL]
assumpS6 = foldlSent [
  S "The", phrase htCap_W `sC` phrase htCap_S_P `sC` S "and",
  phrase htCap_L_P, S "have no spatial variation; that",
  S "is" `sC` S "they are each constant over their entire",
  phrase vol, sSqBr $ makeRef rocTempSimpL]
assumpS7 = foldlSent [
  CT.law_conv_cooling ^. defn, S "applies between the",
  phrase coil `sAnd` S "the", phrase water,
  sSqBr $ makeRef dd1HtFluxC]
assumpS8 = foldlSent [
  S "The", phrase temp_C, S "is constant over", phrase time,
  sSqBr $ makeRef dd1HtFluxC `sC` makeRef likeChg2L]
assumpS9 = foldlSent [
  S "The", phrase temp_C, S "does not vary along its length",
  sSqBr $ makeRef dd1HtFluxC `sC` makeRef likeChg3L]
assumpS10 = foldlSent [
  CT.law_conv_cooling ^. defn, S "applies between the",
  phrase water `sAnd` S "the", short phsChgMtrl,
  sSqBr $ makeRef dd2HtFluxP]
assumpS11 = foldlSent [
  S "The", phrase model, S "only accounts for", (charging ^. defn) `sC`
  S "not" +:+. phrase discharging, S "The", phrase temp_W `sAnd`
  phrase temp_PCM, S "can only increase, or remain",
  S "constant; they do not decrease. This implies that the",
  phrase temp_init, sSqBr $ makeRef newA12, S "is less than (or equal)",
  S "to the", phrase temp_C, sSqBr $ makeRef eBalanceOnWtrL
   `sC` makeRef likeChg4L]
assumpS12 = foldlSent [
  phrase temp_init `ofThe'` phrase water `sAnd` S "the",
  short phsChgMtrl `isThe` S "same",
  sSqBr $ makeRef eBalanceOnWtrL `sC` makeRef eBalanceOnPCML
  `sC` makeRef likeChg5L]
assumpS13 = foldlSent [
  S "The", phrase simulation, S "will start with the",
  short phsChgMtrl, S "in a", solid ^. defn,
  sSqBr $ makeRef eBalanceOnPCML `sC` makeRef heatEInPCML]
assumpS14 = foldlSent [
  (S "operating" +:+ phrase temp +:+ S "range") `ofThe'` phrase system,
  S "is such that the", phrase water,
  S "is always in" +:+. (liquid ^. defn), S "That is" `sC`
  S "the", phrase temp, S "will not drop below the",
  phrase melt_pt, S "of", phrase water `sC` S "or rise above its",
  phrase boil_pt, sSqBr $ makeRef eBalanceOnWtrL `sC` makeRef heatEInWtrL]
assumpS15 = foldlSent [
  S "The", phrase tank, S "is", phrase perfect_insul,
  S "so that there is no", phrase CT.heat, S "loss from the",
  phrase tank, sSqBr $ makeRef eBalanceOnWtrL
  `sC` makeRef likeChg6L]
assumpS16 = foldlSent [
  S "No internal", phrase CT.heat, S "is generated by either the",
  phrase water, S "or the", short phsChgMtrl `semiCol`
  S "therefore, the", phrase vol_ht_gen, S "is zero",
  sSqBr $ makeRef eBalanceOnWtrL `sC` makeRef eBalanceOnPCML]
assumpS17 = foldlSent [
  (phrase vol +:+ phrase change) `ofThe'` short phsChgMtrl,
  S "due to", phrase CT.melting, S "is negligible", sSqBr $ makeRef eBalanceOnPCML]
assumpS18 = foldlSent [
  S "The", short phsChgMtrl, S "is either in a", liquid ^. defn,
  S "or a", solid ^. defn, S "but not a", gaseous ^. defn,
  sSqBr $ makeRef eBalanceOnPCML `sC` makeRef heatEInPCML]
assumpS19 = foldlSent [
  S "The pressure in the", phrase tank, S "is atmospheric, so the",
  phrase melt_pt `sAnd` phrase boil_pt, S "are", S (show (0 :: Integer)) :+:
  Sy (unit_symb temp) `sAnd` S (show (100 :: Integer)) :+:
  Sy (unit_symb temp) `sC` S "respectively", sSqBr $ makeRef eBalanceOnWtrL `sC` makeRef heatEInWtrL]
assumpS20 = foldlSent [
  S "When considering the", phrase w_vol, S "in the",
  phrase tank `sC` (phrase vol `ofThe` phrase coil),
  S "is assumed to be negligible", sSqBr $ makeRef inputInitQuantsL]
  --FIXME , sSqBr $ makeRef req2]

--- Again, list structure is same between all examples.
-- Can booktabs colored links be used? The box links completely cover nearby
-- punctuation.
