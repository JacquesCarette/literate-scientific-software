module Drasil.SWHS.GenDefs (swhsGenDefs, nwtnCooling, rocTempSimp) where

import Prelude hiding (sin, cos, tan)

import Language.Drasil
import Drasil.DocumentLanguage.RefHelpers (refA)

import Data.Drasil.SentenceStructures (foldlSent)
import Data.Drasil.Quantities.PhysicalProperties as QPP (vol, mass)
import Data.Drasil.Quantities.Thermodynamics as QT (ht_flux, heat_cap_spec,
  temp)
import Data.Drasil.Quantities.Physics as QP (time)
import Drasil.SWHS.Unitals (vol_ht_gen, deltaT, temp_env, pcm_SA,
  out_SA, in_SA, ht_flux_in, ht_flux_out, htTransCoeff, thFluxVect)
import Data.Drasil.SentenceStructures (isThe, sAnd)
import Data.Drasil.Utils (unwrap)
import Data.Drasil.Concepts.Math (equation, rOfChng, rate)
import Data.Drasil.Concepts.Thermodynamics (law_conv_cooling)
import Drasil.SWHS.Assumptions (swhsRefDB, newA2)

---------------------------
--  General Definitions  --
---------------------------

swhsGenDefs :: [RelationConcept]
swhsGenDefs = [nwtnCooling, rocTempSimp]

--
nwtnCooling :: RelationConcept
nwtnCooling = makeRC "nwtnCooling" (nounPhraseSP "Newton's law of cooling") 
  nwtnCooling_desc nwtnCooling_rel Nothing--label

nwtnCooling_rel :: Relation
nwtnCooling_rel = apply1 thFluxVect QP.time $= sy htTransCoeff *
  apply1 deltaT QP.time

nwtnCooling_desc :: Sentence
nwtnCooling_desc = foldlSent [at_start law_conv_cooling +:+.
  S "describes convective cooling from a surface" +:
  S "The law is stated as", S "the", phrase rate,
  S "of heat loss from a body is proportional to the",
  S "difference in", plural temp, S "between the body" +:+.
  S "and its surroundings", E (apply1 thFluxVect QP.time) `isThe`
  S "thermal flux" +:+. sParen (Sy $ unit_symb thFluxVect),
  ch htTransCoeff `isThe` S "heat transfer coefficient" `sC`
  S "assumed independant of", ch QT.temp, sParen (refA swhsRefDB newA2) +:+.
  sParen (Sy $ unit_symb htTransCoeff),
  E (apply1 deltaT QP.time $= apply1 temp QP.time -
  apply1 temp_env QP.time) `isThe` S "time-dependant thermal gradient",
  S "between the environment and the object",
  sParen (Sy $ unit_symb deltaT)]

--
rocTempSimp :: RelationConcept
rocTempSimp = makeRC "rocTempSimp" (nounPhraseSP $ "Simplified rate " ++
  "of change of temperature") rocTempSimp_desc rocTempSimp_rel Nothing--label

rocTempSimp_rel :: Relation
rocTempSimp_rel = (sy QPP.mass) * (sy QT.heat_cap_spec) *
  deriv (sy QT.temp) QP.time $= sy ht_flux_in * sy in_SA -
  sy ht_flux_out * sy out_SA + sy vol_ht_gen * sy QPP.vol

rocTempSimp_desc :: Sentence
rocTempSimp_desc = foldlSent [S "The basic", phrase equation,
  S "governing the", phrase rOfChng, S "of", phrase temp `sC`
  S "for a given", phrase QPP.vol, ch QPP.vol `sC` S "with" +:+.
  phrase QP.time, ch QPP.mass `isThe` phrase QPP.mass +:+.
  sParen (Sy $ unit_symb QPP.mass), ch QT.heat_cap_spec `isThe` 
  phrase QT.heat_cap_spec +:+. sParen (Sy $ unit_symb QT.heat_cap_spec),
  ch temp `isThe` phrase temp, sParen (Sy $ unit_symb temp) `sAnd`
  ch QP.time `isThe` phrase QP.time +:+. sParen (Sy $ unit_symb QP.time),
  ch ht_flux_in `sAnd` ch ht_flux_out, S "are the in and out heat",
  S "transfer rates, respectively" +:+. sParen (Sy $ unit_symb QT.ht_flux),
  ch in_SA `sAnd` ch out_SA, S "are the surface areas over which the",
  S "heat is being transferred in and out, respectively" +:+.
  sParen (unwrap $ getUnit pcm_SA), ch vol_ht_gen `isThe`
  S "volumetric heat generated" +:+. sParen (Sy $ unit_symb vol_ht_gen),
  ch QPP.vol `isThe` phrase QPP.vol, sParen (Sy $ unit_symb QPP.vol)]
