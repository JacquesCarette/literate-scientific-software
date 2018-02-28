module Drasil.NoPCM.Body where

import Language.Drasil
import Data.Drasil.SI_Units
import Control.Lens ((^.))

import Drasil.NoPCM.DataDesc (inputMod)
import Drasil.NoPCM.Definitions (ht_trans, srs_swhs)
import Drasil.NoPCM.GenDefs (roc_temp_simp_deriv)

-- Since NoPCM is a simplified version of SWHS, the file is to be built off
-- of the SWHS libraries.  If the source for something cannot be found in
-- NoPCM, check SWHS.
import Drasil.SWHS.Assumptions (assump1, assump2, assump7, assump8, assump9,
  assump14, assump15, assump20)
import Drasil.SWHS.Body (charac_of_reader_knowledge, charac_of_reader_understanding, org_of_doc_intro,
  gen_sys_desc, physSyst1, physSyst2, data_def_intro_end, insta_model_d1startPara,
  trace_matrix_grph_trailing)
import Drasil.SWHS.Concepts (progName, water, gauss_div, sWHT, tank, coil,
  transient, perfect_insul, tank_para)
import Drasil.SWHS.Unitals (w_vol, tank_length, tank_vol, tau_W, temp_W,
  w_mass, diam, coil_SA, temp_C, w_density, htCap_W, time_final,
  in_SA, out_SA, vol_ht_gen, thFluxVect, ht_flux_in, ht_flux_out, tau, htCap_L,
  htTransCoeff, temp_env, diam, tank_length, ht_flux_C, coil_HTC,
  deltaT, w_E, tank_length_min, tank_length_max, htTransCoeff_min,
  w_density_min, w_density_max, htCap_W_min, htCap_W_max, coil_HTC_min,
  coil_HTC_max, time_final_max, sim_time, coil_SA_max, eta)
import Drasil.SWHS.DataDefs(swhsSymbMapDRef, swhsSymbMapTRef, dd1HtFluxC,
  data_def_DD1, swhsSymbMapT)
import Drasil.SWHS.TMods (theory_model_T1, t1ConsThermE, t1ConsThermE_new)
import Drasil.SWHS.GenDefs (swhsGenDefs, nwtnCooling, rocTempSimp)
import Drasil.SWHS.IMods (heatEInWtr, heatEInWtr_new)
import Drasil.NoPCM.IMods (eBalanceOnWtr, eBalanceOnWtr_new)
import Drasil.NoPCM.Unitals (temp_init)
import Drasil.SWHS.References (ref2, ref3, ref4, ref5, ref6)
import Drasil.SWHS.Requirements (non_func_req)
import Drasil.SWHS.LikelyChanges (likeChg2, likeChg3, likeChg6)

import Data.Drasil.People (thulasi)
import Data.Drasil.Utils (enumSimple, getES, refFromType,
  itemRefToSent, makeTMatrix, itemRefToSent, weave)

import Data.Drasil.Concepts.Documentation (datumConstraint, inModel,
  requirement, section_, traceyGraph, item, assumption, dataDefn,
  likelyChg, genDefn, thModel, traceyMatrix, model, acroNumGen,
  output_, quantity, input_, physicalConstraint, condition,
  property, variable, description, symbol_, uncertainty,
  information, uncertCol, value, column, softwareConstraint, goalStmt,
  physSyst, problem, definition, srs, content, reference, document,
  goal, purpose, typUnc)

import qualified Data.Drasil.Concepts.Math as M
import Data.Drasil.Concepts.Software (program)
import Data.Drasil.Concepts.Thermodynamics (ener_src, thermal_analysis, temp,
  thermal_energy, ht_trans_theo, heat, melt_pt, boil_pt, heat_trans, ht_flux,
  heat_cap_spec, thermal_conduction)
import qualified Data.Drasil.Quantities.Thermodynamics as QT (temp,
  heat_cap_spec, ht_flux)
import Data.Drasil.Quantities.Physics (time, energy)
import Data.Drasil.Quantities.PhysicalProperties (vol, mass, density)
import Data.Drasil.Quantities.Math (uNormalVect, surface, gradient)
import Data.Drasil.Software.Products (compPro)

import Drasil.Sections.ReferenceMaterial (intro)
import qualified Drasil.SRS as SRS (funcReq, likeChg, probDesc, goalStmt,
  inModel, missingP)
import Drasil.DocumentLanguage {-(DocDesc,
  tsymb, mkRequirement, mkLklyChnk, mkAssump, mkDoc,
  TSIntro (SymbOrder, SymbConvention, TSPurpose),
  DocSection (Verbatim, Bibliography, IntroSec, RefSec),
  RefTab (TAandA, TUnits),
  RefSec (RefProg),
  IntroSec (IntroProg),
  IntroSub (IOrgSec, IScope, IChar, IPurpose),
  Literature (Lit, Doc'))-}
import Drasil.DocumentLanguage.Definitions
import Drasil.DocumentLanguage.Chunk.GenDefn
  
import Drasil.Sections.SpecificSystemDescription (inDataConstTbl,
  outDataConstTbl, solChSpecF, dataConstraintUncertainty, physSystDesc,
  termDefnF, specSysDesF)
import Drasil.Sections.Requirements (reqF)
import Drasil.Sections.TraceabilityMandGs (traceGIntro, traceMGF)
import Drasil.Sections.AuxiliaryConstants (valsOfAuxConstantsF)

import Data.Drasil.SentenceStructures (showingCxnBw, foldlSent_, sAnd,
  foldlList, isThe, sOf, ofThe, foldlSPCol, foldlSent, foldlSP, acroIM,
  acroGD)
import Data.Drasil.Units.Thermodynamics (thermal_flux)

-- This defines the standard units used throughout the document
this_si :: [UnitDefn]
this_si = map UU [metre, kilogram, second] ++ map UU [centigrade, joule, watt]

-- This defines the list of acronyms that are used throughout the document
acronyms :: [CI]
acronyms = [assumption, dataDefn, genDefn, goalStmt, inModel, likelyChg, M.ode,
            physSyst, requirement, srs, progName, thModel, typUnc]

-- This contains the list of symbols used throughout the document
nopcm_Symbols :: [CQSWrapper]
nopcm_Symbols = (map cqs nopcm_Units) ++ (map cqs nopcm_Constraints)
  
nopcm_SymbolsAll :: [QWrapper] --FIXME: Why is PCM (swhsSymbolsAll) here?
                               --Can't generate without SWHS-specific symbols like pcm_HTC and pcm_SA
                               --FOUND LOC OF ERROR: Instance Models
nopcm_SymbolsAll = (map qs nopcm_Units) ++ (map qs nopcm_Constraints) ++
  (map qs specParamValList) ++ 
  (map qs [coil_SA_max]) ++ (map qs [tau_W]) ++ 
  (map qs [surface, uNormalVect, gradient, eta])

nopcm_Units :: [UCWrapper]
nopcm_Units = map ucw [density, tau, in_SA, out_SA,
  htCap_L, QT.ht_flux, ht_flux_in, ht_flux_out, vol_ht_gen,
  htTransCoeff, mass, tank_vol, QT.temp, QT.heat_cap_spec,
  deltaT, temp_env, thFluxVect, time, ht_flux_C,
  vol, w_mass, w_vol]

nopcm_Constraints :: [UncertQ]
nopcm_Constraints =  [coil_SA, w_E, htCap_W, coil_HTC, temp_init,
  time_final, tank_length, temp_C, w_density, diam, temp_W]

spec_sys_des, prob_des, termi_def, phy_sys_des, goal_stmt, sol_charac_spec,
  req, functional_req, likely_chg, trace_matrix_grph, aux_cons :: Section



-------------------
--INPUT INFORMATION
-------------------


--------------------------------
--Section 1 : REFERENCE MATERIAL
--------------------------------
  
mkSRS :: DocDesc
mkSRS = RefSec (RefProg intro
  [TUnits, tsymb [TSPurpose, SymbConvention
  [Lit (nw ht_trans), Doc' (nw progName)], SymbOrder], TAandA]) :
  IntroSec (IntroProg (intro_start ener_src energy progName)
    (intro_end progName program)
  [IPurpose (purpose_of_doc progName),
  IScope (scp_of_req_start thermal_analysis sWHT) (scp_of_req_end temp thermal_energy
    water),
  IChar (charac_of_reader_knowledge ht_trans_theo) (charac_of_reader_understanding M.de) EmptyS,
  IOrgSec org_of_doc_intro inModel (SRS.inModel SRS.missingP [])
  (org_of_doc_end inModel M.ode progName)]) : 
  Verbatim gen_sys_desc: --Verbatim gen_sys_desc:
  ------
  {-SSDSec 
    (SSDProg [SSDSubVerb prob_des
      , SSDSolChSpec 
        (SCSProg 
          [ TMs ([Label] ++ stdFields) [t1ConsThermE_new] -- only have the same T1 with SWHS
          , GDs [Label, Units, DefiningEquation   ---check glassbr
          , Description Verbose IncludeUnits
          , Source, RefBy] generalDefinitions ShowDerivation
          , DDs ([Label, Symbol, Units] ++ stdFields) [dd1HtFluxC] ShowDerivation
          , IMs ([Label, Input, Output, InConstraints, OutConstraints] ++ stdFields)
           [eBalanceOnWtr_new, heatEInWtr_new] HideDerivation
          ]
        )
      ]
    ): --Testing General Definitions.-}
  
  Verbatim spec_sys_des: -- Comment this out and the above in for testing GDs.
  map Verbatim [req, likely_chg, trace_matrix_grph, aux_cons] ++
  [Bibliography ref_refList]

stdFields :: Fields
stdFields = [DefiningEquation, Description Verbose IncludeUnits, Source, RefBy]
-- zzzzz  means the generalDefinations have a type of a list of GenDefn zzzzzzz -----
generalDefinitions :: [GenDefn]
generalDefinitions = [gd nwtnCooling (Just thermal_flux) ([] :: Attributes),
  gd rocTempSimp (Nothing :: Maybe DerUChunk) [D roc_temp_simp_deriv]]

nopcm_si :: SystemInformation
nopcm_si = SI {
  _sys = srs_swhs,
  _kind = srs,
  _authors = [thulasi],
  _units = this_si,
  _quants = nopcm_Symbols,
  _concepts = (nopcm_Symbols),
  _definitions = [dd1HtFluxC],          --dataDefs
  _inputs = (map qs nopcm_Constraints), --inputs
  _outputs = (map qs [temp_W, w_E]),     --outputs
  _defSequence = [Parallel dd1HtFluxC []],
  _constraints = (nopcm_Constraints),        --constrained
  _constants = [],
  _sysinfodb = nopcm_SymbMap
}

nopcm_code :: CodeSpec
nopcm_code = codeSpec' nopcm_si [inputMod]
-- Sub interpolation mod into list when possible              ^

nopcm_srs :: Document
nopcm_srs = mkDoc mkSRS (for) nopcm_si

nopcm_SymbMap :: ChunkDB
nopcm_SymbMap = cdb nopcm_SymbolsAll (map nw nopcm_Symbols ++ map nw acronyms) ([] :: [CWrapper]) -- FIXME: Fill in Concepts
  this_si

--------------------------
--Section 2 : INTRODUCTION
--------------------------
-- s2_start
intro_start :: ConceptChunk -> UnitalChunk -> CI-> Sentence
intro_start es en pro = foldlSent [S "Due to increasing cost, diminishing",
  S "availability, and negative environmental impact of",
  S "fossil fuels, there is a higher demand for renewable",
  plural es `sAnd` phrase en +:+. S "storage technology", 
  at_start' pro, S "provide a novel way of storing", phrase en]

-- s2_end
intro_end :: CI -> ConceptChunk -> Sentence
intro_end pro pr = foldlSent_ [EmptyS +:+. plural pro, S "The developed",
  phrase pr, S "will be referred to as", titleize pro,
  sParen (short pro)]

-----------------------------------
--Section 2.1 : PURPOSE OF DOCUMENT
-----------------------------------
--s2_1
purpose_of_doc :: CI -> Sentence
purpose_of_doc pro = foldlSent [S "The main", phrase purpose, S "of this",
  phrase document, S "is to describe the modelling of" +:+.
  phrase pro, S "The", plural goal `sAnd` plural thModel,
  S "used in the", short pro, S "code are provided, with an emphasis",
  S "on explicitly identifying", plural assumption, S "and unambiguous" +:+.
  plural definition, S "This", phrase document,
  S "is intended to be used as a", phrase reference,
  S "to provide ad hoc access to all", phrase information,
  S "necessary to understand and verify the" +:+. phrase model, S "The",
  short srs, S "is abstract because the", plural content, S "say what",
  phrase problem, S "is being solved, but do not say how to solve it"]

-------------------------------------
--Section 2.2 : SCOPE OF REQUIREMENTS
-------------------------------------
--s2_2_start
scp_of_req_start :: ConceptChunk -> ConceptChunk -> Sentence
scp_of_req_start ta sw = foldlSent_ [phrase ta, S "of a single", phrase sw]
--s2_2_end
scp_of_req_end :: ConceptChunk -> ConceptChunk -> ConceptChunk -> Sentence
scp_of_req_end tem te wa = foldlSent_ [S "predict the",
  phrase tem `sAnd` phrase te,
  S "histories for the", phrase wa]

--------------------------------------------------
--Section 2.3 : CHARACTERISTICS Of INTENDED READER
--------------------------------------------------
--s2_3kn
{-charac_of_reader_kn = foldlSent_ [phrase heat, S "transfer" +:+. phrase theory,
  S "A third or fourth year Mechanical Engineering course on this topic",
  S "is recommended"]
--s2_3un
charac_of_reader_un = foldlSent_ [S "differential", plural equation `sC`
  S "as typically covered in first and second year Calculus courses"]
--s2_3
charac_of_reader = charIntRdrF knowledge understanding sWHS EmptyS
  (SRS.userChar SRS.missingP [])
  -- FIXME: referencing this for now until we figure out how to reference
  -- auto-generated section (section 3.2)
  where knowledge = phrase heat +:+ S "transfer" +:+. phrase theory +:+
          S "A third or fourth year Mechanical Engineering" +:+
          S "course on this topic is recommended"

        understanding = S "differential" +:+ plural equation `sC`
          S "as typically covered in first and second year" +:+
          S "Calculus courses"-}
          
---------------------------------------
--Section 2.4: ORGANIZATION OF DOCUMENT
---------------------------------------
--s2_4s
{-org_of_doc_start = foldlSent [S "The", phrase organization, S "of this",
  phrase document, S "follows the template for an", short srs,
  S "for", phrase sciCompS, S "proposed by [2] and",
  sSqBr (S "5")]-}

--s2_4_end
org_of_doc_end :: CI -> CI -> CI -> Sentence
org_of_doc_end im od pro = foldlSent_ [S "The", phrase im,
  sParen (makeRef (SRS.inModel SRS.missingP [])),
  S "to be solved is referred to as" +:+. acroIM 1,
  S "The", phrase im, S "provides the",
  titleize od, sParen (short od), S "that model the"
  +:+. phrase pro, short pro, S "solves this", short od]

----------------------------------------
--Section 3 : GENERAL SYSTEM DESCRIPTION
----------------------------------------

--ALL OF THIS SECTION IS NOW PULLED FROM SWHS
--s3 = genSysF [sys_context] user_charac_intro [] []
--TODO: If/when system constraints recieves any content, add sys_constrt_intro
--to the first empty list

------------------------------
--Section 3.1 : SYSTEM CONTEXT
------------------------------
-- s3_1
-- sys_context = SRS.sysCont [sys_context_intro, sys_context_fig] []

--s3_1_intro
-- sys_context_intro = foldlSP [makeRef sys_context_fig, S "shows the" +:+.
  -- phrase sysCont, S "A circle represents an external entity outside the",
  -- phrase software `sC` S "the", phrase user,
  -- S "in this case. A rectangle represents the", phrase softwareSys,
  -- S "itself" +:+. sParen (getAcc progName), S "Arrows are used to show the",
  -- plural datum, S "flow between the", phrase section_,
  -- S "and its", phrase environment]
            
-- sys_context_fig = Figure (makeRef sys_context_fig :+: S ":" +:+
  -- titleize sysCont) "SystemContextFigure.png"
  
------------------------------------
--Section 3.2 : USER CHARACTERISTICS
------------------------------------
--s3_2_intro
-- user_charac_intro = foldlSP [S "The end", phrase user, S "of",
  -- short progName, S "should have an understanding of undergraduate",
  -- S "Level 1 Calculus and", titleize physics]

----------------------------------
--Section 3.3 : SYSTEM CONSTRAINTS
----------------------------------
--s3_3_intro
--sys_constrt_intro = Paragraph $ EmptyS

--TODO: Placeholder value until content can be added



-----------------------------------------
--Section 4 : SPECIFIC SYSTEM DESCRIPTION
-----------------------------------------

--TODO: finish filling in the subsections
--s4
spec_sys_des = specSysDesF (words_ sWHT) [prob_des, sol_charac_spec]
  where
  words_ sw = (plural definition `sAnd` S "finally the" +:+
    phrase inModel +:+ sParen (getAcc M.ode) +:+
    S "that" +:+ plural model +:+ S "the" +:+ phrase sw)

-----------------------------------
--Section 4.1 : PROBLEM DESCRIPTION
-----------------------------------
--s4_1  
prob_des = SRS.probDesc [prob_des_intro progName compPro water sWHT]
  [termi_def, phy_sys_des, goal_stmt]

--s4_1_intro
prob_des_intro :: CI -> NamedChunk -> ConceptChunk -> ConceptChunk -> Contents
prob_des_intro pro cp wa sw = foldlSP [getAcc pro, S "is a",
  phrase cp, S "developed to investigate",
  S "the heating of", phrase wa, S "in a", phrase sw]

--s4_1_1
termi_def = termDefnF Nothing [termi_def_bullets]

--s4_1_1_bullets
termi_def_bullets :: Contents
termi_def_bullets = Enumeration $ (Bullet $ map (\x -> Flat $
  (at_start x) :+: S ":" +:+ (x ^. defn))
  [ht_flux, heat_cap_spec, thermal_conduction, transient])

--s4_1_2  
phy_sys_des = physSystDesc (getAcc progName) fig_tank
  [phy_sys_des_list, fig_tank]

fig_tank :: Contents
fig_tank = fig (at_start sWHT `sC` S "with" +:+ phrase ht_flux +:+
  S "from" +:+ phrase coil `sOf` getES ht_flux_C)
  "TankWaterOnly.png"

--s4_1_2_list
phy_sys_des_list :: Contents
phy_sys_des_list = enumSimple 1 (short physSyst) $ map foldlSent_
  [physSyst1 tank water, physSyst2 coil tank ht_flux_C]

--s4_1_3
goal_stmt = SRS.goalStmt [goal_stmt_intro temp coil temp_W, goal_stmt_list temp_W w_E]
  []

--s4_1_3_intro
goal_stmt_intro :: ConceptChunk -> ConceptChunk -> UncertQ -> Contents
goal_stmt_intro te co temw = foldlSPCol [S "Given", phrase te `ofThe`
  phrase co `sC` S "initial", phrase temw  `sC` S "and material",
  plural property `sC` S "the", phrase goalStmt, S "are"]

--s4_1_3_list
goal_stmt_list :: UncertQ -> UncertQ -> Contents
goal_stmt_list temw we = enumSimple 1 (short goalStmt) [
  (S "predict the" +:+ phrase temw +:+ S "over time"),
  (S "predict the" +:+ phrase we +:+ S "over time")]

------------------------------------------------------
--Section 4.2 : SOLUTION CHARACTERISTICS SPECIFICATION
------------------------------------------------------
--s4_2  
sol_charac_spec = solChSpecF progName (prob_des, likely_chg) data_def_intro_end (mid,
  dataConstraintUncertainty, end) (assump_list, acroNumGen theory_model_T1 1,
  general_def_paragraph M.rOfChng temp, acroNumGen data_def_DD1 1,
  [swhsSymbMapT eBalanceOnWtr] ++ (insta_model_d1startPara energy water) ++
  intance_model_paragraph ++ [swhsSymbMapT heatEInWtr], [data_constraint_table1, data_constraint_table2])
  []
  where
  mid = foldlSent [S "The", phrase column, S "for",
    plural softwareConstraint, S "restricts the range of",
    plural input_, S "to reasonable", plural value]

  end = foldlSent [S "The", phrase uncertCol,
    S "provides an estimate of the confidence with which the physical",
    plural quantity, S "can be measured. This", phrase information,
    S "would be part of the input if one were performing an",
    phrase uncertainty, S "quantification exercise"]
--s4_2_1_list
assump_list :: [Contents]
assump_list = acroNumGen [assump1, assump2, assump3, assump4, assump5, assump7,
  assump8, assump9, assump9_npcm, assump14, assump15, assump12, assump13,
  assump20] 1
  
assump3, assump4, assump5, assump9_npcm, assump12, assump13 :: Contents

assump3 = mkAssump "assump3"
  (foldlSent [S "The", phrase water, S "in the", phrase tank,
  S "is fully mixed, so the", phrase temp_W `isThe`
  S "same throughout the entire", phrase tank, sSqBr (acroGD 2)])
assump4 = mkAssump "assump4"
  (foldlSent [S "The", phrase w_density, S "has no spatial variation; that is"
  `sC` S "it is constant over their entire", phrase vol, sSqBr ((acroGD 2)`sC`
  (acroTest likeChg2 likely_chg_list))])
assump5 = mkAssump "assump5"
  (foldlSent [S "The", phrase htCap_W, S "has no spatial variation; that", 
  S "is, it is constant over its entire", phrase vol, sSqBr (acroGD 2)])
assump9_npcm = mkAssump "assump9_npnc"
  (foldlSent [S "The", phrase model, S "only accounts for charging",
  S "of the tank" `sC` S "not discharging. The", phrase temp_W, S "can only",
  S "increase, or remain constant; it cannot decrease. This implies that the",
  phrase temp_init, S "is less than (or equal to) the", phrase temp_C,
  sSqBr ((acroIM 1) `sC` (acroTest likeChg3_npcm likely_chg_list))])
assump12 = mkAssump "assump12"
  (S "No internal" +:+ phrase heat +:+ S "is generated by the" +:+ phrase water
  `semiCol` S "therefore, the" +:+ phrase vol_ht_gen +:+ S "is zero" +:+.
  sSqBr (acroIM 1))
assump13 = mkAssump "assump13"
  (S "The pressure in the" +:+ phrase tank +:+ S "is atmospheric, so the" +:+
  phrase melt_pt `sAnd` phrase boil_pt +:+ S "are" +:+ S (show (0 :: Integer))
  :+: Sy (unit_symb QT.temp) `sAnd` S (show (100 :: Integer)) :+:
  Sy (unit_symb QT.temp) `sC` S "respectively" +:+.
  sSqBr ((acroIM 1) `sC` (acroIM 2)))

-- assumpNumGen :: [AssumpChunk] -> [Contents]
-- assumpNumGen assump =  zipWith Assumption assump [S "A" :+: (S $ show x) | x <- [1..]]


-- assump3 = [S "The", phrase water, S "in the", phrase tank,
  -- S "is fully mixed, so the", phrase temp_W `isThe`
  -- S "same throughout the entire", phrase tank,
  -- sSqBr (acroGD 2)]

-- assump4 = [S "The", phrase w_density, S "has no spatial variation; that is"
  -- `sC` S "it is constant over their entire", phrase vol, sSqBr (acroGD 2)]

-- assump5 = [S "The", phrase htCap_W, S "has no spatial variation; that",
  -- S "is, it is constant over its entire", phrase vol, sSqBr (acroGD 2)]

-- {-assump6 = [at_start law_conv_cooling,
  -- S "applies between the", phrase coil, S "and the",
  -- phrase water, sSqBr (swhsSymbMapDRef dd1HtFluxC)]-}

-- assump7_npcm = [S "The", phrase temp_C, S "is constant over",
  -- phrase time, sSqBr (swhsSymbMapDRef dd1HtFluxC `sC` acroLC 1)]

-- assump8 = [S "The", phrase temp_C, S "does not vary along its length",
  -- sSqBr (swhsSymbMapDRef dd1HtFluxC `sC` acroLC 2)]

---- TODO: Re-implement above when Data Definitions is created.

-- assump9 = [S "The", phrase model,
  -- S "only accounts for charging of the tank" `sC`
  -- S "not discharging. The", phrase temp_W, S "can only increase, or remain",
  -- S "constant; it cannot decrease. This implies that the",
  -- phrase temp_init, sParen (acroA 12), S "is less than (or equal)",
  -- S "to the", phrase temp_C, sSqBr ((acroIM 1) `sC` (acroLC 3))]

-- assump10 = [(S "operating" +:+ phrase temp +:+ S "range" `ofThe'`
  -- phrase system), S "is such that the", phrase water,
  -- S "is always in", phrase liquid, S "form. That is,",
  -- S "the", phrase temp, S "will not drop below the",
  -- phrase melt_pt, S "of", phrase water `sC` S "or rise above its",
  -- phrase boil_pt, sSqBr (acroIM 1)]

-- assump11 = [S "The", phrase tank, S "is perfectly insulated",
  -- S "so that there is no", phrase heat, S "loss from the",
  -- phrase tank, sSqBr ((acroIM 1) `sC` (acroLC 4))]

-- assump12 = [S "No internal", phrase heat,
  -- S "is generated by the", phrase water `semiCol` S "therefore, the",
  -- phrase vol_ht_gen, S "is zero", sSqBr (acroIM 1)]

-- s4_2_2_TMods
-- theory_model_TMods :: [Contents]
-- theory_model_TMods = map (Definition nopcmSymbMap . Theory) [t1consThermE]

--s4_2_3_paragraph
general_def_paragraph :: ConceptChunk -> ConceptChunk -> [Contents]
general_def_paragraph roc te = (map swhsSymbMapT swhsGenDefs) ++ [foldlSPCol
  [S "Detailed derivation of simplified", phrase roc, S "of", phrase te]] ++
  (weave [general_def_description, general_def_equation])

--s4_2_3_descrition
general_def_description :: [Contents]
general_def_description = map foldlSPCol [
  general_def_desc1 t1ConsThermE vol,
  general_def_desc2 gauss_div surface vol thFluxVect uNormalVect M.unit_,
  general_def_desc3 vol vol_ht_gen,
  general_def_desc4 ht_flux_in ht_flux_out in_SA out_SA density QT.heat_cap_spec
    QT.temp vol [assump3, assump4, assump5],
  general_def_desc5 density mass vol]

--s4_2_3_desc1
general_def_desc1 :: RelationConcept -> UnitalChunk -> [Sentence]
general_def_desc1 t1C vo =
  [S "Integrating", swhsSymbMapTRef t1C,
  S "over a", phrase vo, sParen (getES vo) `sC` S "we have"]

--s4_2_3_desc2
general_def_desc2 :: ConceptChunk -> ConVar -> UnitalChunk -> UnitalChunk ->
  ConVar -> ConceptChunk -> [Sentence]
general_def_desc2  g_d su vo tfv unv un =
  [S "Applying", titleize g_d, S "to the first term over",
  (phrase su +:+ getES su `ofThe` phrase vo) `sC` S "with",
  getES tfv, S "as the", phrase tfv, S "for the",
  phrase su `sAnd` getES unv, S "as a", phrase un,
  S "outward", phrase unv, S "for a", phrase su]

--s4_2_3_desc3
general_def_desc3 :: UnitalChunk -> UnitalChunk -> [Sentence]
general_def_desc3 vo vhg = [S "We consider an arbitrary" +:+. phrase vo, S "The",
  phrase vhg, S "is assumed constant. Then (1) can be written as"]

--s4_2_3_desc4
general_def_desc4 :: UnitalChunk -> UnitalChunk -> UnitalChunk -> UnitalChunk ->
  UnitalChunk -> UnitalChunk -> UnitalChunk -> UnitalChunk ->
  [Contents] -> [Sentence]
general_def_desc4 hfi hfo iS oS den hcs te vo assumps = [S "Where", getES hfi `sC`
  getES hfo `sC` getES iS `sC` S "and", getES oS, S "are explained in" +:+.
  acroGD 2, S "Assuming", getES den `sC` getES hcs `sAnd` getES te,
  S "are constant over the", phrase vo `sC` S "which is true in our case by",
  titleize' assumption, (foldlList $ (map (\d -> sParen (acroTest d assump_list)))
  assumps) `sC` S "we have"]

--s4_2_3_desc5
general_def_desc5 :: UnitalChunk -> UnitalChunk -> UnitalChunk -> [Sentence]
general_def_desc5 den ma vo = [S "Using the fact that", getES den :+: S "=" :+:
  getES ma :+: S "/" :+: getES vo `sC` S "(2) can be written as"]

general_def_eq1, general_def_eq2, general_def_eq3, general_def_eq4, general_def_eq5 :: Expr

--s4_2_3_eq1
general_def_eq1 = (negate (int_all (eqSymb vol) ((C gradient) $. (C thFluxVect)))) + 
  (int_all (eqSymb vol) (C vol_ht_gen)) $=
  (int_all (eqSymb vol) ((C density)
  * (C QT.heat_cap_spec) * Deriv Part (C QT.temp) time))

--s4_2_3_eq2
general_def_eq2 = (negate (int_all (eqSymb surface) ((C thFluxVect) $. (C uNormalVect)))) +
  (int_all (eqSymb vol) (C vol_ht_gen)) $= 
  (int_all (eqSymb vol)
  ((C density) * (C QT.heat_cap_spec) * Deriv Part (C QT.temp) time))

--s4_2_3_eq3
general_def_eq3 = (C ht_flux_in) * (C in_SA) - (C ht_flux_out) *
  (C out_SA) + (C vol_ht_gen) * (C vol) $= 
  (int_all (eqSymb vol) ((C density) * (C QT.heat_cap_spec) * Deriv Part (C QT.temp) time))

--s4_2_3_eq4
general_def_eq4 = (C density) * (C QT.heat_cap_spec) * (C vol) * Deriv Total
  (C QT.temp) time $= (C ht_flux_in) * (C in_SA) - (C ht_flux_out) *
  (C out_SA) + (C vol_ht_gen) * (C vol)

--s4_2_3_eq5
general_def_eq5 = (C mass) * (C QT.heat_cap_spec) * Deriv Total (C QT.temp)
  time $= (C ht_flux_in) * (C in_SA) - (C ht_flux_out)
  * (C out_SA) + (C vol_ht_gen) * (C vol)

--s4_2_3_equation
general_def_equation :: [Contents]
general_def_equation = map EqnBlock [general_def_eq1, general_def_eq2, general_def_eq3, 
  general_def_eq4, general_def_eq5]

--s4_2_5_paragraph
intance_model_paragraph :: [Contents]
intance_model_paragraph = weave [intance_model_description, intance_model_equation]

--TODO: Implement physical properties of a substance
--s4_2_5_description
intance_model_description :: [Contents]
intance_model_description = map foldlSPCol
  [intance_model_desc1 M.rOfChng temp_W energy water vol w_vol mass w_mass htCap_W
    heat_trans ht_flux_C coil_SA tank perfect_insul assump15 vol_ht_gen
    assump12,
  intance_model_desc2 dd1HtFluxC,
  intance_model_desc3 w_mass htCap_W,
  intance_model_desc4 tau_W w_mass htCap_W coil_HTC coil_SA]

--s4_2_5_desc1
intance_model_desc1 :: ConceptChunk -> UncertQ -> UnitalChunk -> ConceptChunk ->
  UnitalChunk -> UnitalChunk -> UnitalChunk -> UnitalChunk ->
  UncertQ -> ConceptChunk -> UnitalChunk -> UncertQ -> ConceptChunk ->
  ConceptChunk -> Contents -> UnitalChunk -> Contents -> [Sentence]
intance_model_desc1 roc temw en wa vo wv ma wm hcw ht hfc csa ta purin a11 vhg a12 =
  [S "To find the", phrase roc `sOf` getES temw `sC`
  S "we look at the", phrase en, S "balance on" +:+.
  phrase wa, S "The", phrase vo, S "being considered" `isThe`
  phrase wv, getES wv `sC` S "which has", phrase ma +:+.
  (getES wm `sAnd` (phrase hcw `sC` getES hcw)),
  at_start ht, S "occurs in the water from the coil as", (getES hfc
  `sC` S "over area") +:+. getES csa, S "No",
  phrase ht, S "occurs to", (S "outside" `ofThe`
  phrase ta) `sC` S "since it has been assumed to be",
  phrase purin +:+. sParen (acroTest a11 assump_list), S "Assuming no",
  phrase vhg +:+. (sParen (acroTest a12 assump_list) `sC`
  E (C vhg $= Int 0)), S "Therefore, the", phrase M.equation, S "for",
  acroGD 2, S "can be written as"]

--s4_2_5_desc2
intance_model_desc2 :: QDefinition -> [Sentence]
intance_model_desc2 d1hf = [S "Using", swhsSymbMapDRef d1hf `sC`
  S "this can be written as"]

--s4_2_5_desc3
intance_model_desc3 :: UnitalChunk -> UncertQ -> [Sentence]
intance_model_desc3 wm hcw = [S "Dividing (3) by", getES wm :+: getES hcw `sC`
  S "we obtain"]

--s4_2_5_desc4
intance_model_desc4 :: UnitalChunk -> UnitalChunk -> UncertQ -> UncertQ ->
  UncertQ -> [Sentence]
intance_model_desc4 temw wm hcw chtc csa = [S "Setting", (getES temw :+: S "=" :+:
  getES wm :+: getES hcw :+: S "/" :+: getES chtc :+: getES csa)
  `sC` titleize M.equation, S "(4) can be written in its final form as"]

--s4_2_5_equation
intance_model_equation :: [Contents]
intance_model_equation = map EqnBlock [intance_model_eq1, intance_model_eq2, 
  intance_model_eq3, intance_model_eq4]

intance_model_eq1, intance_model_eq2, intance_model_eq3, intance_model_eq4 ::Expr

--s4_2_5_eq1
intance_model_eq1 = (C w_mass) * (C htCap_W) * Deriv Total (C temp_W) time $=
  (C ht_flux_C) * (C coil_SA)

--s4_2_5_eq2 
intance_model_eq2 = (C w_mass) * (C htCap_W) * Deriv Total (C temp_W) time $=
  (C coil_HTC) * (C coil_SA) * ((C temp_C) - (C temp_W))

--s4_2_5_eq3
intance_model_eq3 = Deriv Total (C temp_W) time $= ((C coil_HTC) *
  (C coil_SA)) / ((C w_mass) * (C htCap_W)) * ((C temp_C) -
  (C temp_W))

--s4_2_5_eq4
intance_model_eq4 = Deriv Total (C temp_W) time $= (1 / (C tau_W)) *
  ((C temp_C) - (C temp_W))

--s4_2_6_table1
data_constraint_table1 :: Contents
data_constraint_table1 = inDataConstTbl data_constraint_conListIn
-- data_constraint_table1 = Table [S "Var", titleize' physicalConstraint, titleize software +:+
  -- titleize' constraint, S "Typical" +:+ titleize value, titleize uncertainty]
  -- (mkTable [(\x -> x!!0), (\x -> x!!1), (\x -> x!!2), (\x -> x!!3), (\x -> x!!4)]
  -- data_constraint_conListIn) (titleize input_ +:+ titleize' variable) True

--s4_2_6_conListIn
data_constraint_conListIn :: [UncertQ]
data_constraint_conListIn = [tank_length, diam, coil_SA, temp_C, w_density, htCap_W,
  coil_HTC, temp_init, time_final]

--s4_2_6_table2
data_constraint_table2 :: Contents
data_constraint_table2 = outDataConstTbl data_constraint_conListOut
-- data_constraint_table2 = Table [S "Var", titleize' physicalConstraint]
  -- (mkTable [(\x -> x!!0), (\x -> x!!1)] data_constraint_conListOut)
  -- (titleize output_ +:+ titleize' variable) True

--s4_2_6_conListOut
data_constraint_conListOut :: [UncertQ]
data_constraint_conListOut = [temp_W, w_E]

inputVar :: [QWrapper]
inputVar = map qs data_constraint_conListIn 




--------------------------
--Section 5 : REQUIREMENTS
--------------------------
--s5
req = reqF [functional_req, non_func_req]

---------------------------------------
--Section 5.1 : FUNCTIONAL REQUIREMENTS
---------------------------------------
--s5_1
functional_req = SRS.funcReq functional_req_list [] 
--TODO: Placeholder values until content can be added

--s5_1_list
functional_req_list :: [Contents]
functional_req_list = weave [functional_req_list_words_num, functional_req_list_items]

--s5_1_list_items
functional_req_list_items :: [Contents]
functional_req_list_items = [

  Table [titleize symbol_, titleize M.unit_, titleize description]
  (mkTable
  [getES,
  unit'2Contents,
  phrase] inputVar)
  (titleize input_ +:+ titleize variable +:+ titleize requirement) False,

  EqnBlock ((C w_mass) $= (C w_vol) * (C w_density) $=
  (((C diam) / 2) * (C tank_length) * (C w_density)))
  ]
--s5_1_list_words
-- functional_req_list_words = map (\x -> Enumeration $ Simple [x])
  -- $ mkEnumAbbrevList 1 (short requirement) $ map foldlSent_ [

  -- [titleize input_, S "the following", plural quantity `sC`
  -- S "which define the", phrase tank, S "parameters, material",
  -- plural property, S "and initial" +: plural condition],

  -- [S "Use the", plural input_, S "in", acroR 1, S "to find the",
  -- phrase mass, S "needed for", acroIM 1, S "to", acroIM 4 `sC`
  -- S "as follows, where", getES w_vol `isThe` phrase w_vol,
  -- S "and" +: (getES tank_vol `isThe` phrase tank_vol)],

  -- [S "Verify that the", plural input_, S "satisfy the required",
  -- phrase physicalConstraint, S "shown in" +:+. makeRef data_constraint_table1],

  -- [titleize' output_, S "and", plural input_, plural quantity, S "and derived",
  -- plural quantity, S "in the following list: the", plural quantity, S "from",
  -- (acroR 1) `sC` S "the", phrase mass, S "from", acroR 2, S "and", getES tau_W +:+.
  -- sParen(S "from" +:+ acroIM 1)],

  -- [S "Calculate and output the", phrase temp, S "of the", phrase water,
  -- sParen (getES temp_W :+: sParen (getES time)), S "over the", phrase simulation +:+.
  -- phrase time],

  -- [S "Calculate and", phrase output_, S "the", phrase w_E,
  -- sParen (getES w_E :+: sParen (getES time)), S "over the",
  -- phrase simulation, phrase time +:+. sParen (S "from" +:+ acroIM 3)]
  -- ]
--s5_1_list_words_num 
functional_req_list_words_num :: [Contents]
functional_req_list_words_num = acroNumGen [req1, req2, req3, req4, req5, req6] 1

req1, req2, req3, req4, req5, req6 :: Contents

--Empty list is supposed to take a ModuleChunk. Not sure what to put there.
req1 = mkRequirement "req1" $
  titleize input_ +:+ S "the following" +:+ plural quantity `sC`
  S "which define the" +:+ plural tank_para `sC` S "material" +:+
  plural property +:+ S "and initial" +: plural condition
req2 = mkRequirement "req2" $
  S "Use the" +:+ plural input_ +:+ S "in" +:+
  acroTest req1 functional_req_list_words_num +:+ S "to find the" +:+ phrase mass +:+
  S "needed for" +:+ acroIM 1 +:+ S "to" +:+ acroIM 2 `sC`
  S "as follows, where" +:+ getES w_vol `isThe` phrase w_vol +:+
  S "and" +: (getES tank_vol `isThe` phrase tank_vol)
req3 = mkRequirement "req3" $
  S "Verify that the" +:+ plural input_ +:+ S "satisfy the required"
  +:+ phrase physicalConstraint +:+ S "shown in" +:+. makeRef data_constraint_table1
req4 = mkRequirement "req4" $
  titleize' output_ `sAnd` plural input_ +:+ plural quantity
  +:+ S "and derived" +:+ plural quantity +:+ S "in the following list: the" +:+
  plural quantity +:+ S "from" +:+ (acroTest req1 functional_req_list_words_num) `sC`
  S "the" +:+ phrase mass +:+ S "from" +:+ acroTest req2 functional_req_list_words_num
  `sAnd` getES tau_W +:+. sParen(S "from" +:+ acroIM 1)
req5 = mkRequirement "req5" $
  S "Calculate and output the" +:+ phrase temp_W +:+
  sParen (getES temp_W :+: sParen (getES time)) +:+ S "over the" +:+
  phrase sim_time
req6 = mkRequirement "req6" $
  S "Calculate and" +:+ phrase output_ +:+ S "the" +:+ phrase w_E
  +:+ sParen (getES w_E :+: sParen (getES time)) +:+ S "over the" +:+
  phrase sim_time +:+. sParen (S "from" +:+ acroIM 3)

-------------------------------------------
--Section 5.2 : NON-FUNCTIONAL REQUIREMENTS
-------------------------------------------
-- s5_2
-- s5_2 = nonFuncReqF [performance] [correctness, verifiability,
  -- understandability, reusability, maintainability]
  -- (S "This problem is small in size and relatively simple")
  -- (S "Any reasonable implementation will be very quick and use minimal storage.")



----------------------------
--Section 6 : LIKELY CHANGES
----------------------------
--s6
likely_chg = SRS.likeChg likely_chg_list []

--s6_list
likely_chg_list :: [Contents]
likely_chg_list = acroNumGen [likeChg2, likeChg3, likeChg3_npcm, likeChg6] 1

-- likeChg1, likeChg2, likeChg3, likeChg4 :: Contents

---- Empty list is supposed to take a ModuleChunk. Not sure what to put there.
-- likeChg1 = LikelyChange (LCChunk (nw $ npnc "likeChg1" $
  -- nounPhraseSent (makeRef assump7 :+: S "- The" +:+ phrase temp_C +:+
  -- S "will change over" +:+ (S "course" `ofThe` S "day, depending") +:+
  -- S "on the" +:+ phrase energy +:+ S "received from the sun."))
  -- []) EmptyS
-- likeChg2 = LikelyChange (LCChunk (nw $ npnc "likeChg2" $
  -- nounPhraseSent (makeRef assump8 :+: S "- The" +:+ phrase temp_C +:+
  -- S "will actually change along its length as the" +:+ phrase water +:+
  -- S "within it cools."))
  -- []) EmptyS
likeChg3_npcm :: Contents
likeChg3_npcm = mkLklyChnk "likeChg3" $
  acroTest assump9_npcm assump_list :+: S "- The" +:+ phrase model +:+
  S "currently only accounts for charging of the tank. A more complete"
  +:+ phrase model +:+. S "would also account for discharging of the tank"
-- likeChg4 = LikelyChange (LCChunk (nw $ npnc "likeChg4" $
  -- nounPhraseSent (makeRef assump11 :+: S "- Any real" +:+ phrase tank +:+
  -- S "cannot be perfectly insulated and will lose" +:+. phrase heat))
  -- []) EmptyS

-- s6_list 
-- likely_chg_list = enumSimple 1 (short likelyChg) $ map foldlSent likely_chg__likeChg_list

-- s6_likeChg_list
-- likely_chg__likeChg_list :: [[Sentence]]
-- likeChg1, likeChg2, likeChg3, likeChg4 :: [Sentence]

-- likely_chg__likeChg_list = [likeChg1, likeChg2, likeChg3, likeChg4]

--s6_start
-- likeChg1 = [likely_chg_start 7, S "The", phrase temp_C,
  -- S "will change over", S "course" `ofThe` S "day, depending",
  -- S "on the", phrase energy, S "received from the sun"]

-- likeChg2 = [likely_chg_start 8, S "The", phrase temp_C,
  -- S "will actually change along its length as the",
  -- phrase water, S "within it cools"]

-- likeChg3 = [likely_chg_start 9, S "The", phrase model +:+.
  -- S "currently only accounts for charging of the tank",
  -- S "A more complete", phrase model, S "would also",
  -- S "account for discharging of the tank"]

-- likeChg4 = [likely_chg_start 11, S "Any real", phrase tank,
  -- S "cannot be perfectly insulated and will lose",
  -- phrase heat]



----------------------------------------------
--Section 7:  TRACEABILITY MATRICES AND GRAPHS
----------------------------------------------
--s7
trace_matrix_grph = traceMGF trace_matrix_grph_refList trace_matrix_grph_trailing
  ([trace_matrix_grph_table1, trace_matrix_grph_table2, trace_matrix_grph_table3] ++
  (trace_matrix_grph_intro2) ++ [trace_matrix_grph_fig1, trace_matrix_grph_fig2]) []

--s7_refList
trace_matrix_grph_refList :: [Contents]
trace_matrix_grph_refList = [trace_matrix_grph_table1, trace_matrix_grph_table2, 
  trace_matrix_grph_table3]

trace_matrix_grph_instaModel, trace_matrix_grph_data , trace_matrix_grph_funcReq, 
  trace_matrix_grph_likelyChg, trace_matrix_grph_dataDefs, trace_matrix_grph_genDefs,
  trace_matrix_grph_assump, trace_matrix_grph_theories :: [String]

trace_matrix_grph_dataRef, trace_matrix_grph_funcReqRef, trace_matrix_grph_instaModelRef, 
  trace_matrix_grph_assumpRef, trace_matrix_grph_theoriesRef,
  trace_matrix_grph_dataDefRef, trace_matrix_grph_likelyChgRef, trace_matrix_grph_genDefRef :: [Sentence]

--s7_instaModel
trace_matrix_grph_instaModel = ["IM1", "IM2"]
trace_matrix_grph_instaModelRef = map (refFromType Theory) [eBalanceOnWtr,
  heatEInWtr]

--s7_funcReq
trace_matrix_grph_funcReq = ["R1", "R2", "R3", "R4", "R5", "R6"]
trace_matrix_grph_funcReqRef = map (\x -> acroTest x functional_req_list_words_num)
  functional_req_list_words_num--makeListRef s7_funcReq functional_req

--s7_data
trace_matrix_grph_data = ["Data Constraints"]
trace_matrix_grph_dataRef = [makeRef data_constraint_table1] --FIXME: Reference section?

--s7_assump
trace_matrix_grph_assump = ["A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10",
  "A11", "A12", "A13", "A14"]
trace_matrix_grph_assumpRef = map (\x -> acroTest x assump_list) assump_list
--makeListRef trace_matrix_grph_assump (SRS.inModel SRS.missingP [])

--s7_theories
trace_matrix_grph_theories = ["T1"]
trace_matrix_grph_theoriesRef = map (refFromType Theory) [t1ConsThermE]

--s7_genDefs
trace_matrix_grph_genDefs = ["GD1", "GD2"]
trace_matrix_grph_genDefRef = map (refFromType Theory) swhsGenDefs

--s7_dataDefs
trace_matrix_grph_dataDefs = ["DD1"]
trace_matrix_grph_dataDefRef = map (refFromType Data) [dd1HtFluxC]

--s7_likelychg
trace_matrix_grph_likelyChg = ["LC1", "LC2", "LC3", "LC4"]
trace_matrix_grph_likelyChgRef = map (\x -> acroTest x likely_chg_list) likely_chg_list
--makeListRef trace_matrix_grph_likelyChg likely_chg

{-Traceability Matrix 1-}

--s7_row_t1
trace_matrix_grph_row_t1 :: [String]
trace_matrix_grph_row_t1 = trace_matrix_grph_theories ++ trace_matrix_grph_genDefs ++ 
  trace_matrix_grph_dataDefs ++ trace_matrix_grph_instaModel

--s7_row_header_t1
trace_matrix_grph_row_header_t1 :: [Sentence]
trace_matrix_grph_row_header_t1 = zipWith itemRefToSent trace_matrix_grph_row_t1
  (trace_matrix_grph_theoriesRef ++ trace_matrix_grph_genDefRef 
    ++ trace_matrix_grph_dataDefRef ++ trace_matrix_grph_instaModelRef)

--s7_columns_t1
trace_matrix_grph_columns_t1 :: [[String]]
trace_matrix_grph_columns_t1 = [trace_matrix_grph_t1_T1, trace_matrix_grph_t1_GD1, 
  trace_matrix_grph_t1_GD2, trace_matrix_grph_t1_DD1, trace_matrix_grph_t1_IM1, 
  trace_matrix_grph_t1_IM2]

--s7_t1_T1, s7_t1_GD1, s7_t1_GD2, s7_t1_DD1, s7_t1_IM1, s7_t1_IM2
trace_matrix_grph_t1_T1, trace_matrix_grph_t1_GD1, trace_matrix_grph_t1_GD2, 
 trace_matrix_grph_t1_DD1, trace_matrix_grph_t1_IM1, trace_matrix_grph_t1_IM2 :: [String]
--list of each item that "X" item requires for traceability matrix
trace_matrix_grph_t1_T1 = []
trace_matrix_grph_t1_GD1 = []
trace_matrix_grph_t1_GD2 = ["T1"]
trace_matrix_grph_t1_DD1 = ["GD1"]
trace_matrix_grph_t1_IM1 = ["GD2", "DD1"]
trace_matrix_grph_t1_IM2 = []

--s7_table1
trace_matrix_grph_table1 :: Contents
trace_matrix_grph_table1 = Table (EmptyS:trace_matrix_grph_row_header_t1)
  (makeTMatrix (trace_matrix_grph_row_header_t1) (trace_matrix_grph_columns_t1) 
    (trace_matrix_grph_row_t1))
  (showingCxnBw traceyMatrix
  (titleize' requirement `sAnd` titleize' inModel)) True

{-Traceability Matrix 2-}
--s7_row_t2
trace_matrix_grph_row_t2 :: [String]
trace_matrix_grph_row_t2 = trace_matrix_grph_instaModel ++ trace_matrix_grph_data
  ++ trace_matrix_grph_funcReq

--column header
--s7_row_header_t2
trace_matrix_grph_row_header_t2 :: [Sentence]
trace_matrix_grph_row_header_t2 = zipWith itemRefToSent trace_matrix_grph_row_t2
  (trace_matrix_grph_instaModelRef ++ trace_matrix_grph_dataRef ++ trace_matrix_grph_funcReqRef)

--row header
--s7_col_header_t2
trace_matrix_grph_col_header_t2 :: [Sentence]
trace_matrix_grph_col_header_t2 = zipWith itemRefToSent (trace_matrix_grph_instaModel 
  ++ trace_matrix_grph_funcReq)
  (trace_matrix_grph_instaModelRef ++ trace_matrix_grph_funcReqRef)

--s7_columns_t2
trace_matrix_grph_columns_t2 :: [[String]]
trace_matrix_grph_columns_t2 = [trace_matrix_grph_t2_IM1, trace_matrix_grph_t2_IM2,
 trace_matrix_grph_t2_R1, trace_matrix_grph_t2_R2, trace_matrix_grph_t2_R3, 
 trace_matrix_grph_t2_R4, trace_matrix_grph_t2_R5, trace_matrix_grph_t2_R6]

--s7_t2_IM1, s7_t2_IM2, s7_t2_R1, s7_t2_R2, s7_t2_R3, 
 --s7_t2_R4, s7_t2_R5, s7_t2_R6 :: [String]
trace_matrix_grph_t2_IM1, trace_matrix_grph_t2_IM2, trace_matrix_grph_t2_R1, 
  trace_matrix_grph_t2_R2, trace_matrix_grph_t2_R3, trace_matrix_grph_t2_R4, 
  trace_matrix_grph_t2_R5, trace_matrix_grph_t2_R6 :: [String]
--list of each item that "X" item requires for traceability matrix
trace_matrix_grph_t2_IM1 = []
trace_matrix_grph_t2_IM2 = []
trace_matrix_grph_t2_R1 = []
trace_matrix_grph_t2_R2 = ["R1","IM1"]
trace_matrix_grph_t2_R3 = ["Data Constraints"]
trace_matrix_grph_t2_R4 = ["R1", "R2", "IM1"]
trace_matrix_grph_t2_R5 = ["IM1"]
trace_matrix_grph_t2_R6 = ["IM2"]

--s7_table2
trace_matrix_grph_table2 :: Contents
trace_matrix_grph_table2 = Table (EmptyS:trace_matrix_grph_row_header_t2)
  (makeTMatrix (trace_matrix_grph_col_header_t2) (trace_matrix_grph_columns_t2) 
    (trace_matrix_grph_row_t2))
  (showingCxnBw traceyMatrix
  (titleize' requirement `sAnd` titleize' inModel)) True

{-Traceability Matrix 3-}
--s7_row_t3
trace_matrix_grph_row_t3 :: [String]
trace_matrix_grph_row_t3 = trace_matrix_grph_assump

--s7_row_header_t3
trace_matrix_grph_row_header_t3, trace_matrix_grph_col_header_t3 :: [Sentence]
trace_matrix_grph_row_header_t3 = zipWith itemRefToSent trace_matrix_grph_assump 
 trace_matrix_grph_assumpRef

--s7_col_header_t3
trace_matrix_grph_col_header_t3 = zipWith itemRefToSent
  (trace_matrix_grph_theories ++ trace_matrix_grph_genDefs ++ trace_matrix_grph_dataDefs ++ 
    trace_matrix_grph_instaModel ++ trace_matrix_grph_likelyChg)
  (trace_matrix_grph_theoriesRef ++ trace_matrix_grph_genDefRef ++ trace_matrix_grph_dataDefRef 
    ++ trace_matrix_grph_instaModelRef ++
  trace_matrix_grph_likelyChgRef)

--s7_columns_t3
--s7_t3_T1, s7_t3_GD1, s7_t3_GD2, s7_t3_DD1,
--  s7_t3_IM1, s7_t3_IM2, s7_t3_LC1, s7_t3_LC2, s7_t3_LC3, s7_t3_LC4
trace_matrix_grph_columns_t3 :: [[String]]
trace_matrix_grph_columns_t3 = [trace_matrix_grph_t3_T1, trace_matrix_grph_t3_GD1, 
  trace_matrix_grph_t3_GD2, trace_matrix_grph_t3_DD1, trace_matrix_grph_t3_IM1, 
  trace_matrix_grph_t3_IM2, trace_matrix_grph_t3_LC1, trace_matrix_grph_t3_LC2, 
  trace_matrix_grph_t3_LC3, trace_matrix_grph_t3_LC4]

trace_matrix_grph_t3_T1, trace_matrix_grph_t3_GD1, trace_matrix_grph_t3_GD2, 
  trace_matrix_grph_t3_DD1, trace_matrix_grph_t3_IM1, trace_matrix_grph_t3_IM2, 
  trace_matrix_grph_t3_LC1, trace_matrix_grph_t3_LC2, trace_matrix_grph_t3_LC3,
   trace_matrix_grph_t3_LC4 :: [String]

trace_matrix_grph_t3_T1  = ["A1"]
trace_matrix_grph_t3_GD1 = ["A2"]
trace_matrix_grph_t3_GD2 = ["A3", "A4", "A5"]
trace_matrix_grph_t3_DD1 = ["A6", "A7", "A8"]
trace_matrix_grph_t3_IM1 = ["A9", "A10"]
trace_matrix_grph_t3_IM2 = ["A10"]
trace_matrix_grph_t3_LC1 = ["A7"]
trace_matrix_grph_t3_LC2 = ["A8"]
trace_matrix_grph_t3_LC3 = ["A9"]
trace_matrix_grph_t3_LC4 = ["A11"]

--s7_table3
trace_matrix_grph_table3 :: Contents
trace_matrix_grph_table3 = Table (EmptyS:trace_matrix_grph_row_header_t3)
  (makeTMatrix trace_matrix_grph_col_header_t3 trace_matrix_grph_columns_t3 
    trace_matrix_grph_row_t3)
  (showingCxnBw traceyMatrix (titleize' assumption `sAnd` S "Other" +:+
  titleize' item)) True

-- These matrices can probably be generated automatically when enough info is
-- abstracted out.

------------------------
-- Traceabilty Graphs --
------------------------

tempName :: [CI]
tempName = [thModel, genDefn, dataDefn, inModel, likelyChg, assumption]

--s7_intro2
trace_matrix_grph_intro2 :: [Contents]
trace_matrix_grph_intro2 = traceGIntro [trace_matrix_grph_fig1, trace_matrix_grph_fig2]

  [(foldlList $ map plural tempName) +:+. S "on each other",

  foldlSent_ [plural inModel `sC` plural requirement `sC`
  S "and", plural datumConstraint, S "on each other"]]

-- s7_fig1
trace_matrix_grph_fig1 :: Contents
trace_matrix_grph_fig1 = fig (showingCxnBw traceyGraph (titleize' item +:+
  S "of Different" +:+ titleize' section_)) "ATrace.png"

--s7_fig2
trace_matrix_grph_fig2 :: Contents
trace_matrix_grph_fig2 = fig (showingCxnBw traceyGraph (titleize' requirement `sC`
  titleize' inModel `sC` S "and" +:+ titleize' datumConstraint)) "RTrace.png"

  -- Using the SWHS graphs as place holders until ones can be generated for NoPCM 



------------------------------------------
--Section 8: SPECIFICATION PARAMETER VALUE
------------------------------------------

specParamValList :: [QDefinition]
specParamValList = [tank_length_min, tank_length_max, htTransCoeff_min,
  w_density_min, w_density_max, htCap_W_min, htCap_W_max, coil_HTC_min,
  coil_HTC_max, time_final_max]

--s8
aux_cons = valsOfAuxConstantsF progName specParamValList



------------
--REFERENCES
------------
--
--s9
--ref = SRS.reference [ref_refs] []

--s9_refs
--ref_refs :: Contents

-- ref_refs = mkRefsList 1 $ map foldlsC ref_refList

--s9_refList
ref_refList :: BibRef
ref_refList = [ref2, ref3, ref4, ref5, ref6]
