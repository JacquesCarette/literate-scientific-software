{-# LANGUAGE PostfixOperators #-}
module Drasil.SSP.Body (srs, si, symbMap, printSetting, fullSI) where

import Data.List (nub)
import Language.Drasil hiding (Verb, number, organization, section, variable)
import Language.Drasil.Printers (PrintingInformation(..), defaultConfiguration)
import Database.Drasil (Block(Parallel), ChunkDB, ReferenceDB,
  SystemInformation(SI), cdb, rdb, refdb, _authors, _purpose, _concepts, _constants,
  _constraints, _datadefs, _instModels, _configFiles, _defSequence, _inputs,
  _kind, _outputs, _quants, _sys, _sysinfodb, _usedinfodb)
import Theory.Drasil (qdFromDD)

import Prelude hiding (sin, cos, tan)
import Utils.Drasil
import Utils.Drasil.Concepts
import qualified Utils.Drasil.NounPhrase as NP
import qualified Utils.Drasil.Sentence as S

import Drasil.DocLang (DocSection(..), IntroSec(..), IntroSub(..),
  LFunc(..), RefSec(..), RefTab(..), TConvention(..),
  TSIntro(..), Fields, Field(..), SRSDecl, SSDSec(..), SSDSub(..),
  Verbosity(..), InclUnits(..), DerivationDisplay(..), SolChSpec(..),
  SCSSub(..), GSDSec(..), GSDSub(..), TraceabilitySec(TraceabilityProg),
  ReqrmntSec(..), ReqsSub(..), AuxConstntSec(..), ProblemDescription(PDProg),
  PDSub(..), intro, mkDoc, tsymb'', traceMatStandard, purpDoc, getTraceConfigUID,
  secRefs, fillTraceSI, traceyGraphGetRefs)

import qualified Drasil.DocLang.SRS as SRS (inModel, assumpt,
  genDefn, dataDefn, datCon)

import Data.Drasil.Concepts.Documentation as Doc (analysis, assumption,
  constant, constraint, document, effect, endUser, environment,
  input_, interest, loss, method_, organization,
  physical, physics, problem, software,
  softwareSys, srsDomains, symbol_, sysCont, system,
  template, type_, user, value, variable, doccon, doccon')
import qualified Data.Drasil.Concepts.Documentation as Doc (srs)
import Data.Drasil.TheoryConcepts as Doc (inModel)
import Data.Drasil.Concepts.Education (solidMechanics, undergraduate, educon)
import Data.Drasil.Concepts.Math (equation, shape, surface, mathcon, mathcon',
  number)
import Data.Drasil.Concepts.PhysicalProperties (dimension, mass, physicalcon)
import Data.Drasil.Concepts.Physics (cohesion, fbd, force, gravity, isotropy,
  strain, stress, time, twoD, physicCon)
import Data.Drasil.Concepts.Software (program, softwarecon)
import Data.Drasil.Concepts.SolidMechanics (mobShear, normForce, shearForce, 
  shearRes, solidcon)
import Data.Drasil.Concepts.Computation (compcon, algorithm)
import Data.Drasil.Software.Products (sciCompS, prodtcon)
import Data.Drasil.Theories.Physics (physicsTMs)

import Data.Drasil.People (brooks, henryFrankis)
import Data.Drasil.Citations (koothoor2013, smithLai2005)
import Data.Drasil.SI_Units (degree, metre, newton, pascal, kilogram, second, derived, fundamentals)

import Drasil.SSP.Assumptions (assumptions, assumpRefs)
import Drasil.SSP.Changes (likelyChgs, unlikelyChgs, chgRefs)
import qualified Drasil.SSP.DataDefs as SSP (dataDefs, dataDefRefs)
import Drasil.SSP.Defs (acronyms, crtSlpSrf, defs, defs', effFandS, factor, fsConcept,
  intrslce, layer, morPrice, mtrlPrpty, plnStrn, slice, slip, slope, slpSrf, soil,
  soilLyr, soilMechanics, soilPrpty, ssa, ssp, stabAnalysis, waterTable)
import Drasil.SSP.Figures (figRefs)
import Drasil.SSP.GenDefs (generalDefinitions, genDefRefs)
import Drasil.SSP.Goals (goals, goalRefs)
import Drasil.SSP.IMods (instModIntro, iModRefs)
import qualified Drasil.SSP.IMods as SSP (iMods)
import Drasil.SSP.References (citations, morgenstern1965, citeRefs)
import Drasil.SSP.Requirements (funcReqs, funcReqTables, nonFuncReqs, reqRefs)
import Drasil.SSP.TMods (tMods, tModRefs)
import Drasil.SSP.Unitals (constrained, effCohesion, fricAngle, fs, index,
  inputs, inputsWUncrtn, outputs, symbols)

--Document Setup--

srs :: Document
srs = mkDoc mkSRS S.forT si

printSetting :: PrintingInformation
printSetting = PI symbMap Equational defaultConfiguration

fullSI :: SystemInformation
fullSI = fillTraceSI mkSRS si

resourcePath :: String
resourcePath = "../../../datafiles/SSP/"

si :: SystemInformation
si = SI {
  _sys         = ssp, 
  _kind        = Doc.srs, 
  _authors     = [henryFrankis, brooks],
  _purpose     = purpDoc ssp Verbose,
  _quants      = symbols,
  _concepts    = [] :: [DefinedQuantityDict],
  _instModels  = SSP.iMods,
  _datadefs    = SSP.dataDefs,
  _configFiles = [],
  _inputs      = map qw inputs,
  _outputs     = map qw outputs,
  _defSequence = [(\x -> Parallel (head x) (tail x)) $ map qdFromDD SSP.dataDefs],
  _constraints = constrained,
  _constants   = [],
  _sysinfodb   = symbMap,
  _usedinfodb  = usedDB,
   refdb       = refDB
}
  
mkSRS :: SRSDecl
mkSRS = [RefSec $ RefProg intro
  [TUnits, tsymb'' tableOfSymbIntro TAD, TAandA],
  IntroSec $ IntroProg startIntro kSent
    [ IPurpose $ purpDoc ssp Verbose
    , IScope scope
    , IChar []
        [phrase undergraduate +:+ S "level 4" +:+ phrase Doc.physics,
        phrase undergraduate +:+ S "level 2 or higher" +:+ phrase solidMechanics]
        [phrase soilMechanics]
    , IOrgSec orgSecStart inModel (SRS.inModel [] []) orgSecEnd
    ],
    --FIXME: issue #235
  GSDSec $ GSDProg
    [ SysCntxt [sysCtxIntro, LlC sysCtxFig1, sysCtxDesc, sysCtxList]
    , UsrChars [userCharIntro], SystCons [sysConstraints] []
    ],
  SSDSec $
    SSDProg
      [ SSDProblem $ PDProg prob []
        [ TermsAndDefs Nothing terms
        , PhySysDesc ssp physSystParts figPhysSyst physSystContents 
        , Goals goalsInputs]
      , SSDSolChSpec $ SCSProg
        [ Assumptions
        , TMs [] (Label : stdFields)
        , GDs [] ([Label, Units] ++ stdFields) ShowDerivation
        , DDs [] ([Label, Symbol, Units] ++ stdFields) ShowDerivation
        , IMs instModIntro ([Label, Input, Output, InConstraints, OutConstraints] ++ stdFields) ShowDerivation
        , Constraints EmptyS inputsWUncrtn --FIXME: issue #295
        , CorrSolnPpties outputs []
        ]
      ],
  ReqrmntSec $ ReqsProg
    [ FReqsSub' funcReqTables
    , NonFReqsSub
    ],
  LCsSec,
  UCsSec,
  TraceabilitySec $ TraceabilityProg $ traceMatStandard si,
  AuxConstntSec $ AuxConsProg ssp [],
  Bibliography]

units :: [UnitDefn]
units = map unitWrapper [metre, degree, kilogram, second] ++ map unitWrapper [newton, pascal]

concIns :: [ConceptInstance]
concIns = goals ++ assumptions ++ funcReqs ++ nonFuncReqs ++ likelyChgs ++ unlikelyChgs

section :: [Section]
section = extractSection srs

labCon :: [LabelledContent]
labCon = [figPhysSyst, figIndexConv, figForceActing] ++ funcReqTables

stdFields :: Fields
stdFields = [DefiningEquation, Description Verbose IncludeUnits, Notes, Source, RefBy]

-- SYMBOL MAP HELPERS --
symbMap :: ChunkDB
symbMap = cdb (map qw SSP.iMods ++ map qw symbols) (map nw symbols
  ++ map nw acronyms ++ map nw doccon ++ map nw prodtcon ++ map nw generalDefinitions ++ map nw SSP.iMods
  ++ map nw defs ++ map nw defs' ++ map nw softwarecon ++ map nw physicCon 
  ++ map nw physicsTMs
  ++ map nw mathcon ++ map nw mathcon' ++ map nw solidcon ++ map nw physicalcon
  ++ map nw doccon' ++ map nw derived ++ map nw fundamentals ++ map nw educon
  ++ map nw compcon ++ [nw algorithm, nw ssp] ++ map nw units)
  (map cw SSP.iMods ++ map cw symbols ++ srsDomains) units SSP.dataDefs SSP.iMods
  generalDefinitions tMods concIns section labCon allRefs

usedDB :: ChunkDB
usedDB = cdb ([] :: [QuantityDict]) (map nw symbols ++ map nw acronyms)
 ([] :: [ConceptChunk]) ([] :: [UnitDefn]) [] [] [] [] [] [] [] ([] :: [Reference])

refDB :: ReferenceDB
refDB = rdb citations concIns

-- SECTION 1 --
--automatically generated in mkSRS -

-- SECTION 1.1 --
--automatically generated in mkSRS

-- SECTION 1.2 --
--automatically generated in mkSRS using the intro below
tableOfSymbIntro :: [TSIntro]
tableOfSymbIntro = [TSPurpose, TypogConvention [Verb $ foldlSent_
  [S "a subscript", ch index, S "indicates that the", phrase value, 
  S "will be taken at, and analyzed at, a", phrase slice `S.or_` phrase slice, 
  S "interface composing the total slip", phrase mass]], VectorUnits]

-- SECTION 1.3 --
--automatically generated in mkSRS

-- SECTION 2 --
startIntro, kSent :: Sentence
startIntro = foldlSent [atStartNP (a_ slope), S "of geological",
  phrase mass `sC` S "composed of", phrase soil, S "and rock and sometimes",
  S "water, is subject to the influence" `S.of_` (phraseNP (gravity `onThe` mass) !.),
  S "This can cause instability in the form" `S.of_` phrase soil +:+.
  S "or rock movement", atStartNP' (NP.the (effect `of_PS` soil)),
  S "or rock movement can range from inconvenient to",
  S "seriously hazardous, resulting in significant life and economic" +:+.
  plural loss, atStart slope, S "stability is of", phrase interest,
  S "both when analysing natural", plural slope `sC`
  S "and when designing an excavated" +:+. phrase slope, atStart ssa `S.isThe`
  S "assessment" `S.ofThe` S "safety of a" +:+ phrase slope `sC`
  S "identifying the", phrase surface,
  S "most likely to experience", phrase slip `S.and_`
  S "an index of its relative stability known as the", phrase fs]

kSent = keySent ssa ssp

keySent :: (Idea a, Idea b) => a -> b -> Sentence
keySent probType pname = foldlSent_ [(phraseNP (NP.a_ (combineNINI probType problem)) !.),
  S "The developed", phrase program, S "will be referred to as the",
  introduceAbb pname]
  
-- SECTION 2.1 --
-- Purpose of Document automatically generated in IPurpose


-- SECTION 2.2 --
-- Scope of Requirements automatically generated in IScope
scope :: Sentence
scope = foldlSent_ [phraseNP (stabAnalysis `ofA` twoD), sParen (getAcc twoD),
  phraseNP (combineNINI soil mass) `sC` S "composed of a single homogeneous", phrase layer,
  S "with" +:+. pluralNP (combineNINI constant mtrlPrpty), atStartNP (NP.the (combineNINI soil mass))
  `S.is` S "assumed to extend infinitely in the third" +:+.
  phrase dimension, atStartNP (the analysis), S "will be at an instant" `S.in_`
  phrase time :+: S ";", plural factor, S "that may change the", plural soilPrpty,
  S "over", phrase time, S "will not be considered"]

-- SECTION 2.3 --
-- Characteristics of the Intended Reader generated in IChar

-- SECTION 2.4 --
-- Organization automatically generated in IOrgSec
orgSecStart, orgSecEnd :: Sentence
orgSecStart = foldlSent [atStartNP (the organization), S "of this",
  phrase document, S "follows the", phrase template, S "for an",
  short Doc.srs `S.for` phrase sciCompS,
  S "proposed by Koothoor", refS koothoor2013, S "as well as Smith" `S.and_`
  S "Lai", refS smithLai2005]
orgSecEnd   = foldlSent_ [atStartNP' (the inModel), S "provide the set of",
  S "algebraic", plural equation, S "that must be solved"]

-- SECTION 3 --
-- SECTION 3.1 --
-- System Context automatically generated
sysCtxIntro :: Contents
sysCtxIntro = foldlSP
  [refS sysCtxFig1 +:+ S "shows the" +:+. phrase sysCont,
   S "A circle represents an external entity outside the" +:+. phrase software, S "A rectangle represents the",
   phrase softwareSys, S "itself" +:+. sParen (short ssp),
   S "Arrows are used to show the data flow between the" +:+ phraseNP (system
   `andIts` environment)]
   
sysCtxFig1 :: LabelledContent
sysCtxFig1 = llcc (makeFigRef "sysCtxDiag") $ fig (titleize sysCont) (resourcePath ++ "SystemContextFigure.png")

sysCtxDesc :: Contents
sysCtxDesc = foldlSPCol
  [S "The responsibilities of the", phraseNP (user `andThe` system),
   S "are as follows"]
   
sysCtxUsrResp :: [Sentence]
sysCtxUsrResp = [S "Provide" +:+ phraseNP (the input_) +:+ S "data related to" +:+
  phraseNP (the soilLyr) :+: S "(s) and water table (if applicable)" `sC`
  S "ensuring conformation to" +:+ phrase input_ +:+ S "data format" +:+
  S "required by" +:+ short ssp,
  S "Ensure that consistent units are used for" +:+ pluralNP (combineNINI input_ variable),
  S "Ensure required" +:+ pluralNP (combineNINI software assumption) +:+ sParen ( 
  refS $ SRS.assumpt ([]::[Contents]) ([]::[Section])) +:+ S "are" +:+ 
  S "appropriate for the" +:+ phrase problem +:+ S "to which the" +:+ 
  phrase user +:+ S "is applying the" +:+ phrase software]
  
sysCtxSysResp :: [Sentence]
sysCtxSysResp = [S "Detect data" +:+ phrase type_ +:+ S "mismatch, such as" +:+
  S "a string of characters" +:+ phrase input_ +:+ S "instead of a floating" +:+
  S "point" +:+ phrase number,
  S "Verify that the" +:+ plural input_ +:+ S "satisfy the required" +:+
  phrase physical `S.and_` S "other data" +:+ plural constraint +:+ sParen (refS $ SRS.datCon ([]::[Contents]) ([]::[Section])),
  S "Identify the" +:+ phrase crtSlpSrf +:+ S "within the possible" +:+
  phrase input_ +:+ S "range",
  S "Find the" +:+ phrase fsConcept +:+ S "for the" +:+ phrase slope,
  S "Find the" +:+ phrase intrslce +:+ phraseNP (normForce `and_` shearForce) +:+ S "along the" +:+ phrase crtSlpSrf]
  
sysCtxResp :: [Sentence]
sysCtxResp = [titleize user +:+ S "Responsibilities",
  short ssp +:+ S "Responsibilities"]

sysCtxList :: Contents
sysCtxList = UlC $ ulcc $ Enumeration $ bulletNested sysCtxResp $
  map bulletFlat [sysCtxUsrResp, sysCtxSysResp]

-- SECTION 3.2 --
-- User Characteristics automatically generated with the
-- userContraints intro below

userCharIntro :: Contents
userCharIntro = userChar ssp [S "Calculus", titleize Doc.physics]
  [phrase soil, plural mtrlPrpty] [phrase effCohesion, phrase fricAngle, 
  S "unit weight"]

userChar :: (Idea a) => a -> [Sentence] -> [Sentence] -> [Sentence] -> Contents
userChar pname understandings familiarities specifics = foldlSP [
  atStartNP (the endUser) `S.of_` short pname,
  S "should have an understanding of undergraduate Level 1",
  foldlList Comma List understandings `sC`
  S "and be familiar with", foldlList Comma List familiarities `sC` 
  S "specifically", foldlList Comma List specifics]

-- SECTION 3.2 --
sysConstraints :: Contents
sysConstraints = foldlSP [atStartNP (NP.the (combineNINI morPrice method_)), 
  refS morgenstern1965 `sC` S "which involves dividing the", phrase slope,
  S "into vertical", plural slice `sC` S "will be used to derive the",
  plural equation, S "for analysing the", phrase slope]

-- SECTION 4 --

-- SECTION 4.1 --
prob :: Sentence
prob = foldlSent_ [S "evaluate the", phrase fs `S.ofA` phrasePoss slope,
  phrase slpSrf `S.and_` S "identify", phraseNP (crtSlpSrf `the_ofThe` slope) `sC`
  S "as well as the", phrase intrslce, phraseNP (normForce `and_` shearForce),
  S "along the", phrase crtSlpSrf]

{-
From when solution was used in Problem Description:
  It is intended to be used as an educational tool for introducing slope stability
  issues and to facilitate the analysis and design of a safe slope.
-}

-- SECTION 4.1.1 --
terms :: [ConceptChunk]
terms = [fsConcept, slpSrf, crtSlpSrf, waterTable, stress, strain, normForce,
  shearForce, mobShear, shearRes, effFandS, cohesion, isotropy, plnStrn]

  -- most of these are in concepts (physics or solidMechanics)
  -- except for fsConcept, crtSlpSrf & plnStrn which are in defs.hs

-- SECTION 4.1.2 --
physSystParts :: [Sentence]
physSystParts = map foldlSent [
  [atStartNP (a_ slope), S "comprised of one", phrase soilLyr],
  [atStartNP (a_ waterTable) `sC` S "which may or may not exist"]]

figPhysSyst :: LabelledContent
figPhysSyst = llcc (makeFigRef "PhysicalSystem") $
  fig (foldlSent_ [S "An example", phraseNP (slope `for` analysis),
  S "by", short ssp `sC` S "where the dashed line represents the",
  phrase waterTable]) (resourcePath ++ "PhysSyst.png")

physSystContents :: [Contents]
physSystContents = [physSysConv, LlC figIndexConv, physSysFbd, LlC figForceActing]

physSysConv :: Contents
physSysConv = foldlSP [atStart morPrice, phrase analysis, refS morgenstern1965
  `S.ofThe` phrase slope,  S "involves representing the", phrase slope,
  S "as a series of vertical" +:+. plural slice, S "As shown in",
  refS figIndexConv `sC` phraseNP (the index), ch index, S "is used to denote a",
  phrase value, S "for a single", phrase slice `sC` S "and an", phrase intrslce, 
  phrase value, S "at a given", phrase index, ch index, S "refers to the",
  phrase value, S "between", phrase slice, ch index `S.and_` S "adjacent", phrase slice,
  eS $ sy index `addI` int 1]

figIndexConv :: LabelledContent
figIndexConv = llcc (makeFigRef "IndexConvention") $ 
  fig (foldlSent_ [S "Index convention for", phraseNP (slice `and_` 
  intrslce), plural value]) (resourcePath ++ "IndexConvention.png")

physSysFbd :: Contents
physSysFbd = foldlSP [atStartNP' (NP.a_ (fbd `ofThe` force)), S "acting on a",
  phrase slice `S.is` S "displayed in" +:+. refS figForceActing, S "The specific",
  pluralNP (force `and_PP` symbol_), S "will be discussed in detail in",
  refS (SRS.genDefn [] []) `S.and_` refS (SRS.dataDefn [] [])]

figForceActing :: LabelledContent
figForceActing = llcc (makeFigRef "ForceDiagram") $
  fig (atStartNP' (fbd `of_` force) +:+ S "acting on a" +:+
  phrase slice) (resourcePath ++ "ForceDiagram.png")

-- SECTION 4.1.3 --
goalsInputs :: [Sentence]
goalsInputs = [phraseNP (the shape `NP.ofThe` combineNINI soil mass),
  S "location" `S.the_ofThe` phrase waterTable, pluralNP (mtrlPrpty `the_ofThePS` soil)]

-- SECTION 4.2 --

-- SECTION 4.2.1 --
-- Assumptions is automatically generated

-- SECTION 4.2.2 --
-- TModels is automatically generated

-- SECTION 4.2.3 --
-- General Definitions is automatically generated

-- SECTION 4.2.4 --
-- Data Definitions is automatically generated
--FIXME: derivations should be with the appropriate DDef

-- SECTION 4.2.5 --
-- Instance Models is automatically generated
--FIXME: derivations should be with the appropriate IMod

-- SECTION 4.2.6 --
-- Data Constraints is automatically generated

{-
{-input data-}
noTypicalVal, vertConvention :: Sentence
noTypicalVal   = short notApp
vertConvention = S "Consecutive vertexes have increasing x" +:+.
  plural value +:+ S "The start and end vertices of all layers" +:+
  S "go to the same x" +:+. plural value --Monotonicly increasing?

verticesConst :: Sentence -> [Sentence]
verticesConst vertexType = [vertVar vertexType, vertConvention,
  noTypicalVal, noTypicalVal, noTypicalVal]

waterVert, slipVert, slopeVert :: [Sentence]
waterVert = verticesConst $ S "water" +:+ phrase table_
slipVert  = verticesConst $ phrase slip
slopeVert = verticesConst $ phrase slope
-}

-- SECTION 4.2.7 --

-- SECTION 5 --

-- SECTION 5.1 --

-- SECTION 5.2 --

-- SECTION 6 --
--Likely Changes is automatically generated

-- SECTION 7 --
-- Table of aux consts is automatically generated

-- References --
bodyRefs :: [Reference]
bodyRefs = ref sysCtxFig1: map ref concIns ++ map ref section ++ map ref labCon ++ map (ref.makeTabRef.getTraceConfigUID) (traceMatStandard si) ++ traceyGraphGetRefs "SSP"

allRefs :: [Reference]
allRefs = nub (assumpRefs ++ bodyRefs ++ chgRefs ++ figRefs ++ goalRefs ++ SSP.dataDefRefs ++ genDefRefs
  ++ iModRefs ++ tModRefs ++ citeRefs ++ reqRefs ++ secRefs)
