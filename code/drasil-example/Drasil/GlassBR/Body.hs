module Drasil.GlassBR.Body where

import Control.Lens ((^.))
import qualified Data.Map as Map
import Language.Drasil hiding (organization)
import Language.Drasil.Code (CodeSpec, codeSpec, relToQD)
import Language.Drasil.Printers (PrintingInformation(..), defaultConfiguration)
import Database.Drasil (ChunkDB, RefbyMap, ReferenceDB, SystemInformation(SI),
  TraceMap, cdb, collectUnits, generateRefbyMap, rdb, refdb, _authors,
  _concepts, _constants, _constraints, _datadefs, _definitions, _defSequence,
  _inputs, _kind, _outputs, _quants, _sys, _sysinfodb, _usedinfodb)
import Theory.Drasil (GenDefn)

import Drasil.DocLang (AppndxSec(..), AuxConstntSec(..), DerivationDisplay(..), 
  DocDesc, DocSection(..), Field(..), Fields, GSDSec(GSDProg2), GSDSub(..), 
  InclUnits(IncludeUnits), IntroSec(IntroProg), IntroSub(IChar, IOrgSec, IPurpose, IScope), 
  LCsSec'(..), ProblemDescription(..), RefSec(RefProg), RefTab(TAandA, TUnits), 
  ReqrmntSec(..), ReqsSub(FReqsSub, NonFReqsSub'), SCSSub(..),
  SSDSec(..), SSDSub(..), SolChSpec(..), StkhldrSec(StkhldrProg2), 
  StkhldrSub(Client, Cstmr), TraceabilitySec(TraceabilityProg), 
  TSIntro(SymbOrder, TSPurpose), UCsSec(..), Verbosity(Verbose),
  dataConstraintUncertainty, goalStmtF, inDataConstTbl, intro, mkDoc, 
  outDataConstTbl, physSystDesc, termDefnF, traceGIntro, tsymb, generateTraceMap,
  getTraceMapFromTM, getTraceMapFromGD, getTraceMapFromDD, getTraceMapFromIM, getSCSSub,
  generateTraceTable, characteristicsLabel, physSystDescriptionLabel,
  generateTraceMap', mkEnumSimpleD)

import qualified Drasil.DocLang.SRS as SRS (datCon, reference, valsOfAuxCons,
  assumpt, inModel)

import Data.Drasil.Concepts.Computation (computerApp, inParam, compcon, algorithm)
import Data.Drasil.Concepts.Documentation as Doc (analysis, appendix, aspect, 
  assumption, characteristic, code, company, condition, content,
  dataConst, dataDefn, definition, document, emphasis, environment, figure, 
  goal, implementation, information, inModel, input_, interface, item, 
  likelyChg, model, organization, output_, physicalSystem, physSyst, problem, 
  product_, purpose, reference, requirement, section_, software, softwareSys,
  srs, srsDomains, standard, sysCont, system, template, term_, thModel,
  traceyMatrix, user, value, doccon, doccon')
import Data.Drasil.Concepts.Education as Edu(civilEng, scndYrCalculus, structuralMechanics,
  educon)
import Data.Drasil.Concepts.Math (graph, parameter, mathcon, mathcon')
import Data.Drasil.Concepts.PhysicalProperties (dimension, physicalcon, materialProprty)
import Data.Drasil.Concepts.Physics (distance)
import Data.Drasil.Concepts.Software (correctness, verifiability,
  understandability, reusability, maintainability, portability,
  performance, softwarecon)
import Data.Drasil.Software.Products (sciCompS)

import Data.Drasil.Citations (koothoor2013, smithLai2005)
import Data.Drasil.People (mCampidelli, nikitha, spencerSmith)
import Data.Drasil.Phrase (for'', the)
import Data.Drasil.SI_Units (kilogram, metre, newton, pascal, second, fundamentals,
  derived)
import Data.Drasil.SentenceStructures (FoldType(List), SepType(Comma), 
  figureLabel, foldlList, foldlsC, foldlSent, foldlSP, foldlSPCol, 
  isThe, ofThe, sAnd, showingCxnBw, sIn, sOf, sOr, sVersus, tAndDOnly, tAndDWAcc, tAndDWSym, 
  underConsidertn)
import Data.Drasil.Utils (bulletFlat, bulletNested, enumBullet, enumSimple, itemRefToSent, 
  makeTMatrix, noRefs)
  
import Drasil.GlassBR.Assumptions (assumptionConstants, assumptions)
import Drasil.GlassBR.Changes (likelyChgs, unlikelyChgs,
  unlikelyChgsList)
import Drasil.GlassBR.Concepts (acronyms, aR, blastRisk, glaPlane, glaSlab, gLassBR, 
  ptOfExplsn, stdOffDist, glasscon, glasscon')
import Drasil.GlassBR.DataDefs (dataDefns, gbQDefns)
import Drasil.GlassBR.Goals (goals)
import Drasil.GlassBR.IMods (glassBRsymb, gbrIMods, calofDemandi, instModIntro)
import Drasil.GlassBR.ModuleDefs (allMods)
import Drasil.GlassBR.References (astm2009, astm2012, astm2016, gbCitations, rbrtsn2012)
import Drasil.GlassBR.Requirements (funcReqsList, funcReqs, nonfuncReqs,
  inputGlassPropsTable, propsDeriv)
import Drasil.GlassBR.Symbols (symbolsForTable, thisSymbols)
import Drasil.GlassBR.TMods (gbrTMods)
import Drasil.GlassBR.Unitals (aspect_ratio, blast, blastTy, bomb, charWeight,
  demand, demandq, dimlessLoad, explosion, gbConstants, gbConstrained, gbInputDataConstraints,
  gbInputs, gbOutputs, gBRSpecParamVals, glassTy, glassTypes, glBreakage,
  lateralLoad, load, loadTypes, pbTol, probBr, probBreak, sD, stressDistFac,
  termsWithAccDefn, termsWithDefsOnly, terms)

{--}

gbSymbMap :: ChunkDB
gbSymbMap = cdb thisSymbols (map nw acronyms ++ map nw thisSymbols ++ map nw glasscon
  ++ map nw glasscon' ++ map nw terms ++ map nw doccon ++ map nw doccon' ++ map nw educon
  ++ [nw sciCompS] ++ map nw compcon ++ map nw mathcon ++ map nw mathcon'
  ++ map nw softwarecon ++ map nw terms ++ [nw lateralLoad, nw materialProprty]
   ++ [nw distance, nw algorithm] ++
  map nw fundamentals ++ map nw derived ++ map nw physicalcon)
  (map cw glassBRsymb ++ Doc.srsDomains) (map unitWrapper [metre, second, kilogram]
  ++ map unitWrapper [pascal, newton]) glassBRLabel glassBRRefby
  glassBRDatadefn glassBRInsModel glassBRGenDef glassBRTheory glassBRConcIns
  glassBRSection glassBRLabelledCon

glassBRLabel :: TraceMap
glassBRLabel = Map.union (generateTraceMap mkSRS) $ generateTraceMap' glassBRConcIns
 
glassBRRefby :: RefbyMap
glassBRRefby = generateRefbyMap glassBRLabel 

glassBRDatadefn :: [DataDefinition]
glassBRDatadefn = getTraceMapFromDD $ getSCSSub mkSRS

glassBRInsModel :: [InstanceModel]
glassBRInsModel = getTraceMapFromIM $ getSCSSub mkSRS

glassBRGenDef :: [GenDefn]
glassBRGenDef = getTraceMapFromGD $ getSCSSub mkSRS

glassBRTheory :: [TheoryModel]
glassBRTheory = getTraceMapFromTM $ getSCSSub mkSRS

glassBRConcIns :: [ConceptInstance]
glassBRConcIns = assumptions ++ likelyChgs ++ unlikelyChgs ++ funcReqs

glassBRSection :: [Section]
glassBRSection = glassBRSec

glassBRLabelledCon :: [LabelledContent]
glassBRLabelledCon = [inputGlassPropsTable]

glassBRSec :: [Section]
glassBRSec = extractSection glassBRSrs

usedDB :: ChunkDB
usedDB = cdb ([] :: [QuantityDict]) (map nw acronyms ++ map nw thisSymbols ++ map nw checkSi)
 ([] :: [ConceptChunk]) checkSi glassBRLabel glassBRRefby
  glassBRDatadefn glassBRInsModel glassBRGenDef glassBRTheory glassBRConcIns
  glassBRSection glassBRLabelledCon

gbRefDB :: ReferenceDB
gbRefDB = rdb gbCitations glassBRConcIns

printSetting :: PrintingInformation
printSetting = PI gbSymbMap defaultConfiguration

checkSi :: [UnitDefn]
checkSi = collectUnits gbSymbMap thisSymbols 

resourcePath :: String
resourcePath = "../../../datafiles/GlassBR/"

glassBRSrs :: Document
glassBRSrs = mkDoc mkSRS (for'' titleize phrase) glassSystInfo

mkSRS :: DocDesc
mkSRS = [RefSec $ RefProg intro [TUnits, tsymb [TSPurpose, SymbOrder], TAandA],
  IntroSec $
    IntroProg (startIntro software blstRskInvWGlassSlab gLassBR)
      (short gLassBR)
    [IPurpose $ purpOfDocIntro document gLassBR glaSlab,
     IScope incScoR endScoR,
     IChar [] (undIR ++ appStanddIR) [],
     IOrgSec orgOfDocIntro dataDefn (SRS.inModel [] []) orgOfDocIntroEnd],
  StkhldrSec $
    StkhldrProg2
      [Client gLassBR $ S "a" +:+ phrase company
        +:+ S "named Entuitive. It is developed by Dr." +:+ (S $ name mCampidelli),
      Cstmr gLassBR],
  GSDSec $ GSDProg2 [SysCntxt [sysCtxIntro, LlC sysCtxFig1, sysCtxDesc, sysCtxList],
    UsrChars [userCharacteristicsIntro], SystCons [] [] ],
  SSDSec $
    SSDProg
      [SSDProblem $ PDProg probStart gLassBR probEnding [termsAndDesc, physSystDescription, goalStmts],
       SSDSolChSpec $ SCSProg
        [ Assumptions
        , TMs [] (Label : stdFields) gbrTMods
        , GDs [] [] [] HideDerivation -- No Gen Defs for GlassBR
        , DDs [] ([Label, Symbol, Units] ++ stdFields) dataDefns ShowDerivation
        , IMs [instModIntro] ([Label, Input, Output, InConstraints, OutConstraints] ++ stdFields) [calofDemandi] HideDerivation
        , Constraints EmptyS dataConstraintUncertainty
                      (foldlSent [makeRef2S $ SRS.valsOfAuxCons [] [],
                      S "gives", (plural value `ofThe` S "specification"),
                      plural parameter, S "used in", makeRef2S inputDataConstraints])
                      [inputDataConstraints, outputDataConstraints]
        , CorrSolnPpties propsDeriv
        ]
      ],
  ReqrmntSec $ ReqsProg [
    FReqsSub funcReqsList,
    NonFReqsSub' [performance] nonfuncReqs
    (S "This problem is small in size and relatively simple")
    (S "Any reasonable" +:+ phrase implementation +:+.
    (S "will be very quick" `sAnd` S "use minimal storage"))
  ],
  LCsSec' $ LCsProg' likelyChgs,
  UCsSec $ UCsProg unlikelyChgsList,
  TraceabilitySec $
    TraceabilityProg traceyMatrices [traceMatsAndGraphsTable1Desc, traceMatsAndGraphsTable2Desc, traceMatsAndGraphsTable3Desc]
    ((map LlC traceyMatrices) ++ traceMatsAndGraphsIntro2 ++ (map LlC traceyGraphs)) [],
  AuxConstntSec $ AuxConsProg gLassBR auxiliaryConstants,
  Bibliography,
  AppndxSec $ AppndxProg [appdxIntro, LlC fig_5, LlC fig_6]]
 
stdFields :: Fields
stdFields = [DefiningEquation, Description Verbose IncludeUnits, Notes, Source, RefBy]

glassSystInfo :: SystemInformation
glassSystInfo = SI {
  _sys         = gLassBR,
  _kind        = srs,
  _authors     = [nikitha, spencerSmith],
  _quants      = symbolsForTable,
  _concepts    = [] :: [DefinedQuantityDict],
  _definitions = (map (relToQD gbSymbMap) gbrIMods) ++ 
                 (concatMap (^. defined_quant) gbrTMods) ++
                 (concatMap (^. defined_fun) gbrTMods),
  _datadefs    = dataDefns,
  _inputs      = map qw gbInputs,
  _outputs     = map qw gbOutputs,
  _defSequence = gbQDefns,
  _constraints = gbConstrained,
  _constants   = gbConstants,
  _sysinfodb   = gbSymbMap,
  _usedinfodb = usedDB,
   refdb       = gbRefDB
}
  --FIXME: All named ideas, not just acronyms.

glassBRCode :: CodeSpec
glassBRCode = codeSpec glassSystInfo allMods

termsAndDesc, physSystDescription, goalStmts :: Section

physSystDescriptionList, appdxIntro :: Contents

inputDataConstraints, outputDataConstraints, traceMatsAndGraphsTable1, traceMatsAndGraphsTable2, 
  traceMatsAndGraphsTable3, figGlassbr, fig_2, fig_3, fig_4, fig_5, fig_6 :: LabelledContent

--------------------------------------------------------------------------------
termsAndDescBullets :: Contents
termsAndDescBullets = UlC $ ulcc $ Enumeration$ 
  Numeric $
  noRefs $ map tAndDOnly termsWithDefsOnly
  ++
  termsAndDescBulletsGlTySubSec
  ++
  termsAndDescBulletsLoadSubSec
  ++
  map tAndDWAcc termsWithAccDefn
  ++
  [tAndDWSym probBreak probBr]
   --FIXME: merge? Needs 2 arguments because there is no instance for (SymbolForm ConceptChunk)...

termsAndDescBulletsGlTySubSec, termsAndDescBulletsLoadSubSec :: [ItemType]

termsAndDescBulletsGlTySubSec = [Nested (titleize glassTy :+: S ":") $
  Bullet $ noRefs $ map tAndDWAcc glassTypes]

termsAndDescBulletsLoadSubSec = [Nested (at_start load :+: S "-" +:+ (load ^.defn)) $
  Bullet $ noRefs $ (map tAndDWAcc $ take 2 loadTypes)
  ++
  (map tAndDOnly $ drop 2 loadTypes)]

--Used in "Goal Statements" Section--

goalStmtsList :: [Contents]
goalStmtsList = mkEnumSimpleD goals

--Used in "Traceability Matrices and Graphs" Section--

traceyMatrices :: [LabelledContent]
traceyMatrices = [traceTable1, traceMatsAndGraphsTable1, traceMatsAndGraphsTable2, traceMatsAndGraphsTable3]

traceyGraphs :: [LabelledContent]
traceyGraphs = [fig_2, fig_3, fig_4]

solChSpecSubsections :: [CI]
solChSpecSubsections = [thModel, inModel, dataDefn, dataConst]

--Used in "Values of Auxiliary Constants" Section--
auxiliaryConstants :: [QDefinition]
auxiliaryConstants = assumptionConstants ++ gBRSpecParamVals

--Used in "Non-Functional Requirements" Section--
gBRpriorityNFReqs :: [ConceptChunk]
gBRpriorityNFReqs = [correctness, verifiability, understandability,
  reusability, maintainability, portability]

--------------------------------------------------------------------------------

{--INTRODUCTION--}

startIntro :: NamedChunk -> Sentence -> CI -> Sentence
startIntro prgm sfwrPredicts progName = foldlSent [
  at_start prgm, S "is helpful to efficiently" `sAnd` S "correctly predict the"
  +:+. sfwrPredicts, underConsidertn blast,
  S "The", phrase prgm `sC` S "herein called", short progName `sC`
  S "aims to predict the", sfwrPredicts, S "using an intuitive",
  phrase interface]

undIR, appStanddIR :: [Sentence]
undIR = [phrase scndYrCalculus, phrase structuralMechanics, phrase glBreakage,
  phrase blastRisk, plural computerApp `sIn` phrase Edu.civilEng]
appStanddIR = [S "applicable" +:+ plural standard +:+
  S "for constructions using glass from" +:+ (foldlList Comma List
  $ map makeCiteS [astm2009, astm2012, astm2016]) `sIn`
  (makeRef2S $ SRS.reference ([]::[Contents]) ([]::[Section]))]

incScoR, endScoR :: Sentence
incScoR = foldl (+:+) EmptyS [S "getting all", plural inParam,
  S "related to the", phrase glaSlab `sAnd` S "also the", plural parameter,
  S "related to", phrase blastTy]
endScoR = foldl (+:+) EmptyS [S "predicts whether a", phrase glaSlab, 
  S "is safe" `sOr` S "not"]

{--Purpose of Document--}

purpOfDocIntro :: NamedChunk -> CI -> NamedChunk -> Sentence
purpOfDocIntro typeOf progName gvnVar = foldlSent [S "The main", phrase purpose,
  S "of this", phrase typeOf, S "is to predict whether a given", phrase gvnVar,
  S "is likely to resist a specified" +:+. phrase blast, S "The", plural Doc.goal
  `sAnd` plural thModel, S "used in the", short progName, phrase code,
  S "are provided" `sC` S "with an", phrase emphasis,
  S "on explicitly identifying", (plural assumption) `sAnd` S "unambiguous" +:+.
  plural definition, S "This", phrase typeOf, S "is intended to be used as a",
  phrase reference, S "to provide all", phrase information,
  S "necessary to understand" `sAnd` S "verify the" +:+. phrase analysis,
  S "The", short srs, S "is abstract because the", plural content, S "say what",
  phrase problem, S "is being solved" `sC` S "but not how to solve it"]
  --FIXME: Last sentence is also present in SSP, SWHS and NoPCM... pull out?

{--Scope of Requirements--}

{--Organization of Document--}

orgOfDocIntro, orgOfDocIntroEnd :: Sentence
orgOfDocIntro = foldlSent [S "The", phrase organization, S "of this",
  phrase document, S "follows the", phrase template, S "for an", short srs,
  S "for", phrase sciCompS, S "proposed by" +:+ makeCiteS koothoor2013
  `sAnd` makeCiteS smithLai2005 `sC` S "with some", 
  plural aspect, S "taken from Volere", phrase template,
  S "16", makeCiteS rbrtsn2012]

orgOfDocIntroEnd = foldl (+:+) EmptyS [(at_startNP' $ the dataDefn),
  S "are used to support", (plural definition `ofThe` S "different"),
  plural model]

{--STAKEHOLDERS--}

{--The Client--}
{--The Customer--}

{--GENERAL SYSTEM DESCRIPTION--}

{--System Context--}
  
sysCtxIntro :: Contents
sysCtxIntro = foldlSP
  [makeRef2S sysCtxFig1 +:+ S "shows the" +:+. phrase sysCont,
   S "A circle represents an external entity outside the" +:+ phrase software
   `sC` S "the", phrase user, S "in this case. A rectangle represents the",
   phrase softwareSys, S "itself", (sParen $ short gLassBR) +:+. EmptyS,
   S "Arrows are used to show the data flow between the" +:+ phrase system,
   S "and its" +:+ phrase environment]
   
sysCtxFig1 :: LabelledContent
sysCtxFig1 = llcc (makeFigRef "sysCtxDiag") $ 
  fig (titleize sysCont) (resourcePath ++ "SystemContextFigure.png") 

sysCtxDesc :: Contents
sysCtxDesc = foldlSPCol
  [S "The interaction between the", phrase product_, S "and the", phrase user,
   S "is through a user" +:+. phrase interface,
   S "The responsibilities of the", phrase user, S "and the", phrase system,
   S "are as follows"]
   
sysCtxUsrResp :: [Sentence]
sysCtxUsrResp = [S "Provide the input data related to the glass slab and blast",
    S "type ensuring no errors in the data entry",
  S "Ensure that consistent units are used for input variables",
  S "Ensure required" +:+ phrase software +:+ plural assumption +:+
    (sParen $ makeRef2S $ SRS.assumpt ([]::[Contents]) ([]::[Section]))
    +:+ S "are appropriate for any particular" +:+
    phrase problem +:+ S "input to the" +:+ phrase software]

sysCtxSysResp :: [Sentence]
sysCtxSysResp = [S "Detect data type mismatch, such as a string of characters",
    S "input instead of a floating point number",
  S "Determine if the inputs satisfy the required physical and software constraints",
  S "Predict whether the glass slab is safe or not."]
  
sysCtxResp :: [Sentence]
sysCtxResp = [titleize user +:+ S "Responsibilities",
  short gLassBR +:+ S "Responsibilities"]

sysCtxList :: Contents
sysCtxList = UlC $ ulcc $ Enumeration $ bulletNested sysCtxResp $
  map bulletFlat [sysCtxUsrResp, sysCtxSysResp]
   
{--User Characteristics--}

userCharacteristicsIntro :: Contents
userCharacteristicsIntro = LlC $ enumBullet characteristicsLabel $ map foldlSent
  [[S "The end user of GlassBR is expected to have completed at least the",
    S "equivalent of the second year of an undergraduate degree in civil engineering or structural engineering"],
  [S "The end user is expected to have an understanding of theory behind glass",
    S "breakage and blast risk"],
  [S "The end user is expected to have basic computer literacy to handle the software"]]

{--System Constraints--}

{--SPECIFIC SYSTEM DESCRIPTION--}

--Automatically generated

{--PROBLEM DESCRIPTION--}

probStart, probEnding :: Sentence
probStart = foldlSent [S "A", phrase system,
  S "is needed to efficiently" `sAnd` S "correctly predict the",
  phrase blastRisk +:+ S "involved with the glass"]
probEnding = foldl (+:+) EmptyS [S "interpret the", plural input_,
  S "to give out the", plural output_,
  S "which predict whether the", phrase glaSlab,
  S "can withstand the", phrase blast, S "under the",
  plural condition]

{--Terminology and Definitions--}

termsAndDesc = termDefnF (Just (S "All" `sOf` S "the" +:+ plural term_ +:+
  S "are extracted from" +:+ makeCiteS astm2009 `sIn`
  (makeRef2S $ SRS.reference ([]::[Contents]) ([]::[Section])))) [termsAndDescBullets]

{--Physical System Description--}

physSystDescription = physSystDesc (short gLassBR) figGlassbr 
  [physSystDescriptionList, LlC figGlassbr]

figGlassbr = llcc (makeFigRef "physSystImage") $ figWithWidth 
  (at_startNP $ the physicalSystem) (resourcePath ++ "physicalsystimage.png") 30

physSystDescriptionList = LlC $ enumSimple physSystDescriptionLabel 1 (short physSyst) physSystDescriptionListPhysys

--"Dead" knowledge?
physSystDescriptionListPhysys :: [Sentence]
physSystDescriptionListPhysys1 :: Sentence
physSystDescriptionListPhysys2 :: NamedIdea n => n -> Sentence

physSystDescriptionListPhysys = [physSystDescriptionListPhysys1, physSystDescriptionListPhysys2 (ptOfExplsn)]

physSystDescriptionListPhysys1 = S "The" +:+. phrase glaSlab

physSystDescriptionListPhysys2 imprtntElem = foldlSent [S "The"
  +:+. phrase imprtntElem, S "Where the", phrase bomb `sC`
  S "or", (blast ^. defn) `sC` S "is located. The", phrase sD
  `isThe` phrase distance, S "between the", phrase imprtntElem `sAnd`
  S "the glass"]

{--Goal Statements--}

goalStmts = goalStmtF [foldlList Comma List [plural dimension `ofThe` phrase glaPlane,
  phrase glassTy, plural characteristic `ofThe` phrase explosion,
  S "the" +:+ phrase pbTol]] goalStmtsList

{--SOLUTION CHARACTERISTICS SPECIFICATION--}

--Automatically generated

{--Assumptions--}

{--Theoretical Models--}

{--Data Definitions--}

{--Data Constraints--}

{-input and output tables-}

inputDataConstraints = inDataConstTbl gbInputDataConstraints
outputDataConstraints = outDataConstTbl [probBr]

{--REQUIREMENTS--}

{--Functional Requirements--}

{--Nonfunctional Requirements--}

{--LIKELY CHANGES--}

{--UNLIKELY CHANGES--}

{--TRACEABLITY MATRICES AND GRAPHS--}
traceTable1 :: LabelledContent
traceTable1 = generateTraceTable glassSystInfo

traceMatsAndGraphsTable1Desc :: Sentence
traceMatsAndGraphsTable1Desc = foldlList Comma List (map plural (take 3 solChSpecSubsections)) +:+.
  S "with each other"

traceMatsAndGraphsTable2Desc :: Sentence
traceMatsAndGraphsTable2Desc = plural requirement +:+ S "on" +:+. foldlList Comma List
  (map plural solChSpecSubsections)

traceMatsAndGraphsTable3Desc :: Sentence
traceMatsAndGraphsTable3Desc = foldlsC (map plural (take 3 solChSpecSubsections)) `sC`
  plural likelyChg `sAnd` plural requirement +:+ S "on the" +:+
  plural assumption

traceMatsAndGraphsT, traceMatsAndGraphsIM, traceMatsAndGraphsDD, traceMatsAndGraphsDataCons, traceMatsAndGraphsFuncReq, traceMatsAndGraphsA,
  traceMatsAndGraphsLC :: [String]

traceMatsAndGraphsTRef, traceMatsAndGraphsIMRef, traceMatsAndGraphsDDRef, traceMatsAndGraphsDataConsRef, traceMatsAndGraphsFuncReqRef,
  traceMatsAndGraphsARef, traceMatsAndGraphsLCRef :: [Sentence]

traceMatsAndGraphsT = ["T1", "T2"]
traceMatsAndGraphsTRef = map makeRef2S gbrTMods

traceMatsAndGraphsIM = ["IM1", "IM2", "IM3"]
traceMatsAndGraphsIMRef = map makeRef2S gbrIMods

traceMatsAndGraphsDD =  ["DD1", "DD2", "DD3", "DD4", "DD5", "DD6", "DD7", "DD8"]
traceMatsAndGraphsDDRef = map makeRef2S dataDefns

traceMatsAndGraphsDataCons  = ["Data Constraints"]
traceMatsAndGraphsDataConsRef = [makeRef2S $ SRS.datCon ([]::[Contents]) ([]::[Section])]

traceMatsAndGraphsFuncReq = ["R1", "R2", "R3", "R4", "R5", "R6"]
traceMatsAndGraphsFuncReqRef = map makeRef2S funcReqs

traceMatsAndGraphsA = ["A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8"]
traceMatsAndGraphsARef = map makeRef2S assumptions

traceMatsAndGraphsLC = ["LC1", "LC2", "LC3", "LC4", "LC5"]
traceMatsAndGraphsLCRef = map makeRef2S likelyChgs

traceMatsAndGraphsRowT1 :: [String]
traceMatsAndGraphsRowT1 = traceMatsAndGraphsT ++ traceMatsAndGraphsIM ++ traceMatsAndGraphsDD

-- The headers for the first row, and column
traceMatsAndGraphsRowHdrT1 :: [Sentence]
traceMatsAndGraphsRowHdrT1 = zipWith itemRefToSent traceMatsAndGraphsRowT1 (traceMatsAndGraphsTRef ++
  traceMatsAndGraphsIMRef ++ traceMatsAndGraphsDDRef)

-- list of columns and their rows for traceability matrix
traceMatsAndGraphsColsT1 :: [[String]]
traceMatsAndGraphsColsT1 = [traceMatsAndGraphsColsT1_T1, traceMatsAndGraphsColsT1_T2, traceMatsAndGraphsColsT1_IM1, traceMatsAndGraphsColsT1_IM2, traceMatsAndGraphsColsT1_IM3,
  traceMatsAndGraphsColsT1_DD1, traceMatsAndGraphsColsT1_DD2, traceMatsAndGraphsColsT1_DD3, traceMatsAndGraphsColsT1_DD4, traceMatsAndGraphsColsT1_DD5, traceMatsAndGraphsColsT1_DD6, traceMatsAndGraphsColsT1_DD7,
  traceMatsAndGraphsColsT1_DD8]

traceMatsAndGraphsColsT1_T1, traceMatsAndGraphsColsT1_T2, traceMatsAndGraphsColsT1_IM1, traceMatsAndGraphsColsT1_IM2, traceMatsAndGraphsColsT1_IM3, traceMatsAndGraphsColsT1_DD1, traceMatsAndGraphsColsT1_DD2,
  traceMatsAndGraphsColsT1_DD3, traceMatsAndGraphsColsT1_DD4, traceMatsAndGraphsColsT1_DD5, traceMatsAndGraphsColsT1_DD6, traceMatsAndGraphsColsT1_DD7, traceMatsAndGraphsColsT1_DD8 :: [String]

-- list of each item that "this" item requires for traceability matrix
traceMatsAndGraphsColsT1_T1  = ["T2", "IM1"]
traceMatsAndGraphsColsT1_T2  = ["T1", "IM2", "IM3"]
traceMatsAndGraphsColsT1_IM1 = ["DD1", "DD2", "DD3"]
traceMatsAndGraphsColsT1_IM2 = ["DD4", "DD5"]
traceMatsAndGraphsColsT1_IM3 = []
traceMatsAndGraphsColsT1_DD1 = []
traceMatsAndGraphsColsT1_DD2 = []
traceMatsAndGraphsColsT1_DD3 = ["DD6"]
traceMatsAndGraphsColsT1_DD4 = ["DD2", "DD6"]
traceMatsAndGraphsColsT1_DD5 = []
traceMatsAndGraphsColsT1_DD6 = ["IM3", "DD2", "DD5"]
traceMatsAndGraphsColsT1_DD7 = ["DD8"]
traceMatsAndGraphsColsT1_DD8 = ["DD2"]

traceMatsAndGraphsTable1 = llcc (makeTabRef "TraceyItemSecs") $ Table
  (EmptyS:traceMatsAndGraphsRowHdrT1)
  (makeTMatrix traceMatsAndGraphsRowHdrT1 traceMatsAndGraphsColsT1 traceMatsAndGraphsRowT1)
  (showingCxnBw traceyMatrix
  (titleize' item +:+ S "of Different" +:+ titleize' section_)) True

--

traceMatsAndGraphsRowT2 :: [String]
traceMatsAndGraphsRowT2 = traceMatsAndGraphsRowT1 ++ traceMatsAndGraphsDataCons ++ traceMatsAndGraphsFuncReq

traceMatsAndGraphsRowHdrT2, traceMatsAndGraphsColHdrT2 :: [Sentence]
traceMatsAndGraphsRowHdrT2 = traceMatsAndGraphsRowHdrT1 ++
  (zipWith itemRefToSent (traceMatsAndGraphsDataCons ++ traceMatsAndGraphsFuncReq)
   (traceMatsAndGraphsDataConsRef ++ traceMatsAndGraphsFuncReqRef))

traceMatsAndGraphsColHdrT2 = zipWith (\x y -> (S x) +:+ (sParen (S "in" +:+ y)))
  traceMatsAndGraphsFuncReq traceMatsAndGraphsFuncReqRef

traceMatsAndGraphsColsT2_R1, traceMatsAndGraphsColsT2_R2, traceMatsAndGraphsColsT2_R3,
  traceMatsAndGraphsColsT2_R4, traceMatsAndGraphsColsT2_R5, traceMatsAndGraphsColsT2_R6 :: [String]

traceMatsAndGraphsColsT2 :: [[String]]
traceMatsAndGraphsColsT2 = [traceMatsAndGraphsColsT2_R1, traceMatsAndGraphsColsT2_R2, 
  traceMatsAndGraphsColsT2_R3, traceMatsAndGraphsColsT2_R4, traceMatsAndGraphsColsT2_R5,
  traceMatsAndGraphsColsT2_R6]
traceMatsAndGraphsColsT2_R1 = []
traceMatsAndGraphsColsT2_R2 = []
traceMatsAndGraphsColsT2_R3 = ["Data Constraints"]
traceMatsAndGraphsColsT2_R4 = ["R1", "R2"]
traceMatsAndGraphsColsT2_R5 = ["T1", "T2"]
traceMatsAndGraphsColsT2_R6 = ["IM1", "IM2", "IM3", "DD2", "DD3", "DD4", "DD5", "DD6", "DD7", "DD8"]

traceMatsAndGraphsTable2 = llcc (makeTabRef "TraceyReqsItems") $ Table
  (EmptyS:traceMatsAndGraphsRowHdrT2)
  (makeTMatrix traceMatsAndGraphsColHdrT2 traceMatsAndGraphsColsT2 traceMatsAndGraphsRowT2)
  (showingCxnBw traceyMatrix (titleize' requirement `sAnd` S "Other" +:+
  titleize' item)) True

--

traceMatsAndGraphsRowT3 :: [String]
traceMatsAndGraphsRowT3 = traceMatsAndGraphsA

traceMatsAndGraphsRowHdr3, traceMatsAndGraphsColHdr3 :: [Sentence]
traceMatsAndGraphsRowHdr3 = zipWith itemRefToSent traceMatsAndGraphsA traceMatsAndGraphsARef

traceMatsAndGraphsColHdr3 = traceMatsAndGraphsRowHdrT1 ++ (zipWith itemRefToSent
  (traceMatsAndGraphsLC ++ traceMatsAndGraphsFuncReq) (traceMatsAndGraphsLCRef ++ traceMatsAndGraphsFuncReqRef))

traceMatsAndGraphsColsT3 :: [[String]]
traceMatsAndGraphsColsT3 = [traceMatsAndGraphsColsT3_T1, traceMatsAndGraphsColsT3_T2, traceMatsAndGraphsColsT3_IM1, traceMatsAndGraphsColsT3_IM2, traceMatsAndGraphsColsT3_IM3, traceMatsAndGraphsColsT3_DD1,
  traceMatsAndGraphsColsT3_DD2, traceMatsAndGraphsColsT3_DD3, traceMatsAndGraphsColsT3_DD4, traceMatsAndGraphsColsT3_DD5, traceMatsAndGraphsColsT3_DD6, traceMatsAndGraphsColsT3_DD7, traceMatsAndGraphsColsT3_DD8,
  traceMatsAndGraphsColsT3_LC1, traceMatsAndGraphsColsT3_LC2, traceMatsAndGraphsColsT3_LC3, traceMatsAndGraphsColsT3_LC4, traceMatsAndGraphsColsT3_LC5, traceMatsAndGraphsColsT3_R1, traceMatsAndGraphsColsT3_R2,
  traceMatsAndGraphsColsT3_R3, traceMatsAndGraphsColsT3_R4, traceMatsAndGraphsColsT3_R5, traceMatsAndGraphsColsT3_R6]

traceMatsAndGraphsColsT3_T1, traceMatsAndGraphsColsT3_T2, traceMatsAndGraphsColsT3_IM1, traceMatsAndGraphsColsT3_IM2, traceMatsAndGraphsColsT3_IM3, traceMatsAndGraphsColsT3_DD1, traceMatsAndGraphsColsT3_DD2,
  traceMatsAndGraphsColsT3_DD3, traceMatsAndGraphsColsT3_DD4, traceMatsAndGraphsColsT3_DD5, traceMatsAndGraphsColsT3_DD6, traceMatsAndGraphsColsT3_DD7, traceMatsAndGraphsColsT3_DD8,
  traceMatsAndGraphsColsT3_LC1, traceMatsAndGraphsColsT3_LC2, traceMatsAndGraphsColsT3_LC3, traceMatsAndGraphsColsT3_LC4, traceMatsAndGraphsColsT3_LC5, traceMatsAndGraphsColsT3_R1,
  traceMatsAndGraphsColsT3_R2, traceMatsAndGraphsColsT3_R3, traceMatsAndGraphsColsT3_R4, traceMatsAndGraphsColsT3_R5, traceMatsAndGraphsColsT3_R6 :: [String]

-- list of each item that "this" item requires for traceability matrix
traceMatsAndGraphsColsT3_T1  = []
traceMatsAndGraphsColsT3_T2  = []
traceMatsAndGraphsColsT3_IM1 = ["A4", "A6", "A7"]
traceMatsAndGraphsColsT3_IM2 = ["A1", "A2", "A5"]
traceMatsAndGraphsColsT3_IM3 = []
traceMatsAndGraphsColsT3_DD1 = []
traceMatsAndGraphsColsT3_DD2 = []
traceMatsAndGraphsColsT3_DD3 = []
traceMatsAndGraphsColsT3_DD4 = ["A4"]
traceMatsAndGraphsColsT3_DD5 = []
traceMatsAndGraphsColsT3_DD6 = ["A5"]
traceMatsAndGraphsColsT3_DD7 = []
traceMatsAndGraphsColsT3_DD8 = ["A4"]
traceMatsAndGraphsColsT3_LC1 = ["A3"]
traceMatsAndGraphsColsT3_LC2 = ["A4", "A8"]
traceMatsAndGraphsColsT3_LC3 = ["A5"]
traceMatsAndGraphsColsT3_LC4 = ["A6"]
traceMatsAndGraphsColsT3_LC5 = ["A7"]
traceMatsAndGraphsColsT3_R1  = []
traceMatsAndGraphsColsT3_R2  = ["A4", "A5", "A8"]
traceMatsAndGraphsColsT3_R3  = []
traceMatsAndGraphsColsT3_R4  = []
traceMatsAndGraphsColsT3_R5  = []
traceMatsAndGraphsColsT3_R6  = []

traceMatsAndGraphsTable3 = llcc (makeTabRef "TraceyAssumpsOthers") $ Table
  (EmptyS:traceMatsAndGraphsRowHdr3)
  (makeTMatrix traceMatsAndGraphsColHdr3 traceMatsAndGraphsColsT3 traceMatsAndGraphsRowT3)
  (showingCxnBw traceyMatrix (titleize' assumption `sAnd` S "Other"
  +:+ titleize' item)) True

--
traceMatsAndGraphsIntro2 :: [Contents]
traceMatsAndGraphsIntro2 = map UlC $ traceGIntro traceyGraphs
  [(foldlList Comma List (map plural (take 3 solChSpecSubsections)) +:+.
  S "on each other"), (plural requirement +:+ S "on" +:+. foldlList Comma List
  (map plural solChSpecSubsections)),
  (foldlList Comma List ((map plural (take 3 solChSpecSubsections))++
  [plural requirement, plural likelyChg +:+ S "on" +:+ plural assumption]))]

fig_2 = figureLabel 2 traceyMatrix
  (titleize' item +:+ S "of Different" +:+ titleize' section_)
  (resourcePath ++ "Trace.png") "TraceyItemSecs"

fig_3 = figureLabel 3 traceyMatrix
  (titleize' requirement `sAnd` S "Other" +:+ titleize' item)
  (resourcePath ++ "RTrace.png") "TraceyReqsItems"

fig_4 = figureLabel 4 traceyMatrix
  (titleize' assumption `sAnd` S "Other" +:+ titleize' item)
  (resourcePath ++ "ATrace.png") "TraceyAssumpsOthers"

{--VALUES OF AUXILIARY CONSTANTS--}

{--REFERENCES--}

{--APPENDIX--}

appdxIntro = foldlSP [
  S "This", phrase appendix, S "holds the", plural graph,
  sParen ((makeRef2S fig_5) `sAnd` (makeRef2S fig_6)),
  S "used for interpolating", plural value, S "needed in the", plural model]

fig_5 = llcc (makeFigRef "demandVSsod") $ fig (titleize figure +: S "5" +:+ (demandq ^. defn) +:+
  sParen (ch demand) `sVersus` at_start sD +:+ sParen (getAcc stdOffDist)
  `sVersus` at_start charWeight +:+ sParen (ch charWeight))
  (resourcePath ++ "ASTM_F2248-09.png")

fig_6 = llcc (makeFigRef "dimlessloadVSaspect") $ fig (titleize figure +: S "6" +:+ S "Non dimensional" +:+
  phrase lateralLoad +:+ sParen (ch dimlessLoad)
  `sVersus` titleize aspect_ratio +:+ sParen (getAcc aR)
  `sVersus` at_start stressDistFac +:+ sParen (ch stressDistFac))
  (resourcePath ++ "ASTM_F2248-09_BeasonEtAl.png")

blstRskInvWGlassSlab :: Sentence
blstRskInvWGlassSlab = phrase blastRisk +:+ S "involved with the" +:+
  phrase glaSlab
