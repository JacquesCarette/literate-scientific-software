module Drasil.PDController.Body (pidODEInfo, printSetting, si, srs, fullSI) where

import Data.List (nub)
import Data.Drasil.Concepts.Documentation (doccon, doccon', srsDomains)
import qualified Data.Drasil.Concepts.Documentation as Doc (srs)
import Data.Drasil.Concepts.Math (mathcon, mathcon', ode)
import Data.Drasil.Concepts.Software (program)
import Data.Drasil.ExternalLibraries.ODELibraries
       (apacheODESymbols, arrayVecDepVar, odeintSymbols, osloSymbols,
        scipyODESymbols)
import qualified Data.Drasil.TheoryConcepts as IDict (dataDefn)
import Data.Drasil.Quantities.Physics (physicscon)
import Data.Drasil.Concepts.PhysicalProperties (physicalcon)
import Data.Drasil.Quantities.PhysicalProperties (mass)
import Data.Drasil.SI_Units (second, kilogram)
import Database.Drasil
       (Block, ChunkDB, ReferenceDB, SystemInformation(SI), _authors, _concepts,
        _configFiles, _constants, _constraints, _datadefs, _defSequence,
        _inputs, _kind, _outputs, _purpose, _quants, _sys, _instModels,
        _sysinfodb, _usedinfodb, cdb, rdb, refdb)
import Drasil.DocLang
       (DerivationDisplay(..),
        DocSection(Bibliography, GSDSec, IntroSec, LCsSec, RefSec, ReqrmntSec,
                   SSDSec, TraceabilitySec),
        Field(..), Fields, GSDSec(..), GSDSub(..), InclUnits(IncludeUnits),
        IntroSec(..), IntroSub(..), PDSub(..), ProblemDescription(PDProg),
        RefSec(..), RefTab(..), ReqrmntSec(..), ReqsSub(..), SCSSub(..),
        SRSDecl, SSDSec(..), SSDSub(SSDProblem, SSDSolChSpec),
        SolChSpec(SCSProg), TSIntro(..), TraceabilitySec(TraceabilityProg),
        Verbosity(Verbose), intro, mkDoc, traceMatStandard, tsymb, getTraceConfigUID,
        secRefs, fillTraceSI, traceyGraphGetRefs)
import qualified Drasil.DocLang.SRS as SRS (inModel)

import Language.Drasil
import Language.Drasil.Printers (PrintingInformation(..), defaultConfiguration)
import Theory.Drasil (DataDefinition, GenDefn, InstanceModel, TheoryModel)
import qualified Utils.Drasil.Sentence as S

import Drasil.PDController.Assumptions (assumptions, assumpRefs)
import Drasil.PDController.Changes (likelyChgs, chgRefs)
import Drasil.PDController.Concepts (acronyms, pidControllerSystem,
  pidC, concepts, defs)
import Drasil.PDController.DataDefs (dataDefinitions, dataDefRefs)
import Drasil.PDController.GenDefs (genDefns, genDefRefs)
import Drasil.PDController.GenSysDesc
       (gsdSysContextFig, gsdSysContextList, gsdSysContextP1, gsdSysContextP2,
        gsduserCharacteristics, figRefs)
import Drasil.PDController.IModel (instanceModels, imPD, iModRefs)
import Drasil.PDController.IntroSection
       (introDocOrg, introPara, introPurposeOfDoc, introUserChar1,
        introUserChar2, introscopeOfReq)
import Drasil.PDController.References (citations, citeRefs)
import Drasil.PDController.Requirements (funcReqs, nonfuncReqs, reqRefs)
import Drasil.PDController.SpSysDesc
       (goals, sysFigure, sysGoalInput, sysParts, sysProblemDesc, sysDescRefs)
import Drasil.PDController.TModel (theoreticalModels, tModRefs)
import Drasil.PDController.Unitals (symbols, inputs, outputs, inputsUC,
  inpConstrained, pidConstants, pidDqdConstants )
import Drasil.PDController.ODEs (pidODEInfo)

naveen :: Person
naveen = person "Naveen Ganesh" "Muralidharan"

srs :: Document
srs = mkDoc mkSRS (S.forGen titleize phrase) si

fullSI :: SystemInformation
fullSI = fillTraceSI mkSRS si

printSetting :: PrintingInformation
printSetting = PI symbMap Equational defaultConfiguration

mkSRS :: SRSDecl
mkSRS
  = [RefSec $ RefProg intro [TUnits, tsymb [TSPurpose, SymbOrder], TAandA],
     IntroSec $
       IntroProg introPara (phrase pidControllerSystem)
         [IPurpose [introPurposeOfDoc], IScope introscopeOfReq,
          IChar introUserChar1 introUserChar2 [],
          IOrgSec introDocOrg IDict.dataDefn (SRS.inModel [] [])
            (S "The instance model referred as" +:+ refS imPD +:+
               S "provides an"
               +:+ titleize ode +:+ sParen (short ode)
               +:+ S "that models the"
               +:+ phrase pidC)],
     GSDSec $
       GSDProg
         [SysCntxt
            [gsdSysContextP1, LlC gsdSysContextFig, gsdSysContextP2,
             gsdSysContextList],
          UsrChars [gsduserCharacteristics], SystCons [] []],
     SSDSec $
       SSDProg
         [SSDProblem $
            PDProg sysProblemDesc []
              [TermsAndDefs Nothing defs,
               PhySysDesc pidControllerSystem sysParts sysFigure [],
               Goals sysGoalInput],
          SSDSolChSpec $
            SCSProg
              [Assumptions, TMs [] (Label : stdFields),
               GDs [] (Label : stdFields) HideDerivation,
               DDs [] ([Label, Symbol, Units] ++ stdFields) ShowDerivation,
               IMs []
                 ([Label, Input, Output, InConstraints, OutConstraints] ++
                    stdFields)
                 ShowDerivation,
               Constraints EmptyS inputsUC]],

     ReqrmntSec $ ReqsProg [FReqsSub EmptyS [], NonFReqsSub], LCsSec,
     TraceabilitySec $ TraceabilityProg $ traceMatStandard si, Bibliography]

si :: SystemInformation
si
  = SI{_sys = pidControllerSystem, _kind = Doc.srs, _authors = [naveen],
       _purpose = [], _quants = symbolsAll,
       _concepts = [] :: [DefinedQuantityDict],
       _datadefs = dataDefinitions, _instModels = [],
       _configFiles = [], _inputs = inputs, _outputs = outputs,
       _defSequence = [] :: [Block QDefinition],
       _constraints = map cnstrw inpConstrained, _constants = pidConstants,
       _sysinfodb = symbMap, _usedinfodb = usedDB, refdb = refDB}

symbolsAll :: [QuantityDict]
symbolsAll
  = symbols ++
      scipyODESymbols ++
        osloSymbols ++
          apacheODESymbols ++
            odeintSymbols ++
              map qw [arrayVecDepVar pidODEInfo] ++
                map qw pidDqdConstants ++ map qw pidConstants

symbMap :: ChunkDB
symbMap
  = cdb (map qw physicscon ++ symbolsAll ++ map qw [mass])
      (nw pidControllerSystem :
         [nw program] ++
           map nw doccon ++
             map nw doccon' ++
               concepts ++
                 map nw mathcon ++
                   map nw mathcon' ++
                     map nw [second, kilogram] ++
                       map nw symbols ++ map nw physicscon ++ map nw acronyms
                       ++ map nw physicalcon)
      (map cw inpConstrained ++ srsDomains)
      (map unitWrapper [second, kilogram])
      dataDefinitions
      instanceModels
      genDefns
      theoreticalModels
      conceptInstances
      ([] :: [Section])
      ([] :: [LabelledContent])
      allRefs

usedDB :: ChunkDB
usedDB
  = cdb ([] :: [QuantityDict]) (map nw acronyms ++ map nw symbolsAll)
      ([] :: [ConceptChunk])
      ([] :: [UnitDefn])
      ([] :: [DataDefinition])
      ([] :: [InstanceModel])
      ([] :: [GenDefn])
      ([] :: [TheoryModel])
      ([] :: [ConceptInstance])
      ([] :: [Section])
      ([] :: [LabelledContent])
      ([] :: [Reference])

refDB :: ReferenceDB
refDB = rdb citations conceptInstances

conceptInstances :: [ConceptInstance]
conceptInstances = assumptions ++ goals ++ funcReqs ++ nonfuncReqs ++ likelyChgs

stdFields :: Fields
stdFields
  = [DefiningEquation, Description Verbose IncludeUnits, Notes, Source, RefBy]

-- References --
bodyRefs :: [Reference]
bodyRefs = map ref conceptInstances ++ map (ref.makeTabRef.getTraceConfigUID) (traceMatStandard si) ++ traceyGraphGetRefs "PDController"

allRefs :: [Reference]
allRefs = nub (assumpRefs ++ bodyRefs ++ chgRefs ++ figRefs ++ sysDescRefs ++ dataDefRefs ++ genDefRefs
  ++ iModRefs ++ tModRefs ++ citeRefs ++ reqRefs ++ secRefs)

