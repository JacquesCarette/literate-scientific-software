module Drasil.HGHC.Body (srs, si, symbMap, printSetting, fullSI) where

import Data.List (nub)
import Language.Drasil hiding (Manual) -- Citation name conflict. FIXME: Move to different namespace
import Drasil.DocLang (DocSection(RefSec, SSDSec), Literature(Lit, Manual), 
    RefSec(..), RefTab(TUnits), TSIntro(SymbConvention, TSPurpose), SRSDecl, 
    intro, mkDoc, tsymb, InclUnits(IncludeUnits), Verbosity(Verbose),
    Field(DefiningEquation, Description, Label, Symbol, Units), SolChSpec(SCSProg), 
    SCSSub(DDs), DerivationDisplay(HideDerivation), SSDSub(SSDSolChSpec), 
    SSDSec(SSDProg), traceMatStandard, getTraceConfigUID, secRefs, fillTraceSI, traceyGraphGetRefs)
import Language.Drasil.Printers (PrintingInformation(..), defaultConfiguration)
import Database.Drasil (Block, ChunkDB, SystemInformation(SI), cdb,
  rdb, refdb, _authors, _concepts, _constants, _constraints, _purpose,
  _datadefs, _instModels, _configFiles, _defSequence, _inputs, _kind, _outputs, _quants, 
  _sys, _sysinfodb, _usedinfodb)
import qualified Utils.Drasil.Sentence as S

import Drasil.HGHC.HeatTransfer (fp, hghc, dataDefs, htInputs, htOutputs, 
    nuclearPhys, symbols, htTransCladCoolDD, htTransCladFuelDD)

import Data.Drasil.SI_Units (siUnits, fundamentals, derived, degree)
import Data.Drasil.People (spencerSmith)
import Data.Drasil.Concepts.Documentation (doccon, doccon')
import Data.Drasil.Concepts.Math (mathcon)
import qualified Data.Drasil.Concepts.Documentation as Doc (srs)
  
srs :: Document
srs = mkDoc mkSRS S.forT si

fullSI :: SystemInformation
fullSI = fillTraceSI mkSRS si

printSetting :: PrintingInformation
printSetting = PI symbMap Equational defaultConfiguration

si :: SystemInformation
si = SI {
  _sys         = hghc,
  _kind        = Doc.srs,
  _authors     = [spencerSmith],
  _quants      = symbols,
  _purpose     = [],
  _concepts    = [] :: [UnitaryConceptDict],
  _instModels  = [], -- FIXME; empty _instModels
  _datadefs    = dataDefs,
  _configFiles = [],
  _inputs      = htInputs,
  _outputs     = htOutputs,
  _defSequence = [] :: [Block QDefinition],
  _constraints = [] :: [ConstrainedChunk],
  _constants   = [],
  _sysinfodb   = symbMap,
  _usedinfodb  = usedDB,
   refdb       = rdb [] [] -- FIXME?
}
  
mkSRS :: SRSDecl
mkSRS = [RefSec $
    RefProg intro [TUnits, tsymb [TSPurpose, SymbConvention [Lit $ nw nuclearPhys, Manual $ nw fp]]],
    SSDSec $ SSDProg [
      SSDSolChSpec $ SCSProg [
        DDs [] [Label, Symbol, Units, DefiningEquation,
          Description Verbose IncludeUnits] HideDerivation
      ]]]

symbMap :: ChunkDB
symbMap = cdb symbols (map nw symbols ++ map nw doccon ++ map nw fundamentals ++ map nw derived
  ++ [nw fp, nw nuclearPhys, nw hghc, nw degree] ++ map nw doccon' ++ map nw mathcon)
  ([] :: [ConceptChunk])-- FIXME: Fill in concepts
  siUnits dataDefs [] [] [] [] [] [] allRefs

usedDB :: ChunkDB
usedDB = cdb ([] :: [QuantityDict]) (map nw symbols)
           ([] :: [ConceptChunk]) ([] :: [UnitDefn]) [] [] [] [] [] [] [] ([] :: [Reference])

-- References --
bodyRefs :: [Reference]
bodyRefs = map (ref.makeTabRef.getTraceConfigUID) (traceMatStandard si) ++ traceyGraphGetRefs "HGHC"

dataDefRefs :: [Reference]
dataDefRefs = map ref [htTransCladCoolDD, htTransCladFuelDD]

allRefs :: [Reference]
allRefs = nub (bodyRefs ++ dataDefRefs ++ secRefs)