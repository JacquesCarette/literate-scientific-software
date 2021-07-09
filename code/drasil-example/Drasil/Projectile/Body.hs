module Drasil.Projectile.Body (printSetting, si, srs, projectileTitle, fullSI) where

import Data.List (nub)
import Language.Drasil
import Language.Drasil.Printers (PrintingInformation(..), defaultConfiguration)
import Database.Drasil (Block, ChunkDB, ReferenceDB, SystemInformation(SI),
  cdb, rdb, refdb, _authors, _purpose, _concepts, _constants, _constraints, 
  _datadefs, _instModels, _configFiles, _defSequence, _inputs, _kind, 
  _outputs, _quants, _sys, _sysinfodb, _usedinfodb)
import Utils.Drasil
import Utils.Drasil.Concepts
import qualified Utils.Drasil.NounPhrase as NP
import qualified Utils.Drasil.Sentence as S

import Drasil.DocLang (AuxConstntSec(AuxConsProg),
  DerivationDisplay(ShowDerivation),
  DocSection(AuxConstntSec, Bibliography, IntroSec, RefSec, ReqrmntSec, SSDSec, TraceabilitySec),
  Emphasis(Bold), Field(..), Fields, InclUnits(IncludeUnits),
  IntroSec(IntroProg), IntroSub(IScope), ProblemDescription(PDProg), PDSub(..),
  RefSec(..), RefTab(..), ReqrmntSec(..), ReqsSub(..), SCSSub(..), SRSDecl,
  SSDSec(..), SSDSub(SSDProblem, SSDSolChSpec), SolChSpec(SCSProg),
  TConvention(..), TSIntro(..), TraceabilitySec(TraceabilityProg),
  Verbosity(Verbose), intro, mkDoc, traceMatStandard, tsymb, getTraceConfigUID,
  secRefs, fillTraceSI, traceyGraphGetRefs)

import Data.Drasil.Concepts.Computation (inValue)
import Data.Drasil.Concepts.Documentation (analysis, doccon, doccon', physics,
  problem, srsDomains, assumption, goalStmt, physSyst,
  requirement, typUnc)
import qualified Data.Drasil.Concepts.Documentation as Doc (srs)
import Data.Drasil.Concepts.Math (cartesian, mathcon)
import Data.Drasil.Concepts.PhysicalProperties (mass)
import Data.Drasil.Concepts.Physics (gravity, physicCon, physicCon',
  rectilinear, oneD, twoD, motion)
import Data.Drasil.Concepts.Software (errMsg, program)

import Data.Drasil.Quantities.Math (pi_, piConst)
import Data.Drasil.Quantities.Physics (acceleration, constAccel,
  gravitationalAccelConst, iPos, iSpeed, iVel, ixPos, iyPos, ixVel, iyVel,
  position, scalarPos, time, velocity, xAccel, xConstAccel, xPos,
  xVel, yAccel, yConstAccel, yPos, yVel, physicscon)

import Data.Drasil.People (brooks, samCrawford, spencerSmith)
import Data.Drasil.SI_Units (metre, radian, second)
import Data.Drasil.Theories.Physics (accelerationTM, velocityTM)
import Data.Drasil.TheoryConcepts (dataDefn, genDefn, inModel, thModel)

import Drasil.Projectile.Assumptions (assumptions, assumpRefs)
import Drasil.Projectile.Concepts (concepts, landingPosNC,
  launcher, projectile, target)
import Drasil.Projectile.DataDefs (dataDefs, dataDefRefs)
import Drasil.Projectile.Figures (figLaunch, figRefs)
import Drasil.Projectile.GenDefs (genDefns, genDefRefs)
import Drasil.Projectile.Goals (goals, goalRefs)
import Drasil.Projectile.IMods (iMods, iModRefs)
import Drasil.Projectile.References (citations, citeRefs)
import Drasil.Projectile.Requirements (funcReqs, nonfuncReqs, reqRefs)
import Drasil.Projectile.Unitals

import Theory.Drasil (TheoryModel)

srs :: Document
srs = mkDoc mkSRS (S.forGen titleize phrase) si

fullSI :: SystemInformation
fullSI = fillTraceSI mkSRS si

printSetting :: PrintingInformation
printSetting = PI symbMap Equational defaultConfiguration

mkSRS :: SRSDecl
mkSRS = [
  RefSec $
    RefProg intro
      [ TUnits
      , tsymb [TSPurpose, TypogConvention [Vector Bold], SymbOrder, VectorUnits]
      , TAandA
      ],
  IntroSec $
    IntroProg justification (phrase projectileTitle)
      [ IScope scope ],
  SSDSec $
    SSDProg
      [ SSDProblem $ PDProg prob []
        [ TermsAndDefs Nothing terms
        , PhySysDesc projectileTitle physSystParts figLaunch []
        , Goals [(phrase iVel +:+ S "vector") `S.the_ofThe` phrase projectile]]
      , SSDSolChSpec $ SCSProg
        [ Assumptions
        , TMs [] (Label : stdFields)
        , GDs [] ([Label, Units] ++ stdFields) ShowDerivation
        , DDs [] ([Label, Symbol, Units] ++ stdFields) ShowDerivation
        , IMs [] ([Label, Input, Output, InConstraints, OutConstraints] ++ stdFields) ShowDerivation
        , Constraints EmptyS inConstraints
        , CorrSolnPpties outConstraints []
        ]
      ],
  ReqrmntSec $
    ReqsProg
      [ FReqsSub EmptyS []
      , NonFReqsSub
      ],
  TraceabilitySec $ TraceabilityProg $ traceMatStandard si,
  AuxConstntSec $
    AuxConsProg projectileTitle constants,
  Bibliography
  ]

justification, scope :: Sentence
justification = foldlSent [atStart projectile, phrase motion, S "is a common" +:+.
  phraseNP (problem `in_` physics), S "Therefore, it is useful to have a",
  phrase program, S "to solve and model these types of" +:+. plural problem,
  atStartNP (the program), S "documented here is called", phrase projectileTitle]
scope = foldlSent_ [phraseNP (NP.the (analysis `ofA` twoD)),
  sParen (getAcc twoD), phraseNP (combineNINI projectile motion), phrase problem, S "with",
  phrase constAccel]

projectileTitle :: CI
projectileTitle = commonIdea "projectileTitle" (pn "Projectile") "Projectile" []

si :: SystemInformation
si = SI {
  _sys         = projectileTitle,
  _kind        = Doc.srs,
  _authors     = [samCrawford, brooks, spencerSmith],
  _purpose     = [],
  _quants      = symbols,
  _concepts    = [] :: [DefinedQuantityDict],
  _instModels  = iMods,
  _datadefs    = dataDefs,
  _configFiles = [],
  _inputs      = inputs,
  _outputs     = outputs,
  _defSequence = [] :: [Block QDefinition],
  _constraints = map cnstrw constrained,
  _constants   = constants,
  _sysinfodb   = symbMap,
  _usedinfodb  = usedDB,
   refdb       = refDB
}

tMods :: [TheoryModel]
tMods = [accelerationTM, velocityTM]

symbMap :: ChunkDB
symbMap = cdb (qw pi_ : map qw physicscon ++ unitalQuants ++ symbols)
  (nw projectileTitle : nw mass : nw inValue : [nw errMsg, nw program] ++
    map nw doccon ++ map nw doccon' ++ map nw physicCon ++ map nw physicCon' ++
    map nw physicscon ++ map nw mathcon ++ concepts ++ unitalIdeas ++
    map nw acronyms ++ map nw symbols ++ map nw [metre, radian, second]) (cw pi_ : map cw constrained ++ srsDomains)
  (map unitWrapper [metre, radian, second]) dataDefs iMods genDefns tMods concIns [] [] allRefs

usedDB :: ChunkDB
usedDB = cdb ([] :: [QuantityDict]) (nw pi_ : map nw acronyms ++ map nw symbols)
  (cw pi_ : srsDomains) ([] :: [UnitDefn]) [] [] [] [] [] [] [] ([] :: [Reference])

stdFields :: Fields
stdFields = [DefiningEquation, Description Verbose IncludeUnits, Notes, Source, RefBy]

refDB :: ReferenceDB
refDB = rdb citations concIns

concIns :: [ConceptInstance]
concIns = assumptions ++ funcReqs ++ goals ++ nonfuncReqs

-------------------------
-- Problem Description --
-------------------------

prob :: Sentence
prob = foldlSent_ [S "efficiently" `S.and_` S "correctly predict the",
  phraseNP (landingPosNC `ofA` projectile)]

---------------------------------
-- Terminology and Definitions --
---------------------------------

terms :: [ConceptChunk]
terms = [launcher, projectile, target, gravity, cartesian, rectilinear]

---------------------------------
-- Physical System Description --
---------------------------------

physSystParts :: [Sentence]
physSystParts = map (!.)
  [atStartNP (the launcher),
  atStartNP (the projectile) +:+ sParen (S "with" +:+ getTandS iVel `S.and_` getTandS launAngle),
  atStartNP (the target)]

----------------------------------------------------
-- Various gathered data that should be automated --
----------------------------------------------------
symbols :: [QuantityDict]
symbols = qw gravitationalAccelConst : unitalQuants ++ map qw constants ++
  map qw [acceleration, constAccel, iPos, iSpeed, iVel, ixPos,
  iyPos, ixVel, iyVel, position, scalarPos, projSpeed, time, velocity, xAccel,
  xConstAccel, xPos, xVel, yAccel, yConstAccel, yPos, yVel]

constants :: [QDefinition]
constants = [gravitationalAccelConst, piConst, tol]

inputs :: [QuantityDict]
inputs = map qw [launSpeed, launAngle, targPos]

outputs :: [QuantityDict]
outputs = [message, qw offset]

unitalQuants :: [QuantityDict]
unitalQuants = message : map qw constrained

unitalIdeas :: [IdeaDict]
unitalIdeas = nw message : map nw constrained

inConstraints :: [UncertQ]
inConstraints = [launAngleUnc, launSpeedUnc, targPosUnc]

outConstraints :: [UncertQ]
outConstraints = [landPosUnc, offsetUnc]

constrained :: [ConstrConcept]
constrained = [flightDur, landPos, launAngle, launSpeed, offset, targPos]

acronyms :: [CI]
acronyms = [oneD, twoD, assumption, dataDefn, genDefn, goalStmt, inModel,
  physSyst, requirement, Doc.srs, thModel, typUnc]

-- References --
bodyRefs :: [Reference]
bodyRefs = map ref tMods ++ map ref concIns ++ map (ref.makeTabRef.getTraceConfigUID) (traceMatStandard si) ++ traceyGraphGetRefs "Projectile"

allRefs :: [Reference]
allRefs = nub (assumpRefs ++ bodyRefs ++ figRefs ++ goalRefs ++ dataDefRefs ++ genDefRefs
  ++ iModRefs ++ citeRefs ++ reqRefs ++ secRefs)

