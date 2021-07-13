module Drasil.GamePhysics.Body where

import Data.List (nub)
import Language.Drasil hiding (organization, section)
import Language.Drasil.Printers (PrintingInformation(..), defaultConfiguration)
import Database.Drasil (Block(Parallel), ChunkDB, ReferenceDB, SystemInformation(SI),
  cdb, rdb, refdb, _authors, _purpose, _concepts, _constants, _constraints, _datadefs,
  _instModels, _configFiles, _defSequence, _inputs, _kind, _outputs, _quants, 
  _sys, _sysinfodb, _usedinfodb)
import Theory.Drasil (qdFromDD)
import Utils.Drasil
import Utils.Drasil.Concepts
import qualified Utils.Drasil.Sentence as S
import Drasil.DocLang (DerivationDisplay(..), DocSection(..), Emphasis(..),
  Field(..), Fields, InclUnits(IncludeUnits), IntroSec(..), IntroSub(..),
  RefSec(..), RefTab(..), SCSSub(..), SRSDecl, SSDSec(SSDProg), SSDSub(..),
  SolChSpec(SCSProg), TConvention(..), TSIntro(..), Verbosity(Verbose),
  OffShelfSolnsSec(..), GSDSec(..), GSDSub(..), TraceabilitySec(TraceabilityProg),
  ReqrmntSec(..), ReqsSub(..), AuxConstntSec(..), ProblemDescription(PDProg),
  PDSub(..), intro, mkDoc, tsymb, traceMatStandard, purpDoc, getTraceConfigUID,
  secRefs, fillTraceSI)
import qualified Drasil.DocLang.SRS as SRS
import Data.Drasil.Concepts.Computation (algorithm)
import Data.Drasil.Concepts.Documentation as Doc (assumption, concept,
  condition, consumer, document, endUser, environment, game, guide,
  input_, interface, object, organization, physical,
  physicalSim, physics, problem, product_, project, quantity, realtime,
  section_, simulation, software, softwareSys, srsDomains, system,
  systemConstraint, sysCont, task, template, user, doccon, doccon', property)
import qualified Data.Drasil.Concepts.Documentation as Doc (srs)
import Data.Drasil.TheoryConcepts as Doc (dataDefn, inModel)
import Data.Drasil.Concepts.Education (frstYr, highSchoolCalculus,
  highSchoolPhysics, educon)
import Data.Drasil.Concepts.Software (physLib, softwarecon)
import Data.Drasil.People (alex, luthfi, olu)
import Data.Drasil.SI_Units (metre, kilogram, second, newton, radian,
  derived, fundamentals, joule)
import Data.Drasil.Software.Products (openSource, prodtcon, sciCompS, videoGame)

import qualified Data.Drasil.Concepts.PhysicalProperties as CPP (ctrOfMass, dimension)
import qualified Data.Drasil.Concepts.Physics as CP (elasticity, physicCon, rigidBody, collision, damping)
import qualified Data.Drasil.Concepts.Math as CM (cartesian, equation, law,
  mathcon, mathcon', rightHand, line, point)
import qualified Data.Drasil.Quantities.Physics as QP (force, time)

import Drasil.GamePhysics.Assumptions (assumptions, assumpRefs)
import Drasil.GamePhysics.Changes (likelyChgs, unlikelyChgs, chgRefs)
import Drasil.GamePhysics.Concepts (gamePhysics, acronyms, threeD, twoD)
import Drasil.GamePhysics.DataDefs (dataDefs, dataDefRefs)
import Drasil.GamePhysics.Goals (goals, goalRefs)
import Drasil.GamePhysics.IMods (iMods, instModIntro, iModRefs)
import Drasil.GamePhysics.References (citations, koothoor2013, smithLai2005, citeRefs)
import Drasil.GamePhysics.Requirements (funcReqs, nonfuncReqs, reqRefs)
import Drasil.GamePhysics.TMods (tMods, tModRefs)
import Drasil.GamePhysics.Unitals (symbolsAll, outputConstraints,
  inputSymbols, outputSymbols, inputConstraints, defSymbols)
import Drasil.GamePhysics.GenDefs (generalDefns, genDefRefs)

srs :: Document
srs = mkDoc mkSRS (S.forGen titleize short) si

fullSI :: SystemInformation
fullSI = fillTraceSI mkSRS si

printSetting :: PrintingInformation
printSetting = PI symbMap Equational defaultConfiguration

resourcePath :: String
resourcePath = "../../../datafiles/GamePhysics/"

mkSRS :: SRSDecl
mkSRS = [RefSec $ RefProg intro [TUnits, tsymb tableOfSymbols, TAandA],
  IntroSec $ IntroProg para1_introduction_intro (short gamePhysics)
  [IPurpose $ purpDoc gamePhysics Verbose,
   IScope scope,
   IChar [] [S "rigid body dynamics", phrase highSchoolCalculus] [],
   IOrgSec organizationOfDocumentsIntro inModel (SRS.inModel [] []) EmptyS],
   GSDSec $ GSDProg [
    SysCntxt [sysCtxIntro, LlC sysCtxFig1, sysCtxDesc, sysCtxList],
    UsrChars [userCharacteristicsIntro], SystCons [] []],
   SSDSec $ SSDProg
      [ SSDProblem $ PDProg probDescIntro []
        [ TermsAndDefs Nothing terms
        , Goals [S "the kinematic" +:+ plural property `sC` S "and" +:+ plural QP.force +:+
                 sParen (S "including any" +:+ phrase CP.collision +:+ plural QP.force) +:+
                 S "applied on a set of" +:+ plural CP.rigidBody]]
      , SSDSolChSpec $ SCSProg
        [ Assumptions
        , TMs [] (Label : stdFields)
        , GDs [] ([Label, Units] ++ stdFields) ShowDerivation
        , DDs [] ([Label, Symbol, Units] ++ stdFields) ShowDerivation
        , IMs [instModIntro] ([Label, Input, Output, InConstraints, OutConstraints] ++ stdFields) ShowDerivation
        , Constraints EmptyS inputConstraints
        , CorrSolnPpties outputConstraints []
        ]
      ],
    ReqrmntSec $ ReqsProg [
      FReqsSub' [],
      NonFReqsSub
    ],
    LCsSec,
    UCsSec,
    OffShelfSolnsSec $ OffShelfSolnsProg offShelfSols,
    TraceabilitySec $ TraceabilityProg $ traceMatStandard si,
    AuxConstntSec $ AuxConsProg gamePhysics [],
    Bibliography]
      where tableOfSymbols = [TSPurpose, TypogConvention[Vector Bold], SymbOrder, VectorUnits]

si :: SystemInformation
si = SI {
  _sys         = gamePhysics,
  _kind        = Doc.srs,
  _authors     = [alex, luthfi, olu],
  _purpose     = purpDoc gamePhysics Verbose,
  -- FIXME: The _quants field should be filled in with all the symbols, however
  -- #1658 is why this is empty, otherwise we end up with unused (and probably
  -- should be removed) symbols. But that's for another time. This is "fine"
  -- because _quants are only used relative to #1658.
  _quants      =  [] :: [QuantityDict], -- map qw iMods ++ map qw symbolsAll,
  _concepts    = [] :: [DefinedQuantityDict],
  _instModels  = iMods,
  _datadefs    = dataDefs,
  _configFiles = [],
  _inputs      = inputSymbols,
  _outputs     = outputSymbols, 
  _defSequence = map (`Parallel` []) qDefs,
  _constraints = inputConstraints,
  _constants   = [],
  _sysinfodb   = symbMap,
  _usedinfodb  = usedDB,
   refdb       = refDB
}
  where qDefs = map qdFromDD dataDefs

concIns :: [ConceptInstance]
concIns = assumptions ++ goals ++ likelyChgs ++ unlikelyChgs ++ funcReqs ++ nonfuncReqs

section :: [Section]
section = extractSection srs

stdFields :: Fields
stdFields = [DefiningEquation, Description Verbose IncludeUnits, Notes, Source, RefBy]

--FIXME: Need to be able to print defn for gravitational constant.

refDB :: ReferenceDB
refDB = rdb citations concIns

--FIXME: All named ideas, not just acronyms.

units :: [UnitDefn] -- FIXME
units = map unitWrapper [metre, kilogram, second, joule] ++ map unitWrapper [newton, radian]

symbMap :: ChunkDB
symbMap = cdb (map qw iMods ++ map qw symbolsAll) (map nw symbolsAll
  ++ map nw acronyms ++ map nw prodtcon ++ map nw generalDefns ++ map nw iMods
  ++ map nw softwarecon ++ map nw doccon ++ map nw doccon'
  ++ map nw CP.physicCon ++ map nw educon ++ [nw algorithm] ++ map nw derived
  ++ map nw fundamentals ++ map nw CM.mathcon ++ map nw CM.mathcon') 
  (map cw defSymbols ++ srsDomains ++ map cw iMods) units dataDefs
  iMods generalDefns tMods concIns section [] allRefs

usedDB :: ChunkDB
usedDB = cdb ([] :: [QuantityDict]) (map nw symbolsAll ++ map nw acronyms)
  ([] :: [ConceptChunk]) ([] :: [UnitDefn]) [] [] [] [] [] [] [] ([] :: [Reference])

--FIXME: The SRS has been partly switched over to the new docLang, so some of
-- the sections below are now redundant. I have not removed them yet, because
-- it makes it easier to test between the two different versions as changes
-- are introduced. Once the SRS is entirely switched to docLang, the redundant
-- sections should be removed.

-- =================================== --
-- SOFTWARE REQUIREMENTS SPECIFICATION --
-- =================================== --

------------------------------
-- Section : INTRODUCTION --
------------------------------

para1_introduction_intro :: Sentence
para1_introduction_intro = foldlSent
  [S "Due to the rising cost of developing", plural videoGame `sC` 
  S "developers are looking for ways to save time and money for their" +:+.
  plural project, S "Using an", phrase openSource, 
  phrase physLib,
  S "that is reliable and free will cut down development costs and lead",
  S "to better quality", plural product_]

-------------------------------
-- 2.1 : Purpose of Document --
-------------------------------
-- Purpose of Document automatically generated in IPurpose


---------------------------------
-- 2.2 : Scope of Requirements --
---------------------------------
scope :: Sentence
scope = foldlSent_ [phraseNP (the physicalSim) `S.of_` getAcc twoD,
  plural CP.rigidBody, S "acted on by", plural QP.force]

--scope_of_requirements_intro_p2 = EmptyS
  
{-scope_of_requirements_intro_p2 = foldlSent_ [S "simulates how these", 
  plural CP.rigidBody, S "interact with one another"]
-}
----------------------------------------------
-- 2.3 : Characteristics of Intended Reader --
----------------------------------------------

-------------------------------------
-- 2.3 : Organization of Documents --
-------------------------------------

organizationOfDocumentsIntro :: Sentence
organizationOfDocumentsIntro = foldlSent 
  [atStartNP (the organization), S "of this", phrase document, 
  S "follows the", phrase template, S "for an", getAcc Doc.srs, S "for", 
  phrase sciCompS, S "proposed by", refS koothoor2013 `S.and_`
  refS smithLai2005]

--------------------------------------------
-- Section 3: GENERAL SYSTEM DESCRIPTION --
--------------------------------------------
--------------------------
-- 3.1 : System Context --
--------------------------

sysCtxIntro :: Contents
sysCtxIntro = foldlSP
  [refS sysCtxFig1, S "shows the" +:+. phrase sysCont,
   S "A circle represents an entity external to the", phrase software
   `sC` phraseNP (the user), S "in this case. A rectangle represents the",
   phrase softwareSys, S "itself", sParen (short gamePhysics) +:+. EmptyS,
   S "Arrows are used to show the data flow between the", phraseNP (system
   `andIts` environment)]

sysCtxFig1 :: LabelledContent
sysCtxFig1 = llcc (makeFigRef "sysCtxDiag") $ fig (titleize sysCont) 
  (resourcePath ++ "sysctx.png")

sysCtxDesc :: Contents
sysCtxDesc = foldlSPCol [S "The interaction between the", phraseNP (product_
   `andThe` user), S "is through an application programming" +:+.
   phrase interface, S "The responsibilities of the", phraseNP (user 
   `andThe` system), S "are as follows"]

sysCtxUsrResp :: [Sentence]
sysCtxUsrResp = [S "Provide initial" +:+ pluralNP (condition `ofThePS`
  physical) +:+ S "state of the" +:+ phrase simulation `sC`
  plural CP.rigidBody +:+ S "present, and" +:+ plural QP.force +:+.
  S "applied to them",
  S "Ensure application programming" +:+ phrase interface +:+
  S "use complies with the" +:+ phrase user +:+. phrase guide,
  S "Ensure required" +:+ phrase software +:+ plural assumption +:+
  sParen (refS $ SRS.assumpt ([]::[Contents]) ([]::[Section])) +:+ 
  S "are appropriate for any particular" +:+
  phrase problem +:+ phraseNP (the software) +:+. S "addresses"]

sysCtxSysResp :: [Sentence]
sysCtxSysResp = [S "Determine if the" +:+ pluralNP (input_ `and_PS`
    simulation) +:+ S "state satisfy the required" +:+
    (phrase physical `S.and_` plural systemConstraint) +:+. sParen(refS $ SRS.datCon ([]::[Contents]) ([]::[Section])),
  S "Calculate the new state of all" +:+ plural CP.rigidBody +:+
    S "within the" +:+ phrase simulation +:+ S "at each" +:+
    phrase simulation +:+. S "step",
  S "Provide updated" +:+ phrase physical +:+ S "state of all" +:+
    plural CP.rigidBody +:+ S "at the end of a" +:+ phrase simulation +:+.
    S "step"]

sysCtxResp :: [Sentence]
sysCtxResp = [titleize user +:+ S "Responsibilities",
  short gamePhysics +:+ S "Responsibilities"]

sysCtxList :: Contents
sysCtxList = UlC $ ulcc $ Enumeration $ bulletNested sysCtxResp $
  map bulletFlat [sysCtxUsrResp, sysCtxSysResp]

--------------------------------
-- 3.2 : User Characteristics --
--------------------------------

userCharacteristicsIntro :: Contents
userCharacteristicsIntro = foldlSP
  [S "The", phrase endUser `S.of_` short gamePhysics,
  S "should have an understanding of", phrase frstYr, S "programming",
  plural concept `S.and_` S "an understanding of", phrase highSchoolPhysics]

-------------------------------
-- 3.3 : System Constraints  --
-------------------------------

---------------------------------------------
-- SECTION 4 : SPECIFIC SYSTEM DESCRIPTION --
---------------------------------------------

-- NOTE: Section 4 remains incomplete. General definitions and instance models
-- have not been encoded.

-------------------------------
-- 4.1 : Problem Description --
-------------------------------

probDescIntro :: Sentence
probDescIntro = foldlSent_
  [S "create a", foldlList Comma List $ map S ["simple", "lightweight", "fast", "portable"],
  getAcc twoD, phrase CP.rigidBody, phrase physLib `sC` S "which will allow for more accessible",
  phrase game, S "development" `S.and_` S "the production of higher quality" +:+. plural product_,
  S "Creating a gaming", phrase physLib, S "is a difficult" +:+. phrase task, titleize' game,
  S "need",  plural physLib, S "that simulate", plural object, S "acting under various", phrase physical,
  plural condition `sC` S "while simultaneously being fast and efficient enough to work in soft",
  phrase realtime, S "during the" +:+. phrase game, S "Developing a", 
  phrase physLib, S "from scratch takes a long period" `S.of_` phrase QP.time `S.and_`
  S "is very costly" `sC` S "presenting barriers of entry which make it difficult for",
  phrase game, S "developers to include", phrase Doc.physics, S "in their" +:+. 
  plural product_, S "There are a few free" `sC` phrase openSource `S.and_` S "high quality",
  plural physLib, S "available to be used for", phrase consumer, plural product_,
  sParen (refS $ SRS.offShelfSol ([] :: [Contents]) ([] :: [Section]))]
  
-----------------------------------------
-- 4.1.1 : Terminology and Definitions --
-----------------------------------------

terms :: [ConceptChunk]
terms = [CP.rigidBody, CP.elasticity, CPP.ctrOfMass, CM.cartesian, CM.rightHand, CM.line, CM.point, CP.damping]

-----------------------------
-- 4.1.2 : Goal Statements --
-----------------------------

--------------------------------------------------
-- 4.2 : Solution Characteristics Specification --
--------------------------------------------------

-------------------------
-- 4.2.1 : Assumptions --
-------------------------

--------------------------------
-- 4.2.2 : Theoretical Models --
--------------------------------

---------------------------------
-- 4.2.3 : General Definitions --
---------------------------------

generalDefinitionsIntro :: Contents
-- general_definitions_GDefs :: [Contents]

generalDefinitionsIntro = foldlSP 
  [S "This", phrase section_, S "collects the", pluralNP (CM.law `and_PP` 
  CM.equation), S "that will be used in deriving the", 
  plural dataDefn `sC` S "which in turn will be used to build the", 
  plural inModel]

-- GDefs not yet implemented --
{-
general_definitions_GDefs :: [Contents]
general_definitions_GDefs = map (Definition . General) gDefs)
-}

------------------------------
-- 4.2.4 : Data Definitions --
------------------------------

dataDefinitionsIntro :: Sentence
dataDefinitionsIntro = foldlSent [atStartNP (the CPP.dimension)
   `S.of_` S "each", phrase quantity, S "is also given"]

-----------------------------
-- 4.2.5 : Instance Models --
-----------------------------

------------------------------
-- Collision Diagram        --
------------------------------
{-- should be paired with the last instance model for this example
secCollisionDiagram = Paragraph $ foldlSent [ S "This section presents an image", 
  S "of a typical collision between two 2D rigid bodies labeled A and B,"  
  S "showing the position of the two objects, the collision normal vector n and",
  S "the vectors from the approximate center of mass of each object to the point",
  S "of collision P, rAP and rBP. Note that this figure only presents", 
  S "vertex-to-edge collisions, as per our assumptions (A5)."]
--}

{--fig_1 = Figure (titleize figure +:+ S "1:" +:+ S "Collision between two rigid bodies")
"CollisionDiagram.png" --}

------------------------------
-- SECTION 5 : REQUIREMENTS --
------------------------------
-- in Requirements.hs

-----------------------------------
-- 5.1 : Functional Requirements --
-----------------------------------

--------------------------------------
-- 5.2 : Nonfunctional Requirements --
--------------------------------------

--------------------------------
-- SECTION 6 : LIKELY CHANGES --
--------------------------------

-----------------------------------------
-- SECTION 7 : OFF-THE-SHELF SOLUTIONS --
-----------------------------------------

offShelfSols :: [Contents]
offShelfSols = [offShelfSolsIntro, offShelfSols2DList,
                offShelfSolsMid,   offShelfSols3DList]

offShelfSolsIntro, offShelfSols2DList, 
  offShelfSolsMid, offShelfSols3DList :: Contents

offShelfSolsIntro = mkParagraph $ foldlSentCol 
  [S "As mentioned in", refS (SRS.probDesc [] []) `sC`
  S "there already exist free", phrase openSource, phrase game +:+.
  plural physLib, S "Similar", getAcc twoD, plural physLib, S "are"]

offShelfSols2DList = enumBulletU [S "Box2D: http://box2d.org/",
  S "Nape Physics Engine: http://napephys.com/"]

offShelfSolsMid = mkParagraph $ foldl (+:+) EmptyS [S "Free", phrase openSource,
  getAcc threeD, phrase game, plural physLib, S "include:"]

offShelfSols3DList = enumBulletU [
  S "Bullet: http://bulletphysics.org/",
  S "Open Dynamics Engine: http://www.ode.org/",
  S "Newton Game Dynamics: http://newtondynamics.com/"]

-----------------------------------------------------
-- SECTION 8 : Traceability Matrices and Graph    --
-----------------------------------------------------

-----------------------------------
-- VALUES OF AUXILIARY CONSTANTS --
-----------------------------------

----------------
-- REFERENCES --
----------------
-- To be added --

-- References --
bodyRefs :: [Reference]
bodyRefs = ref sysCtxFig1: map ref concIns ++ map ref section ++ map (ref.makeTabRef.getTraceConfigUID) (traceMatStandard si)

allRefs :: [Reference]
allRefs = nub (assumpRefs ++ bodyRefs ++ chgRefs ++ goalRefs ++ dataDefRefs ++ genDefRefs
  ++ iModRefs ++ tModRefs ++ citeRefs ++ reqRefs ++ secRefs)
