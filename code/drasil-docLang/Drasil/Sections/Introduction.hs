{-# LANGUAGE PostfixOperators #-}
module Drasil.Sections.Introduction (orgSec, introductionSection, purposeOfDoc, scopeOfRequirements, 
  charIntRdrF, purpDoc) where

import Language.Drasil
import qualified Drasil.DocLang.SRS as SRS (intro, prpsOfDoc, scpOfReq,
  charOfIR, orgOfDoc, goalStmt, thModel, inModel, sysCon)
import Drasil.DocumentLanguage.Definitions(Verbosity(..))
import Utils.Drasil
import Utils.Drasil.Concepts
import qualified Utils.Drasil.Sentence as S

import Data.Drasil.Concepts.Computation (algorithm)
import Data.Drasil.Concepts.Documentation as Doc (assumption, characteristic,
  decision, definition, desSpec, design, designDoc, document, documentation,
  environment, goal, goalStmt, implementation, intReader, model, organization,
  purpose, requirement, scope, section_, softwareDoc, softwareVAV, srs,
  theory, user, vavPlan, problem, information, systemConstraint)
import Data.Drasil.TheoryConcepts as Doc (inModel, thModel)
import Data.Drasil.Citations (parnasClements1986)


-----------------------
--     Constants     --
-----------------------

-- | 'Sentence' that explains the development process of a program.
developmentProcessParagraph :: Sentence
developmentProcessParagraph = foldlSent [S "This", phrase document, 
  S "will be used as a starting point for subsequent development", 
  S "phases, including writing the", phraseNP (desSpec `andThe` softwareVAV) +:+.
  S "plan", atStartNP (the designDoc), S "will show how the", 
  plural requirement, S "are to be realized, including", plural decision, 
  S "on the numerical", plural algorithm, S "and programming" +:+. 
  phrase environment, S "The", phrase vavPlan, 
  S "will show the steps that will be used to increase confidence in the",
  (phraseNP (softwareDoc `andThe` implementation) !.), S "Although",
  S "the", short srs, S "fits in a series of", plural document, 
  S "that follow the so-called waterfall", phrase model `sC` 
  S "the actual development process is not constrained", 
  S "in any way. Even when the waterfall model is not followed, as",
  S "Parnas and Clements point out", refS parnasClements1986 `sC`
  S "the most logical way to present the", phrase documentation,
  S "is still to", Quote (S "fake"), S "a rational", phrase design,
  S "process"]

-- | 'Sentence' containing the subsections of the Introduction.
introductionSubsections :: Sentence
introductionSubsections = foldlList Comma List (map (uncurry S.the_ofThe) 
  [(phrase scope, plural requirement), 
  (plural characteristic, phrase intReader),
  (phrase Doc.organization, phrase document)])

-------------------------
--                    --
-------------------------

-- | Constructor for the Introduction section. In order, the parameters are:
--
--     * problemIntroduction - 'Sentence' introducing the specific example problem.
--     * programDefinition  - 'Sentence' definition of the specific example.
--     * subSections        - List of subsections for this section.
introductionSection :: Sentence -> Sentence -> [Section] -> Section
introductionSection problemIntroduction programDefinition = SRS.intro 
  [mkParagraph problemIntroduction, overviewParagraph programDefinition]


-- | Constructor for the overview paragraph for the Introduction.
-- Takes the definition of the specific example being generated ('Sentence').
overviewParagraph :: Sentence -> Contents
overviewParagraph programDefinition = foldlSP [S "The following", phrase section_,
  S "provides an overview of the", introduceAbb srs, S "for" +:+. 
  programDefinition, S "This", phrase section_, S "explains the", phrase purpose,
  S "of this", phrase document `sC` introductionSubsections]


-- | Constructor for Purpose of Document section that each example controls.
purpDocPara1 :: CI -> Sentence 
purpDocPara1 proName = foldlSent [S "The primary purpose of this", phrase document, S "is to",
  S "record the", plural requirement, S "of the" +:+. titleize proName, 
  atStart' goal `sC` plural assumption `sC` plural thModel `sC` 
  plural definition `sC` S "and other", phrase model, S "derivation",
  phrase information, S "are specified" `sC` S "allowing the reader to fully",
  S "understand" `S.and_` S "verify the", phrase purpose `S.and_` S "scientific",
  S "basis of" +:+. short proName, S "With the exception of", 
  namedRef (SRS.sysCon [] []) (plural systemConstraint) `sC` S "this",
  short Doc.srs, S "will remain abstract, describing what", phrase problem,
  S "is being solved, but not how to solve it"] 

-- | Combines 'purpDocPara1' and 'developmentProcessParagraph'.
-- Verbosity controls if the 'developmentProcessParagraph' is added or not.
purpDoc :: CI -> Verbosity -> [Sentence]
purpDoc proName Verbose = [purpDocPara1 proName, developmentProcessParagraph]
purpDoc proName Succinct = [purpDocPara1 proName]

-- | Constructor for Purpose of Document subsection. Takes a list of 'Sentence's that:
--
--     * Given one element: explains the purpose of the specific example.
--     * Given two elements: explains the purpose of the specific example and the development process.
--     * Otherwise: Uses the default 'developmentProcessParagraph'.
purposeOfDoc :: [Sentence] -> Section
purposeOfDoc [purposeOfProgram] = SRS.prpsOfDoc [mkParagraph purposeOfProgram] []
purposeOfDoc [purposeOfProgram, developmentProcess] = SRS.prpsOfDoc 
  [mkParagraph purposeOfProgram, mkParagraph developmentProcess] []
purposeOfDoc _ = SRS.prpsOfDoc [mkParagraph developmentProcessParagraph] []

-- | Constructor for the Scope of Requirements subsection.
-- Takes in the main requirement for the program.
scopeOfRequirements :: Sentence -> Section
scopeOfRequirements req = SRS.scpOfReq [foldlSP
  [phrase scope `S.the_ofTheC` plural requirement, S "includes", req]] []

-- | Constructor for characteristics of the intended reader subsection.
-- Takes the program name ('Idea'), assumed knowledge ('Sentence's), topic-related subjects ('Sentence's),
-- knowledge assets ('Sentence's), and references ('Section').
charIntRdrF :: (Idea a) => a -> [Sentence] -> [Sentence] -> [Sentence] -> 
  Section -> Section
charIntRdrF progName assumed topic asset r = 
  SRS.charOfIR (intReaderIntro progName assumed topic asset r) []

-- | Helper that creates a paragraph. Called by 'charIntRdrF'. The parameters (in order) should be:
--
--     * program name,
--     * subjects the reader is assumed to understand,
--     * topic-related subjects that the reader should understand,
--     * subjects that would be an asset if the reader understood them,
--     * reference to User Characteristics section.
intReaderIntro :: (Idea a) => a -> [Sentence] -> [Sentence] -> [Sentence] ->
  Section -> [Contents]
intReaderIntro progName assumed topic asset sectionRef = 
  [foldlSP [S "Reviewers of this", phrase documentation,
  S "should have an understanding of" +:+.
  foldlList Comma List (assumed ++ topic), assetSent,
  atStartNP' (the user) `S.of_` short progName, S "can have a lower level" `S.of_`
  S "expertise, as explained" `S.in_` refS sectionRef]]
  where
    assetSent = case asset of
      [] -> EmptyS
      _  -> S "It would be an asset to understand" +:+. foldlList Comma List asset

-- | Constructor for the Organization of the Document section. Parameters should be
-- an introduction ('Sentence'), a resource for a bottom up approach ('NamedIdea'), reference to that resource ('Section'),
-- and any other relevant information ('Sentence').
orgSec :: NamedIdea c => Sentence -> c -> Section -> Sentence -> Section
orgSec i b s t = SRS.orgOfDoc (orgIntro i b s t) []

-- | Helper function that creates the introduction for the Organization of the Document section. Parameters should be
-- an introduction ('Sentence'), a resource for a bottom up approach ('NamedIdea'), reference to that resource ('Section'),
-- and any other relevant information ('Sentence').
orgIntro :: NamedIdea c => Sentence -> c -> Section -> Sentence -> [Contents]
orgIntro intro bottom bottomSec trailingSentence = [foldlSP [
  intro, S "The presentation follows the standard pattern of presenting" +:+.
  foldlList Comma List (map plural [nw Doc.goal, nw theory, nw definition, nw assumption]),
  S "For readers that would like a more bottom up approach" `sC`
  S "they can start reading the", namedRef bottomSec (plural bottom)`S.and_`
  S "trace back to find any additional information they require"],
  folder [refineChain (zip [goalStmt, thModel, inModel]
         [SRS.goalStmt [] [], SRS.thModel [] [], SRS.inModel [] []]), trailingSentence]]
  where
    folder = case trailingSentence of
      EmptyS -> foldlSP_
      _      -> foldlSP
