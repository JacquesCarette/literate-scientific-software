module Drasil.Sections.SpecificSystemDescription 
  ( specSysDescr
  , probDescF
  , termDefnF, termDefnF'
  , physSystDesc
  , goalStmtF
  , solutionCharSpecIntro 
  , assumpF
  , thModF
  , genDefnF
  , dataDefnF
  , inModelF
  , datConF
  , dataConstraintUncertainty
  , inDataConstTbl, outDataConstTbl 
  , listofTablesToRefs
  ) where

import Language.Drasil
import Utils.Drasil

import Data.Drasil.Concepts.Documentation (assumption, column, constraint,
  datum, datumConstraint, definition, element, general, goalStmt, information,
  input_, limitation, model, output_, physical, physicalConstraint, physicalSystem,
  physSyst, problem, problemDescription, purpose, quantity, requirement, scope,
  section_, softwareConstraint, solutionCharacteristic, specification, symbol_,
  system, theory, typUnc, uncertainty, user, value, variable)
import Data.Drasil.Concepts.Math (equation)

import Data.Drasil.IdeaDicts (inModel, thModel)

import qualified Drasil.DocLang.SRS as SRS

import Control.Lens ((^.))

-- | Specific System description section builder. Takes the system and subsections.
specSysDescr :: [Section] -> Section
specSysDescr = SRS.specSysDes [intro_]

-- FIXME: this all should be broken down and mostly generated.
-- Generates an introduction based on the system.
intro_ :: Contents
intro_ = mkParagraph $ foldlSent [S "This", phrase section_, S "first presents the", 
  phrase problemDescription `sC` S "which gives a high-level view of the",
  phrase problem, S "to be solved. This is followed by the", plural solutionCharacteristic,
  phrase specification `sC`  S "which presents the",
  foldlList Comma List [plural assumption, plural theory, plural definition], S "that are used"]

-- describe what a system is needed to accomplist
probDescF :: Sentence -> [Section] -> Section
probDescF prob = SRS.probDesc [mkParagraph $ foldlSent [S "A", phrase system `sIs` S "needed to", prob]]
                  
--can take a (Just sentence) if needed or Nothing if not
termDefnF :: Maybe Sentence -> [Contents] -> Section
termDefnF end otherContents = SRS.termAndDefn (intro : otherContents) []
      where lastF Nothing  = EmptyS
            lastF (Just s) = S "." +:+ s
            intro = foldlSP [S "This subsection provides a list of terms", 
                    S "that are used in the subsequent", plural section_, 
                    S "and their meaning, with the", phrase purpose, 
                    S "of reducing ambiguity and making it easier to correctly", 
                    S "understand the", plural requirement :+: lastF end]

termDefnF' :: Maybe Sentence -> [Contents] -> Section
termDefnF' end otherContents = SRS.termAndDefn (intro : otherContents) []
      where lastF Nothing  = EmptyS
            lastF (Just s) = S "." +:+ s
            intro = foldlSP [S "This subsection provides a list of terms", 
                    S "that are used in the subsequent", plural section_, 
                    S "and their meaning, with the", phrase purpose, 
                    S "of reducing ambiguity and making it easier to correctly", 
                    S "understand the", plural requirement :+: lastF end]

--general introduction for Physical System Description
physSystDesc :: (Idea a) => a -> [Sentence] -> LabelledContent -> [Contents] -> Section
physSystDesc progName parts fg other = SRS.physSyst (intro : bullets : LlC fg : other) []
  where intro = mkParagraph $ foldlSentCol [S "The", phrase physicalSystem `sOf` short progName `sC`
                S "as shown in", makeRef2S fg `sC` S "includes the following", plural element]
        bullets = enumSimpleU 1 (short physSyst) parts

--List all the given inputs. Might be possible to use ofThe combinator from utils.hs
goalStmtF :: [Sentence] -> [Contents] -> Section
goalStmtF givenInputs otherContents = SRS.goalStmt (intro:otherContents) []
  where intro = mkParagraph $ S "Given" +:+ foldlList Comma List givenInputs `sC` S "the" +:+ 
                plural goalStmt +: S "are"


solutionCharSpecIntro :: (Idea a) => a -> Section -> Contents
solutionCharSpecIntro progName instModelSection = foldlSP [S "The", plural inModel, 
  S "that govern", short progName, S "are presented in" +:+. 
  makeRef2S instModelSection, S "The", phrase information, S "to understand", 
  S "meaning" `ofThe` plural inModel, 
  S "and their derivation is also presented, so that the", plural inModel, 
  S "can be verified"]


-- wrappers for assumpIntro. Use assumpF' if genDefs is not needed
assumpF :: [Contents] -> Section
assumpF otherContents = SRS.assumpt (assumpIntro : otherContents) []
  where
    assumpIntro = mkParagraph $ foldlSent 
      [S "This", phrase section_, S "simplifies the original", phrase problem,
       S "and helps in developing the", plural thModel, S "by filling in the", 
       S "missing", phrase information, S "for the" +:+. phrase physicalSystem,
       S "The", plural assumption, S "refine the", phrase scope,
       S "by providing more detail"]

--wrapper for thModelIntro
thModF :: (Idea a) => a -> [Contents] -> Section
thModF progName otherContents = SRS.thModel (thModIntro progName : otherContents) []

-- generalized theoretical model introduction: identifies key word pertaining to topic
thModIntro :: (Idea a) => a -> Contents
thModIntro progName = foldlSP [S "This", phrase section_, S "focuses on the",
  phrase general, plural equation `sAnd` S "laws that", short progName, S "is based on"]

-- just supply the other contents for General Definition. Use empty list if none needed
genDefnF :: [Contents] -> Section
genDefnF otherContents = SRS.genDefn (generalDefinitionIntro otherContents : otherContents) []

generalDefinitionIntro :: [t] -> Contents
generalDefinitionIntro [] = mkParagraph $ S "There are no general definitions."
generalDefinitionIntro _ = foldlSP [S "This", phrase section_, 
  S "collects the laws and", plural equation, 
  S "that will be used to build the", plural inModel]

                       
-- uses EmptyS if ending sentence is not needed
dataDefnF :: Sentence -> [Contents] -> Section                      
dataDefnF endingSent otherContents = SRS.dataDefn 
  (dataDefinitionIntro endingSent : otherContents) []


dataDefinitionIntro :: Sentence -> Contents
dataDefinitionIntro closingSent = mkParagraph (foldlSent [S "This", phrase section_, 
    S "collects and defines all the", plural datum, 
    S "needed to build the", plural inModel] +:+ closingSent)

-- wrappers for inModelIntro. Use inModelF' if genDef are not needed
inModelF :: Section -> Section -> Section -> Section -> [Contents] -> Section
inModelF probDes datDef theMod genDef otherContents = SRS.inModel 
  (inModelIntro probDes datDef theMod genDef : otherContents) []

-- just need to provide the four references in order to this function. Nothing can be input into r4 if only three tables are present
inModelIntro :: Section -> Section -> Section -> Section -> Contents
inModelIntro r1 r2 r3 r4 = foldlSP [S "This", phrase section_, 
  S "transforms the", phrase problem, S "defined in", makeRef2S r1,
  S "into one which is expressed in mathematical terms. It uses concrete", 
  plural symbol_, S "defined in", makeRef2S r2, 
  S "to replace the abstract", plural symbol_, S "in the", 
  plural model, S "identified in", makeRef2S r3 :+: end]
    where end = S " and" +:+ makeRef2S r4

-- wrapper for datConPar
datConF :: Sentence -> Sentence -> Sentence -> [LabelledContent] -> Section
datConF hasUncertainty mid trailing tables = SRS.datCon 
  (dataConstraintParagraph hasUncertainty (listofTablesToRefs tables) mid trailing : map LlC tables) []
  
-- reference to the input/ ouput tables -> optional middle sentence(s) (use EmptyS if not wanted) -> 
-- True if standard ending sentence wanted -> optional trailing sentence(s) -> Contents
dataConstraintParagraph :: Sentence -> Sentence -> Sentence -> Sentence -> Contents
dataConstraintParagraph hasUncertainty tableRef middleSent trailingSent = mkParagraph $
  dataConstraintIntroSent tableRef +:+ middleSent +:+ 
  dataConstraintClosingSent hasUncertainty trailingSent

-- makes a list of references to tables takes
-- l  list of layout objects that can be referenced
-- outputs a sentence containing references to the layout objects 
listofTablesToRefs :: (HasShortName l, Referable l) => [l] -> Sentence
listofTablesToRefs  []     = EmptyS
listofTablesToRefs  [x]    = makeRef2S x +:+ S "shows"
listofTablesToRefs  [x,y]  = makeRef2S x `sAnd` makeRef2S y +:+ S "show" -- for proper grammar with multiple tables
                                                                                    -- no Oxford comma in case there is only two tables to be referenced
listofTablesToRefs  (x:xs) = makeRef2S x `sC` listofTablesToRefs xs
 
dataConstraintIntroSent :: Sentence -> Sentence
dataConstraintIntroSent tableRef = foldlSent [tableRef, S "the", plural datumConstraint, S "on the", phrase input_, 
  S "and", phrase output_ +:+. (plural variable `sC` S "respectively"), S "The", 
  phrase column, S "for", phrase physical, plural constraint, S "gives the", 
  phrase physical, plural limitation, S "on the range of", plural value, 
  S "that can be taken by the", phrase variable]

dataConstraintClosingSent :: Sentence -> Sentence -> Sentence
dataConstraintClosingSent uncertaintySent trailingSent = foldlSent
  [S "The", plural constraint, S "are conservative, to give", 
  phrase user `ofThe` phrase model, S "the flexibility to", 
  S "experiment with unusual situations. The", phrase column, S "of typical",
  plural value, S "is intended to provide a feel for a common scenario"]
  +:+ uncertaintySent +:+ trailingSent

dataConstraintUncertainty :: Sentence
dataConstraintUncertainty = foldlSent [S "The", phrase uncertainty, phrase column,
  S "provides an estimate of the confidence with which the", phrase physical,
  plural quantity +:+. S "can be measured", S "This", phrase information,
  S "would be part of the", phrase input_, S "if one were performing an",
  phrase uncertainty, S "quantification exercise"]

mkDataConstraintTable :: [(Sentence, [Sentence])] -> String -> Sentence -> LabelledContent
mkDataConstraintTable col ref lab = llcc (makeTabRef ref) $ uncurry Table 
  (mkTableFromColumns col) lab True

-- Creates the input Data Constraints Table
inDataConstTbl :: (HasUncertainty c, Quantity c, Constrained c, HasReasVal c, MayHaveUnit c) => 
  [c] -> LabelledContent
inDataConstTbl qlst = mkDataConstraintTable [(S "Var", map ch $ sortBySymbol qlst),
            (titleize' physicalConstraint, map fmtPhys $ sortBySymbol qlst),
            (titleize' softwareConstraint, map fmtSfwr $ sortBySymbol qlst),
            (S "Typical Value", map (\q -> fmtU (E $ getRVal q) q) $ sortBySymbol qlst),
            (short typUnc, map typUncr $ sortBySymbol qlst)]  "InDataConstraints" $
            S "Input Data Constraints"
  where
    getRVal c = uns (c ^. reasVal)
      where uns (Just e) = e
            uns Nothing  = error $ "getRVal found no Expr for " ++ (c ^. uid)


-- Creates the output Data Constraints Table
outDataConstTbl :: (Quantity c, Constrained c) => [c] -> LabelledContent
outDataConstTbl qlst = mkDataConstraintTable [(S "Var", map ch qlst),
            (titleize' physicalConstraint, map fmtPhys qlst),
            (titleize' softwareConstraint, map fmtSfwr qlst)] "OutDataConstraints" $
            S "Output Data Constraints"

-- | formats physical constraints
fmtPhys :: (Constrained c, Quantity c) => c -> Sentence
fmtPhys c = foldConstraints c $ filter isPhysC (c ^. constraints)

-- | formats software constraints
fmtSfwr :: (Constrained c, Quantity c) => c -> Sentence
fmtSfwr c = foldConstraints c $ filter isSfwrC (c ^. constraints)
