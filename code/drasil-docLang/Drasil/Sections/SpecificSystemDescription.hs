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
  , inDataConstTbl, outDataConstTbl, propCorSolF, auxSpecSent
  , tInDataCstRef, tOutDataCstRef
  ) where

import Language.Drasil hiding (variable)
import Utils.Drasil
import Utils.Drasil.Concepts
import qualified Utils.Drasil.NounPhrase as NP
import qualified Utils.Drasil.Sentence as S

import Data.Drasil.Concepts.Documentation (assumption, column, constraint, corSol,
  datum, datumConstraint, inDatumConstraint, outDatumConstraint, definition, element, general, goalStmt, information,
  input_, limitation, model, output_, physical, physicalConstraint, physicalSystem,
  physSyst, problem, problemDescription, property, purpose, quantity, requirement,
  scope, section_, softwareConstraint, solutionCharacteristic, specification,
  symbol_, system, theory, typUnc, uncertainty, user, value, variable, table_, problemDescription)
import qualified Data.Drasil.Concepts.Documentation as DCD (sec)
import Data.Drasil.Concepts.Math (equation, parameter)

import Data.Drasil.TheoryConcepts (inModel, thModel, dataDefn, genDefn)

import qualified Drasil.DocLang.SRS as SRS

import Control.Lens ((^.))
import Data.Maybe

-- Takes the system and subsections.
-- | Specific System Description section builder.
specSysDescr :: [Section] -> Section
specSysDescr = SRS.specSysDes [intro_]

-- FIXME: this all should be broken down and mostly generated.
-- Generates an introduction based on the system.
-- Creates a general introduction for the Specific System Description section.
intro_ :: Contents
intro_ = mkParagraph $ foldlSent [S "This", phrase section_, S "first presents the", 
  phrase problemDescription `sC` S "which gives a high-level view of the",
  phrase problem, S "to be solved. This is followed by the", plural solutionCharacteristic,
  phrase specification `sC`  S "which presents the",
  foldlList Comma List [plural assumption, plural theory, plural definition], S "that are used"]

-- | Describes a problem the system is needed to accomplish.
probDescF :: Sentence -> [Section] -> Section
probDescF prob = SRS.probDesc [mkParagraph $ foldlSent [atStartNP (a_ system) `S.is` S "needed to", prob]]
                  
-- | Creates the Terms and Definitions section. Can take a ('Just' 'Sentence') if needed or 'Nothing' if not. Also takes 'Concept's that contain the definitions.
termDefnF :: Concept c => Maybe Sentence -> [c] -> Section
termDefnF end lst = SRS.termAndDefn [intro, enumBulletU $ map termDef lst] []
  where intro = foldlSP_ [
                  S "This subsection provides a list of terms that are used in the subsequent",
                  plural section_ `S.and_` S "their meaning, with the", phrase purpose `S.of_`
                  S "reducing ambiguity and making it easier to correctly understand the" +:+.
                  plural requirement, fromMaybe EmptyS end]
        termDef x = atStart x +: EmptyS +:+. capSent (x ^. defn)

-- | Similar to 'termDefnF', except does not take definitions from the list of terms. 
termDefnF' :: Maybe Sentence -> [Contents] -> Section
termDefnF' end otherContents = SRS.termAndDefn (intro : otherContents) []
      where intro = foldlSP [S "This subsection provides a list of terms", 
                    S "that are used in the subsequent", plural section_, 
                    S "and their meaning, with the", phrase purpose, 
                    S "of reducing ambiguity and making it easier to correctly", 
                    S "understand the", plural requirement :+: maybe EmptyS (S "." +:+) end]

-- | General introduction for the Physical System Description section.
physSystDesc :: Idea a => a -> [Sentence] -> LabelledContent -> [Contents] -> Section
physSystDesc progName parts fg other = SRS.physSyst (intro : bullets : LlC fg : other) []
  where intro = mkParagraph $ foldlSentCol [atStartNP (the physicalSystem) `S.of_` short progName `sC`
                S "as shown in the", refS fg, S "below" `sC` S "includes the following", plural element]
        bullets = enumSimpleU 1 (short physSyst) parts

-- | General constructor for the Goal Statement section. Takes the given inputs ('Sentence's) and the descriptions ('Contents').
goalStmtF :: [Sentence] -> [Contents] -> Section
goalStmtF givenInputs otherContents = SRS.goalStmt (intro:otherContents) []
  where intro = mkParagraph $ S "Given" +:+ foldlList Comma List givenInputs `sC` 
                pluralNP (the goalStmt) +: S "are"

-- | General introduction for the Solution Characteristics Specification section. Takes the program name and a section of instance models. 
solutionCharSpecIntro :: (Idea a) => a -> Section -> Contents
solutionCharSpecIntro progName instModelSection = foldlSP [atStartNP' (the inModel), 
  S "that govern", short progName, S "are presented in the" +:+. 
  namedRef instModelSection (titleize inModel +:+ titleize DCD.sec), atStartNP (the information), S "to understand", 
  S "meaning" `S.the_ofThe` plural inModel, 
  S "and their derivation is also presented, so that the", plural inModel, 
  S "can be verified"]


-- Wrappers for assumpIntro. Use assumpF' if genDefs is not needed
-- | Creates an Assumptions section by prepending a general introduction to other related 'Contents'.
assumpF :: [Contents] -> Section
assumpF otherContents = SRS.assumpt (assumpIntro : otherContents) []
  where
    assumpIntro = mkParagraph $ foldlSent 
      [S "This", phrase section_, S "simplifies the original", phrase problem,
       S "and helps in developing the", plural thModel, S "by filling in the", 
       S "missing", phrase information, S "for the" +:+. phrase physicalSystem,
       atStartNP' (the assumption), S "refine the", phrase scope,
       S "by providing more detail"]

-- | Wrapper for 'thModelIntro'. Takes the program name and other 'Contents'.
thModF :: (Idea a) => a -> [Contents] -> Section
thModF progName otherContents = SRS.thModel (thModIntro progName : otherContents) []

-- | Creates a eneralized Theoretical Model introduction given the program name.
thModIntro :: (Idea a) => a -> Contents
thModIntro progName = foldlSP [S "This", phrase section_, S "focuses on the",
  phrase general, plural equation `S.and_` S "laws that", short progName, S "is based on"]

-- | Creates a General Definitions section with a general introduction.
-- Takes in relevant general definitions ('Contents'). Use empty list if none are needed.
genDefnF :: [Contents] -> Section
genDefnF otherContents = SRS.genDefn (generalDefinitionIntro otherContents : otherContents) []

-- | Creates the introduction used in 'genDefnF'. If the given list is empty, the returned 'Sentence' is "There are no general definitions."
generalDefinitionIntro :: [t] -> Contents
generalDefinitionIntro [] = mkParagraph $ S "There are no general definitions."
generalDefinitionIntro _ = foldlSP [S "This", phrase section_, 
  S "collects the laws and", plural equation, 
  S "that will be used to build the", plural inModel]

                       
-- Similar to 'genDefnF', but for Data Definitions. It also uses 'EmptyS' if ending 'Sentence' is not needed rather than an empty list.
dataDefnF :: Sentence -> [Contents] -> Section
dataDefnF endingSent otherContents = SRS.dataDefn
  (dataDefinitionIntro endingSent : otherContents) []

-- | Creates a general Data Definition introduction. Appends the given 'Sentence' to the end.
dataDefinitionIntro :: Sentence -> Contents
dataDefinitionIntro closingSent = mkParagraph (foldlSent [S "This", phrase section_, 
    S "collects and defines all the", plural datum, 
    S "needed to build the", plural inModel] +:+ closingSent)

-- wrappers for inModelIntro. Use inModelF' if genDef are not needed
-- | Constructor for Instance Models. Takes the problem description,
-- data definition, theoretical model, general definition, and any other relevant contents.
inModelF :: Section -> Section -> Section -> Section -> [Contents] -> Section
inModelF probDes datDef theMod genDef otherContents = SRS.inModel 
  (inModelIntro probDes datDef theMod genDef : otherContents) []

-- | Creates a general Instance Model introduction. Requires four references to function. Nothing can be input into the last reference if only three tables are present.
inModelIntro :: Section -> Section -> Section -> Section -> Contents
inModelIntro r1 r2 r3 r4 = foldlSP [S "This", phrase section_, 
  S "transforms the", phrase problem, S "defined in the", namedRef r1 $ phrase problemDescription,
  S "into one which is expressed in mathematical terms. It uses concrete", 
  plural symbol_, S "defined in the", namedRef r2 $ plural dataDefn, S "to replace the abstract",
  pluralNP $ symbol_ `inThePP` model, S "identified in", namedRef r3 (plural thModel) `S.and_`
  namedRef r4 (plural genDefn)]

-- | Constructor for Data Constraints section. Takes a trailing 'Sentence' (use 'EmptyS' if none) and data constraints.
datConF :: (HasUncertainty c, Quantity c, Constrained c, HasReasVal c, MayHaveUnit c) => 
  Sentence -> [c] -> Section
datConF _ [] = SRS.datCon [mkParagraph (S "There are no" +:+. plural datumConstraint)] []
datConF t c  = SRS.datCon [dataConstraintParagraph t, LlC $ inDataConstTbl c] []
  
-- optional trailing sentence(s) -> data constraints tables -> Contents
-- | Constructor for the paragraph of the Data Constraints section. Takes in a trailing 'Sentence'.
dataConstraintParagraph :: Sentence -> Contents
dataConstraintParagraph trailingSent = foldlSP_ [inputTableSent, physConsSent,
  uncertSent, conservConsSent, typValSent, trailingSent]

-- | General 'Sentence' that describes the data constraints on the input variables.
inputTableSent :: Sentence
inputTableSent = foldlSent [S "The", namedRef (inDataConstTbl ([] :: [UncertQ])) $ titleize' datumConstraint +:+ titleize table_, S "shows the",
  pluralNP (datumConstraint `onThePS` input_), plural variable]

-- | General 'Sentence' that describes the physical constraints/limitations on the variables.
physConsSent :: Sentence
physConsSent = foldlSent [atStartNP $ NP.the $ column `for` physical,
  plural constraint, S "gives the",  phrase physical, plural limitation,
  S "on the range" `S.of_` plural value, S "that can be taken by the", phrase variable]

-- | General 'Sentence' that describes the uncertainty on the input variables.
uncertSent :: Sentence
uncertSent = foldlSent [atStartNP (the uncertainty), phrase column,
  S "provides an estimate of the confidence with which the", phrase physical,
  plural quantity +:+. S "can be measured", S "This", phrase information,
  S "would be part of the", phrase input_, S "if one were performing an",
  phrase uncertainty, S "quantification exercise"]

-- | General 'Sentence' that describes some conservative constraints on the model.
conservConsSent :: Sentence
conservConsSent = foldlSent [atStartNP' (the constraint) `S.are` S "conservative" `sC`
  S "to give", phrase user `S.the_ofThe` phrase model,
  S "the flexibility to experiment with unusual situations"]

-- | General 'Sentence' that describes the typical values.
typValSent :: Sentence
typValSent = foldlSent [atStartNP (the column) `S.of_` S "typical",
  plural value `S.is` S "intended to provide a feel for a common scenario"]

-- | General 'Sentence' that describes some auxiliary specifications of the system.
auxSpecSent :: Sentence
auxSpecSent = foldlSent [S "The", namedRef (SRS.valsOfAuxCons [] []) $ S "auxiliary constants", S "give",
  plural value `S.the_ofThe` phrase specification, plural parameter, S "used in the",
  namedRef (inDataConstTbl ([] :: [UncertQ])) $ titleize' datumConstraint +:+ titleize table_]

-- | Creates a Data Constraints table. Takes in Columns, reference, and a label.
mkDataConstraintTable :: [(Sentence, [Sentence])] -> String -> Sentence -> LabelledContent
mkDataConstraintTable col rf lab = llcc (makeTabRef rf) $ uncurry Table 
  (mkTableFromColumns col) lab True

-- | Creates the input Data Constraints Table.
inDataConstTbl :: (HasUncertainty c, Quantity c, Constrained c, HasReasVal c, MayHaveUnit c) => 
  [c] -> LabelledContent
inDataConstTbl qlst = mkDataConstraintTable [(S "Var", map ch $ sortBySymbol qlst),
            (titleize' physicalConstraint, map fmtPhys $ sortBySymbol qlst),
            (titleize' softwareConstraint, map fmtSfwr $ sortBySymbol qlst),
            (S "Typical Value", map (\q -> fmtU (eS $ getRVal q) q) $ sortBySymbol qlst),
            (short typUnc, map typUncr $ sortBySymbol qlst)]  (inDatumConstraint ^. uid) $
            titleize' inDatumConstraint
  where
    getRVal c = fromMaybe (error $ "getRVal found no Expr for " ++ (c ^. uid)) (c ^. reasVal)

-- | Creates the output Data Constraints Table.
outDataConstTbl :: (Quantity c, Constrained c) => [c] -> LabelledContent
outDataConstTbl qlst = mkDataConstraintTable [(S "Var", map ch qlst),
            (titleize' physicalConstraint, map fmtPhys qlst),
            (titleize' softwareConstraint, map fmtSfwr qlst)] (outDatumConstraint ^. uid) $
            titleize' outDatumConstraint

--Not actually used here, for exporting references
-- | Input/Output Data Constraint Table references.
tInDataCstRef, tOutDataCstRef :: Reference
tInDataCstRef = makeTabRef (inDatumConstraint ^. uid)
tOutDataCstRef = makeTabRef (outDatumConstraint ^. uid)

-- | Formats Physical Constraints into a 'Sentence'.
fmtPhys :: (Constrained c, Quantity c) => c -> Sentence
fmtPhys c = foldConstraints c $ filter isPhysC (c ^. constraints)

-- | Formats Software Constraints into a 'Sentence'.
fmtSfwr :: (Constrained c, Quantity c) => c -> Sentence
fmtSfwr c = foldConstraints c $ filter isSfwrC (c ^. constraints)

-- | Creates the Properties of a Correct Solution section.
propCorSolF :: (Quantity c, Constrained c) => [c] -> [Contents] -> Section
propCorSolF []  [] = SRS.propCorSol [mkParagraph noPropsSent] []
propCorSolF [] con = SRS.propCorSol con []
propCorSolF c  con = SRS.propCorSol ([propsIntro, LlC $ outDataConstTbl c] ++ con) []

-- | General 'Sentence' that states there are no properties of a correct solution. Used in 'propCorSolF'.
noPropsSent :: Sentence
noPropsSent = foldlSent [S "There are no", pluralNP $ property `ofAPS` corSol]

-- | Creates the Properties of a Correct Solution introduction.
propsIntro :: Contents
propsIntro = foldlSP_ [outputTableSent, physConsSent]

-- | Outputs a data constraint table as a 'Sentence'.
outputTableSent :: Sentence
outputTableSent = foldlSent [S "The", namedRef (outDataConstTbl ([] :: [UncertQ])) $ titleize' datumConstraint +:+ titleize table_, S "shows the",
  pluralNP (datumConstraint `onThePS` output_), plural variable]
