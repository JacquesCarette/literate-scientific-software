module Drasil.DocLang.MIS where

import Language.Drasil
import Drasil.DocLang.GenBuilders (section')

import qualified Data.Drasil.Concepts.Documentation as Doc (accRoutSemantic, 
  assumption, consideration, enviroVar, expConstant, expType, misOfModule, 
  module_, moduleHierarchy, notation, semantic, stateInvar, stateVar, syntax, use)
import qualified Data.Drasil.Concepts.Software as Doc (expAccProgram)
  -- import ^ for section making

import Data.Drasil.Concepts.Documentation (design, document, documentation, form, 
  implementation, mg, mis, purpose, srs, statement, symbol_, templateModule)
import Data.Drasil.Concepts.Software (program)
  -- import ^ for paragraphs (pull out)

import Data.Drasil.SentenceStructures (foldlSP, inThe, ofThe, sAnd)

accRoutSemantics, assumptions, considerations, enviroVars, expAccPrograms, 
  expConstants, expTypes, module_, modHier, notation, semantics, stateInvars, 
  stateVars, syntax, uses :: [Contents] -> [Section] -> Section

modHier          cs ss = section' (titleize Doc.moduleHierarchy)  cs ss "ModHierarchy"
notation         cs ss = section' (titleize Doc.notation)         cs ss "Notation"

module_          cs ss = section' (titleize Doc.module_)          cs ss "Module"
uses             cs ss = section' (titleize' Doc.use)             cs ss "Uses"
syntax           cs ss = section' (titleize Doc.syntax)           cs ss "Syntax"
semantics        cs ss = section' (titleize' Doc.semantic)        cs ss "Semantics"
considerations   cs ss = section' (titleize' Doc.consideration)   cs ss "Considerations"

expConstants     cs ss = section' (titleize' Doc.expConstant)     cs ss "ExpConstants"
expTypes         cs ss = section' (titleize' Doc.expType)         cs ss "ExpTypes"
expAccPrograms   cs ss = section' (titleize' Doc.expAccProgram)   cs ss "ExpAccPrograms"

enviroVars       cs ss = section' (titleize' Doc.enviroVar)       cs ss "EnviroVars"
stateVars        cs ss = section' (titleize' Doc.stateVar)        cs ss "StateVars"
stateInvars      cs ss = section' (titleize' Doc.stateInvar)      cs ss "StateInvars"
assumptions      cs ss = section' (titleize' Doc.assumption)      cs ss "Assumptions" --FIXME: Assumptions already defined for SRS?
accRoutSemantics cs ss = section' (titleize' Doc.accRoutSemantic) cs ss "AccRoutSemantics"

misOfModule :: [Contents] -> [Section] -> String -> Section
misOfModule cs ss mod = section' (titleize $ Doc.misOfModule mod) cs ss $ "MISof" ++ mod ++ "Module"

--FIXME: All these contents need variability to be implemented in other examples

------------------
-- INTRODUCTION --
------------------

introMIS :: (Idea a) => a -> Sentence -> Contents
introMIS progName outerLink = foldlSP [S "The following", phrase document, S "details the", 
  titleize mis, S "for the implemented", plural Doc.module_ `inThe`
  phrase program, short progName :+: S ". It is intended to ease navigation through the",
  phrase program, S "for", phrase design, S "and maintenance" +:+. plural purpose,
  S "Complementary", plural document, S "include the", titleize srs, (sParen $ getAcc srs)
  `sAnd` titleize mg +:+. sParen (getAcc mg), S "The full", phrase documentation `sAnd`
  phrase implementation, S "can be found at", outerLink]
--FIXME: outerLink needs to work!

--------------
-- NOTATION --
--------------

notationIntroMIS :: Contents
notationIntroMIS = foldlSP [S "The structure" `ofThe` getAcc mis, S "for", plural Doc.module_,
  S "comes from Hoffman and Strooper (1995), with the addition that", plural templateModule, 
  S "have been adapted from Ghezzi et al. (2003). The mathematical", phrase Doc.notation, 
  S "comes from Chapter 3 of Hoffman and Strooper (1995). For instance, the", phrase symbol_, 
  S ":= is used for a multiple assignment", phrase statement `sAnd` S "conditional rules",
  S "follow the", phrase form, S "(c1 ) r1jc2 ) r2j:::jcn ) rn)"]

notTblIntro :: (Idea a) => a -> Contents
notTblIntro progName = mkParagraph $ S "The following table summarizes the primitive" +:+
  S "data types used by" +:+ (short progName)

--notationTable :: LabelledContent
--notationTable 

notationIntroContd :: (Idea a) => a -> Contents
notationIntroContd progName = foldlSP [S "The specification of", short progName, 
  S "uses some derived data types: sequences, strings, and tuples.", 
  S " Sequences are lists that represent a countable number",
  S " of ordered values of the same data type, where the same value may occur more than once.",
  S " Strings are sequences of characters. Tuples contain a list of values, potentially of ", 
  S "different types. In addition, ", short progName, S "uses functions, which are defined by the data types",
  S " of their inputs and outputs. Local functions are described by giving their type signature",
  S " followed by their specification"]

----------------------
-- MODULE HIERARCHY --
----------------------

modHierarchyPointer :: Sentence -> Contents
modHierarchyPointer mgLink = mkParagraph $ S "To view the Module Hierarchy, see" +:+ mgLink

---------------------------
-- HARDWARE MODULE INTRO --
---------------------------

hwModIntro :: Contents
hwModIntro = foldlSP [S "This module hides the underlying hardware for I/O (to the",
  S "screen, or file, or other device). In general it will be provided by the selected",
  S "programming language and operating system"]