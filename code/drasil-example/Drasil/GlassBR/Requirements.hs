module Drasil.GlassBR.Requirements (funcReqs, funcReqsTables, inReqDesc, nonfuncReqs, reqRefs) where

import Control.Lens ((^.))

import Language.Drasil
import Drasil.DocLang (inReq, mkQRTuple, mkQRTupleRef, mkValsSourceTable)
import Drasil.DocLang.SRS (datCon, propCorSol)
import Theory.Drasil (DataDefinition)
import Utils.Drasil
import Utils.Drasil.Concepts
import qualified Utils.Drasil.NounPhrase as NP
import qualified Utils.Drasil.Sentence as S

import Data.Drasil.Concepts.Computation (inValue)
import Data.Drasil.Concepts.Documentation (assumption, characteristic, code,
  condition, datumConstraint, environment, funcReqDom, likelyChg, message, mg,
  mis, module_, nonFuncReqDom, output_, property, requirement, srs, system,
  traceyMatrix, type_, unlikelyChg, value, vavPlan)
import Data.Drasil.Concepts.Math (calculation)
import Data.Drasil.Concepts.PhysicalProperties (dimension)
import Data.Drasil.Concepts.Software (errMsg)

import Data.Drasil.TheoryConcepts (dataDefn, genDefn, inModel, thModel)

import Drasil.GlassBR.Assumptions (assumpSV, assumpGL, assumptionConstants)
import Drasil.GlassBR.Concepts (glass)
import Drasil.GlassBR.DataDefs (aspRat, dimLL, glaTyFac, hFromt, loadDF, nonFL, 
  risk, standOffDis, strDisFac, tolPre, tolStrDisFac)
import Drasil.GlassBR.IMods (iMods)
import Drasil.GlassBR.TMods (lrIsSafe, pbIsSafe)
import Drasil.GlassBR.Unitals (blast, isSafeLR, isSafePb, loadSF, notSafe,
  pbTolfail, safeMessage)

{--Functional Requirements--}

funcReqs :: [ConceptInstance]
funcReqs = [sysSetValsFollowingAssumps, checkInputWithDataCons,
  outputValsAndKnownValues, checkGlassSafety, outputValues]

funcReqsTables :: [LabelledContent]
funcReqsTables = [sysSetValsFollowingAssumpsTable, outputValuesTable]

sysSetValsFollowingAssumps, checkInputWithDataCons,
  outputValsAndKnownValues, checkGlassSafety, outputValues :: ConceptInstance

sysSetValsFollowingAssumps = cic "sysSetValsFollowingAssumps" sysSetValsFollowingAssumpsDesc "System-Set-Values-Following-Assumptions" funcReqDom
checkInputWithDataCons     = cic "checkInputWithDataCons"     checkInputWithDataConsDesc     "Check-Input-with-Data_Constraints"       funcReqDom
outputValsAndKnownValues   = cic "outputValsAndKnownValues"   outputValsAndKnownValuesDesc   "Output-Values-and-Known-Values"          funcReqDom
checkGlassSafety           = cic "checkGlassSafety"           checkGlassSafetyDesc           "Check-Glass-Safety"                      funcReqDom
outputValues               = cic "outputValues"               outputValuesDesc               "Output-Values"                           funcReqDom

inReqDesc, sysSetValsFollowingAssumpsDesc, checkInputWithDataConsDesc, outputValsAndKnownValuesDesc, checkGlassSafetyDesc :: Sentence

inReqDesc = foldlList Comma List [pluralNP (NP.the (combineNINI glass dimension)),
  phraseNP (type_ `of_` glass), phrase pbTolfail, pluralNP (characteristic `the_ofThePS` blast)]

sysSetValsFollowingAssumpsDesc = foldlSent [atStartNP (the system), S "shall set the known",
    plural value, S "as described in the table for", namedRef sysSetValsFollowingAssumpsTable (S "Required Assignments")]

sysSetValsFollowingAssumpsTable :: LabelledContent
sysSetValsFollowingAssumpsTable = mkValsSourceTable (mkQRTupleRef r2AQs r2ARs ++ mkQRTuple r2DDs) "ReqAssignments"
                                  (S "Required Assignments" `follows` sysSetValsFollowingAssumps)
  where
    r2AQs = qw loadSF   : map qw (take 4 assumptionConstants)
    r2ARs = assumpGL : replicate 4 assumpSV
    r2DDs = [loadDF, hFromt, glaTyFac, standOffDis, aspRat]

--FIXME:should constants, LDF, and LSF have some sort of field that holds
-- the assumption(s) that're being followed? (Issue #349)

checkInputWithDataConsDesc = foldlSent [atStartNP (the system), S "shall check the entered",
  plural inValue, S "to ensure that they do not exceed the" +:+. namedRef (datCon [] []) (plural datumConstraint), 
  S "If any" `S.ofThe` plural inValue, S "are out" `S.of_` S "bounds" `sC`
  S "an", phrase errMsg, S "is displayed" `S.andThe` plural calculation, S "stop"]

outputValsAndKnownValuesDesc = foldlSent [titleize output_, pluralNP (the inValue),
  S "from", refS (inReq EmptyS) `S.andThe` S "known", plural value,
  S "from", refS sysSetValsFollowingAssumps]

checkGlassSafetyDesc = foldlSent_ [S "If", eS (sy isSafePb $&& sy isSafeLR),
  sParen (S "from" +:+ refS pbIsSafe `S.and_` refS lrIsSafe) `sC`
  phrase output_, phraseNP (the message), Quote (safeMessage ^. defn),
  S "If the", phrase condition, S "is false, then", phrase output_,
  phraseNP (the message), Quote (notSafe ^. defn)]

outputValuesDesc :: Sentence
outputValuesDesc = foldlSent [titleize output_, pluralNP (the value), S "from the table for", namedRef outputValuesTable (S "Required Outputs")]

outputValuesTable :: LabelledContent
outputValuesTable = mkValsSourceTable (mkQRTuple iMods ++ mkQRTuple r6DDs) "ReqOutputs"
                              (S "Required" +:+ titleize' output_ `follows` outputValues)
  where
    r6DDs :: [DataDefinition]
    r6DDs = [risk, strDisFac, nonFL, glaTyFac, dimLL, tolPre, tolStrDisFac, hFromt, aspRat]

{--Nonfunctional Requirements--}

nonfuncReqs :: [ConceptInstance]
nonfuncReqs = [correct, verifiable, understandable, reusable, maintainable, portable]

correct :: ConceptInstance
correct = cic "correct" (foldlSent [
  atStartNP' (output_ `the_ofThePS` code), S "have the",
  plural property, S "described in", refS (propCorSol [] [])
  ]) "Correct" nonFuncReqDom
 
verifiable :: ConceptInstance
verifiable = cic "verifiable" (foldlSent [
  atStartNP (the code), S "is tested with complete",
  phrase vavPlan]) "Verifiable" nonFuncReqDom

understandable :: ConceptInstance
understandable = cic "understandable" (foldlSent [
  atStartNP (the code), S "is modularized with complete",
  phrase mg `S.and_` phrase mis]) "Understandable" nonFuncReqDom

reusable :: ConceptInstance
reusable = cic "reusable" (foldlSent [
  atStartNP (the code), S "is modularized"]) "Reusable" nonFuncReqDom

maintainable :: ConceptInstance
maintainable = cic "maintainable" (foldlSent [
  S "The traceability between", foldlList Comma List [plural requirement,
  plural assumption, plural thModel, plural genDefn, plural dataDefn, plural inModel,
  plural likelyChg, plural unlikelyChg, plural module_], S "is completely recorded in",
  plural traceyMatrix `S.inThe` getAcc srs `S.and_` phrase mg]) "Maintainable" nonFuncReqDom

portable :: ConceptInstance
portable = cic "portable" (foldlSent [
  atStartNP (the code), S "is able to be run in different", plural environment])
  "Portable" nonFuncReqDom

-- References --
reqRefs :: [Reference]
reqRefs = map ref funcReqsTables
  ++ map ref [datCon ([]::[Contents]) ([]::[Section]), propCorSol [] []]
  ++ map ref ([inReq EmptyS] ++ funcReqs ++ nonfuncReqs) ++ map ref [pbIsSafe, lrIsSafe]
  ++ map ref [loadDF, hFromt, glaTyFac, standOffDis, aspRat]
  ++ map ref [risk, strDisFac, nonFL, glaTyFac, dimLL, tolPre, tolStrDisFac, hFromt, aspRat]
  
