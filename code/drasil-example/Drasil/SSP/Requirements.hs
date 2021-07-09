module Drasil.SSP.Requirements (funcReqs, funcReqTables, nonFuncReqs, reqRefs) where

import Language.Drasil
import Utils.Drasil
import Utils.Drasil.Concepts
import qualified Utils.Drasil.Sentence as S

import Drasil.DocLang (mkInputPropsTable, inReq)
import Drasil.DocLang.SRS (datCon, propCorSol) 

import Data.Drasil.Concepts.Computation (inDatum)
import Data.Drasil.Concepts.Documentation (assumption, code,
  datum, funcReqDom, input_, likelyChg, mg, mis, module_, name_, nonFuncReqDom,
  output_, physicalConstraint, property, requirement, srs, symbol_,
  traceyMatrix, unlikelyChg, user, value, propOfCorSol)
import Data.Drasil.Concepts.Physics (twoD)
import Data.Drasil.TheoryConcepts (dataDefn, genDefn, inModel, thModel)

import Drasil.SSP.Defs (crtSlpSrf, slope, slpSrf)
import Drasil.SSP.IMods (fctSfty, nrmShrFor, intsliceFs, crtSlpId)
import Drasil.SSP.Unitals (constF, coords, fs, fsMin, intNormForce, 
  intShrForce, inputs, xMaxExtSlip, xMaxEtrSlip, xMinExtSlip, xMinEtrSlip, 
  yMaxSlip, yMinSlip)

{-Functional Requirements-}

funcReqs :: [ConceptInstance]
funcReqs = [readAndStore, verifyInput, determineCritSlip, verifyOutput, 
  displayInput, displayGraph, displayFS, displayNormal, displayShear, 
  writeToFile]

funcReqTables :: [LabelledContent]
funcReqTables = [inputDataTable, inputsToOutputTable]

readAndStore, verifyInput, determineCritSlip, verifyOutput, displayInput, 
  displayGraph, displayFS, displayNormal, displayShear, 
  writeToFile :: ConceptInstance

readAndStore = cic "readAndStore" ( foldlSent [
  S "Read the", plural input_ `sC` S "shown in the table", 
  namedRef inputDataTable (S "Required Inputs") `sC` S "and store the", plural datum]) 
  "Read-and-Store" funcReqDom

verifyInput = cic "verifyInput" ( foldlSent [
  S "Verify that the", plural inDatum, S "lie within the",
  namedRef (datCon [] []) (plural physicalConstraint)])
  "Verify-Input" funcReqDom

determineCritSlip = cic "determineCritSlip" ( foldlSent [
  S "Determine the", phrase crtSlpSrf, S "for the", phrase input_, 
  phrase slope `sC` S "corresponding to the minimum", phrase fs `sC` 
  S "by using", usingIMs, S "to calculate the", phrase fs, S "for a", 
  phrase slpSrf `S.and_` S "using", refS crtSlpId, S "to find the", 
  phrase slpSrf, S "that minimizes it"]) 
  "Determine-Critical-Slip-Surface" funcReqDom

verifyOutput = cic "verifyOutput" ( foldlSent [
  S "Verify that the", phrase fsMin `S.and_` phrase crtSlpSrf, S "satisfy the",
  plural physicalConstraint, S "shown in", namedRef (propCorSol [] []) (titleize' propOfCorSol)])
  "Verify-Output" funcReqDom

displayInput = cic "displayInput" ( foldlSent [
  S "Display as", phrase output_, phraseNP (the user) :+: S "-supplied",
  plural input_, S "listed in", refS inputsToOutputTable])
  "Display-Input" funcReqDom

displayGraph = cic "displayGraph" ( foldlSent [
  S "Display", phrase crtSlpSrf `S.the_ofThe` short twoD, phrase slope `sC` 
  S "as determined from", refS crtSlpId `sC` S "graphically"]) 
  "Display-Graph" funcReqDom

displayFS = cic "displayFS" ( foldlSent [
  S "Display", phrase value `S.the_ofThe` phrase fs, S "for the", 
  phrase crtSlpSrf `sC` S "as determined from", usingIMs]) 
  "Display-Factor-of-Safety" funcReqDom

displayNormal = cic "displayNormal" ( foldlSent [
  S "Using", usingIMs `sC` S "calculate and graphically display the",
  plural intNormForce]) "Display-Interslice-Normal-Forces" funcReqDom

displayShear = cic "displayShear" ( foldlSent [
  S "Using", usingIMs `sC` S "calculate and graphically display the",
  plural intShrForce]) "Display-Interslice-Shear-Forces" funcReqDom

writeToFile = cic "writeToFile" ( foldlSent [
  S "Provide the option of writing the output result data, as given in", 
  foldlList Comma List (map refS [displayInput, displayGraph, displayFS, 
  displayNormal, displayShear]) `sC` S "to a file"]) "Write-Results-To-File" 
  funcReqDom

usingIMs :: Sentence
usingIMs = foldlList Comma List $ map refS [fctSfty, nrmShrFor, intsliceFs]

------------------
inputDataTable :: LabelledContent
inputDataTable = mkInputPropsTable (dqdWr coords : map dqdWr inputs) readAndStore
  --FIXME: this has to be seperate since coords is a different type

inputsToOutput :: [DefinedQuantityDict]
inputsToOutput = constF : map dqdWr [xMaxExtSlip, xMaxEtrSlip, xMinExtSlip, 
  xMinEtrSlip, yMaxSlip, yMinSlip]

inputsToOutputTable :: LabelledContent
inputsToOutputTable = llcc (makeTabRef "inputsToOutputTable") $
  Table [titleize symbol_, titleize name_] (mkTable [ch, phrase] inputsToOutput)
  (atStart' input_ +:+ S "to be returned as" +:+ phrase output_) True

{-Nonfunctional Requirements-}
nonFuncReqs :: [ConceptInstance]
nonFuncReqs = [correct, understandable, reusable, maintainable]

correct :: ConceptInstance
correct = cic "correct" (foldlSent [
  atStartNP' (output_ `the_ofThePS` code), S "have the",
  plural property, S "described in", refS (propCorSol [] [])
  ]) "Correct" nonFuncReqDom

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

reqRefs :: [Reference]
reqRefs = map ref ([inReq EmptyS] ++ funcReqs ++ nonFuncReqs)
  ++ map ref (funcReqTables ++ [inputsToOutputTable, inputDataTable])