module Drasil.DblPendulum.Requirements where

import Language.Drasil
import Drasil.DocLang (inReq)
import Drasil.DocLang.SRS (datCon, propCorSol)
import Utils.Drasil
import Utils.Drasil.Concepts
import qualified Utils.Drasil.Sentence as S

import Data.Drasil.Concepts.Computation (inValue)
import Data.Drasil.Concepts.Documentation (datumConstraint, funcReqDom,
        output_, value,  nonFuncReqDom, code, environment, propOfCorSol)
--  likelyChg, mg, mis, module_, nonFuncReqDom,
--   requirement, srs, traceyMatrix, unlikelyChg, value, vavPlan)
import Data.Drasil.Concepts.Math (calculation)
import Data.Drasil.Concepts.Software (errMsg)
import Drasil.DblPendulum.IMods (angularDisplacementIM)
import Drasil.DblPendulum.Unitals (lenRod, pendDisplacementAngle)
import Data.Drasil.Quantities.Physics (angularDisplacement)

{--Functional Requirements--}

funcReqs :: [ConceptInstance]
funcReqs = [verifyInptVals, calcAngPos, outputValues]

verifyInptVals, calcAngPos, outputValues :: ConceptInstance

verifyInptVals = cic "verifyInptVals" verifyInptValsDesc "Verify-Input-Values" funcReqDom
calcAngPos  = cic "calcAngPos"   calcAngPosDesc   "Calculate-Angular-Position-Of-Mass" funcReqDom
outputValues = cic "outputValues" outputValuesDesc "Output-Values" funcReqDom

verifyInptValsDesc, calcAngPosDesc, outputValuesDesc :: Sentence

verifyInptValsDesc = foldlSent [S "Check the entered", plural inValue,
  S "to ensure that they do not exceed the" +:+. namedRef (datCon ([]::[Contents]) ([]::[Section])) (plural datumConstraint),
  S "If any of the", plural inValue, S "are out of bounds" `sC`
  S "an", phrase errMsg, S "is displayed" `S.andThe` plural calculation, S "stop"]

calcAngPosDesc = foldlSent [S "Calculate the following" +: plural value,
  foldlList Comma List [
    ch angularDisplacement +:+ sParen (S "from" +:+ refS angularDisplacementIM),  
    ch pendDisplacementAngle   +:+ sParen (S "from" +:+ refS angularDisplacementIM)
  ]]
outputValuesDesc = foldlSent [atStart output_, ch lenRod,
  sParen (S "from" +:+ refS angularDisplacementIM) `S.and_` ch lenRod,
  sParen (S "from" +:+ refS angularDisplacementIM)]


{--Nonfunctional Requirements--}

nonFuncReqs :: [ConceptInstance]
nonFuncReqs = [correct, portable]


correct :: ConceptInstance
correct = cic "correct" (foldlSent [
 atStartNP' (output_ `the_ofThePS` code), S "have the",
 namedRef (propCorSol [] []) (plural propOfCorSol)
 ]) "Correct" nonFuncReqDom

portable :: ConceptInstance
portable = cic "portable" (foldlSent [
  atStartNP (the code), S "is able to be run in different", plural environment])
  "Portable" nonFuncReqDom
 
-- References --
reqRefs :: [Reference]
reqRefs = map ref ([inReq EmptyS] ++ funcReqs ++ nonFuncReqs)