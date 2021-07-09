{-#LANGUAGE PostfixOperators#-}
module Drasil.PDController.Requirements where

import Data.Drasil.Concepts.Documentation (funcReqDom, nonFuncReqDom, datumConstraint)

import Drasil.DocLang (inReq)
import Drasil.DocLang.SRS (datCon)

import Drasil.PDController.Concepts
import Drasil.PDController.IModel

import Language.Drasil
import Utils.Drasil

funcReqs :: [ConceptInstance]
funcReqs = [verifyInputs, calculateValues, outputValues]

verifyInputs, calculateValues, outputValues :: ConceptInstance
verifyInputs
  = cic "verifyInputs" verifyInputsDesc "Verify-Input-Values" funcReqDom
calculateValues
  = cic "calculateValues" calculateValuesDesc "Calculate-Values" funcReqDom
outputValues = cic "outputValues" outputValuesDesc "Output-Values" funcReqDom

verifyInputsDesc, calculateValuesDesc, outputValuesDesc :: Sentence

verifyInputsDesc
  = foldlSent_
      [S "Ensure that the input values are within the",
         S "limits specified in the"
         +:+. namedRef (datCon [] []) (plural datumConstraint)]

calculateValuesDesc
  = foldlSent
      [S "Calculate the", phrase processVariable, fromSource imPD,
         S "over the simulation time"]

outputValuesDesc
  = foldlSent
      [S "Output the", phrase processVariable, fromSource imPD,
         S "over the simulation time"]

-----------------------------------------------------------------------------

nonfuncReqs :: [ConceptInstance]
nonfuncReqs = [portability, security, maintainability, verifiability]

portability :: ConceptInstance
portability
  = cic "portability"
      (S "The code shall be portable to multiple Operating Systems" !.)
      "Portable"
      nonFuncReqDom

security :: ConceptInstance
security
  = cic "security"
      (foldlSent
         [S "The code shall be immune to common security problems such as memory",
            S "leaks, divide by zero errors, and the square root of negative numbers"])
      "Secure"
      nonFuncReqDom

maintainability :: ConceptInstance
maintainability
  = cic "maintainability"
      (foldlSent
         [S "The dependencies among the instance models, requirements,",
            S "likely changes, assumptions and all other relevant sections of",
            S "this document shall be traceable to each other in the trace matrix"])
      "Maintainable"
      nonFuncReqDom

verifiability :: ConceptInstance
verifiability
  = cic "verifiability"
      (S "The code shall be verifiable against a Verification and Validation plan" !.)
      "Verifiable"
      nonFuncReqDom

-- References --
reqRefs :: [Reference]
reqRefs = ref (datCon ([]::[Contents]) ([]::[Section])): 
  map ref ([inReq EmptyS] ++ funcReqs ++ nonfuncReqs)

