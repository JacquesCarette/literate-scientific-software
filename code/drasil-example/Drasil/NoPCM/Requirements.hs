module Drasil.NoPCM.Requirements (funcReqs, inputInitValsTable, reqRefs) where

import Language.Drasil
import Drasil.DocLang (mkInputPropsTable)
import Utils.Drasil
import Utils.Drasil.Concepts

import Data.Drasil.Concepts.Documentation (funcReqDom, input_, value)

import Data.Drasil.Quantities.PhysicalProperties (mass)

import Drasil.SWHS.DataDefs (waterMass, tankVolume, balanceDecayRate)
import Drasil.SWHS.Requirements (calcTempWtrOverTime, calcChgHeatEnergyWtrOverTime,
  checkWithPhysConsts, findMassConstruct, inReqDesc, oIDQConstruct)

import Drasil.NoPCM.DataDefs (waterVolume)
import Drasil.NoPCM.IMods (eBalanceOnWtr)
import Drasil.NoPCM.Unitals (inputs)

--------------------------
--Section 5 : REQUIREMENTS
--------------------------

---------------------------------------
--Section 5.1 : FUNCTIONAL REQUIREMENTS
---------------------------------------

--
inputInitVals :: ConceptInstance
inputInitVals = cic "inputInitVals" (foldlSent [
  titleize input_, S "the following", plural value, S "described in the table for",
  namedRef inputInitValsTable (S "Required Inputs")`sC` S "which define", inReqDesc])
  "Input-Initial-Values" funcReqDom

--
findMass :: ConceptInstance
findMass = findMassConstruct inputInitVals (phrase mass) [eBalanceOnWtr]
            [waterMass, waterVolume, tankVolume]

--
oIDQVals :: [Sentence]
oIDQVals = map foldlSent_ [
  [pluralNP (the value), fromSource inputInitVals],
  [phraseNP (the mass), fromSource findMass],
  [ch balanceDecayRate, fromSource balanceDecayRate]
  ]

inputInitValsTable :: LabelledContent
inputInitValsTable = mkInputPropsTable inputs inputInitVals

funcReqs :: [ConceptInstance]
funcReqs = [inputInitVals, findMass, checkWithPhysConsts,
        oIDQConstruct oIDQVals, calcTempWtrOverTime, calcChgHeatEnergyWtrOverTime]

-------------------------------------------
--Section 5.2 : NON-FUNCTIONAL REQUIREMENTS
-------------------------------------------

--imports from SWHS

--References--
reqRefs :: [Reference]
reqRefs = ref inputInitValsTable : map ref funcReqs
