{-# LANGUAGE PostfixOperators #-}
module Drasil.PDController.GenDefs where

import Data.Drasil.Quantities.PhysicalProperties (mass)
import Data.Drasil.Concepts.Math (equation)
import Drasil.PDController.Assumptions
import Drasil.PDController.Concepts
import Drasil.PDController.References
import Drasil.PDController.TModel
import Language.Drasil
import Theory.Drasil (GenDefn, gd, ModelKinds (OthModel))
import Utils.Drasil
import Utils.Drasil.Concepts
import qualified Utils.Drasil.Sentence as S
import Data.Drasil.Citations ( pidWiki )
import Drasil.PDController.Unitals

genDefns :: [GenDefn]
genDefns = [gdPowerPlant]

----------------------------------------------

gdPowerPlant :: GenDefn
gdPowerPlant
  = gd (OthModel gdPowerPlantRC) (Nothing :: Maybe UnitDefn) Nothing
      [dRef pidWiki, dRef abbasi2015]
      "gdPowerPlant"
      [gdPowerPlantNote]

gdPowerPlantRC :: RelationConcept
gdPowerPlantRC
  = makeRC "gdPowerPlantRC"
      (ccTransferFxn `the_ofThe` powerPlant)
      EmptyS
      gdPowerPlantEqn

gdPowerPlantEqn :: Expr
gdPowerPlantEqn
  = recip_ (square (sy qdFreqDomain) `addRe` sy qdFreqDomain `addRe` exactDbl 20)

gdPowerPlantNote :: Sentence
gdPowerPlantNote
  = foldlSent
      [atStartNP (ccTransferFxn `the_ofThe` secondOrderSystem),
         fromSource tmSOSystem,
         S "is reduced to this equation by substituting the",
         phrase mass, S "(m) to 1 Kg", fromSource aMass `sC`
         phraseNP (the ccDampingCoeff), sParen (P symDampingCoeff),
         S "to 1", fromSource aDampingCoeff `sC` EmptyS
         `S.andThe` phrase ccStiffCoeff, sParen (P symStifnessCoeff),
         S "to 20" +:+. fromSource aStiffnessCoeff,
       atStartNP (the equation), S "is converted to the", phrase ccFrequencyDomain,
         S "by applying the", atStart ccLaplaceTransform +:+. fromSource tmLaplace,
       S "Additionally, there are no external disturbances to the power plant",
         fromSource aExtDisturb]

-- References --
genDefRefs :: [Reference]
genDefRefs = map ref genDefns
