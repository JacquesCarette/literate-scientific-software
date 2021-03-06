module Data.Drasil.Equations.Defining.Physics where

-- We define both some basic equations of physics, and their wrappers as concepts
--
import Language.Drasil
import Utils.Drasil (foldlSent, getTandS)
import qualified Utils.Drasil.Sentence as S (is, of_, the_ofThe)

import qualified Data.Drasil.Quantities.Math as QM (unitVectj)
import qualified Data.Drasil.Quantities.Physics as QP (acceleration, time,
  force, gravitationalAccel, height, weight, velocity, position)
import qualified Data.Drasil.Quantities.PhysicalProperties as QPP (density, 
  mass, specWeight, vol)
import Data.Drasil.Concepts.Documentation (body, constant)

------------------------------------------------------------------------------------------------------
-- The equations

newtonSLEqn, weightEqn, weightDerivAccelEqn, weightDerivNewtonEqn, weightDerivReplaceMassEqn,
  weightDerivSpecWeightEqn,
  hsPressureEqn, speedEqn, velocityEqn, accelerationEqn  :: Relation

newtonSLEqn               = sy QPP.mass `mulRe` sy QP.acceleration

weightEqn                 = sy QPP.vol `mulRe` sy QPP.specWeight
weightDerivNewtonEqn      = sy QP.weight $= mulRe (sy QPP.mass) (sy QP.gravitationalAccel)
weightDerivReplaceMassEqn = sy QP.weight $= mulRe (sy QPP.density) (mulRe (sy QPP.vol) (sy QP.gravitationalAccel))
weightDerivSpecWeightEqn  = sy QP.weight $= mulRe (sy QPP.vol) (sy QPP.specWeight)

weightDerivAccelEqn       = sy QP.acceleration $= vec2D (exactDbl 0) (mulRe (sy QP.gravitationalAccel) (sy QM.unitVectj))

hsPressureEqn             = sy QPP.specWeight `mulRe` sy QP.height

speedEqn                  = norm (sy QP.velocity)
velocityEqn               = deriv (sy QP.position) QP.time
accelerationEqn           = deriv (sy QP.velocity) QP.time

------------------------------------------------------------------------------------------------------
-- The concepts

accelerationQD :: QDefinition
accelerationQD = mkQuantDef QP.acceleration accelerationEqn

velocityQD :: QDefinition
velocityQD = mkQuantDef QP.velocity velocityEqn

newtonSLQD :: QDefinition
newtonSLQD = fromEqn' "force" (nounPhraseSP "Newton's second law of motion")
  newtonSLDesc (eqSymb QP.force) Real newtonSLEqn

newtonSLDesc :: Sentence
newtonSLDesc = foldlSent [S "The net", getTandS QP.force, S "on a",
  phrase body `S.is` S "proportional to", getTandS QP.acceleration `S.the_ofThe`
  phrase body `sC` S "where", ch QPP.mass, S "denotes", phrase QPP.mass `S.the_ofThe`
  phrase body, S "as the", phrase constant `S.of_` S "proportionality"]
