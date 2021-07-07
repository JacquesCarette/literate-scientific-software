module Data.Drasil.Equations.Defining.Physics where

-- We define both some basic equations of physics, and their wrappers as concepts
--
import Language.Drasil
import qualified Language.Drasil.DisplayExpr as DE
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
  weightDerivSpecWeightEqn, hsPressureEqn, speedEqn :: Expr

velocityEqn, accelerationEqn :: DisplayExpr

newtonSLEqn               = sy QPP.mass `mulRe` sy QP.acceleration

weightEqn                 = sy QPP.vol `mulRe` sy QPP.specWeight
weightDerivNewtonEqn      = sy QP.weight $= sy QPP.mass `mulRe` sy QP.gravitationalAccel
weightDerivReplaceMassEqn = sy QP.weight $= sy QPP.density `mulRe` sy QPP.vol `mulRe` sy QP.gravitationalAccel
weightDerivSpecWeightEqn  = sy QP.weight $= sy QPP.vol `mulRe` sy QPP.specWeight

-- TODO: This + the above equations should be DisplayExprs, using "defines"
weightDerivAccelEqn       = sy QP.acceleration $= vec2D (exactDbl 0) (sy QP.gravitationalAccel `mulRe` sy QM.unitVectj)

hsPressureEqn             = sy QPP.specWeight `mulRe` sy QP.height

speedEqn                  = norm (sy QP.velocity)
velocityEqn               = DE.deriv (DE.sy QP.position) QP.time
accelerationEqn           = DE.deriv (DE.sy QP.velocity) QP.time

------------------------------------------------------------------------------------------------------
-- The concepts

-- TODO: accelerationEqn is needed to be a DisplayExpr, but mkQuantDef/QDefinitions require Exprs! Looks like we need a new kind of QDefinition.
accelerationDQD :: DQDefinition
accelerationDQD = mkDQuantDef QP.acceleration accelerationEqn

velocityDQD :: DQDefinition
velocityDQD = mkDQuantDef QP.velocity velocityEqn

newtonSLQD :: QDefinition
newtonSLQD = fromEqn' "force" (nounPhraseSP "Newton's second law of motion")
  newtonSLDesc (eqSymb QP.force) Real newtonSLEqn

newtonSLDesc :: Sentence
newtonSLDesc = foldlSent [S "The net", getTandS QP.force, S "on a",
  phrase body `S.is` S "proportional to", getTandS QP.acceleration `S.the_ofThe`
  phrase body `sC` S "where", ch QPP.mass, S "denotes", phrase QPP.mass `S.the_ofThe`
  phrase body, S "as the", phrase constant `S.of_` S "proportionality"]
