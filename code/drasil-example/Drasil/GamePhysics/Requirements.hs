{-# LANGUAGE PostfixOperators #-}
module Drasil.GamePhysics.Requirements (funcReqs, nonfuncReqs, reqRefs) where

import Language.Drasil hiding (organization)
import Utils.Drasil
import Utils.Drasil.Concepts
import qualified Utils.Drasil.Sentence as S

import Drasil.DocLang (inReq)
import qualified Drasil.DocLang.SRS as SRS (solCharSpec)
import Data.Drasil.Concepts.Documentation as Doc (body, funcReqDom, input_, 
  nonFuncReqDom, output_, physicalConstraint, physicalSim, property, solutionCharSpec)

import qualified Data.Drasil.Concepts.Physics as CP (collision, elasticity, 
  friction, rigidBody, space)
import qualified Data.Drasil.Concepts.Math as CM (surface)
import qualified Data.Drasil.Quantities.Math as QM (orientation)
import qualified Data.Drasil.Quantities.PhysicalProperties as QPP (mass)
import qualified Data.Drasil.Quantities.Physics as QP (angularVelocity, force, 
  position, time, velocity)

import Drasil.GamePhysics.Concepts (twoD)

------------------------------
-- SECTION 5 : REQUIREMENTS --
------------------------------

-----------------------------------
-- 5.1 : Functional Requirements --
-----------------------------------

-- Currently need separate chunks for plurals like rigid bodies,
-- velocities, etc.
funcReqs :: [ConceptInstance]
funcReqs = [simSpace, inputInitialConds, inputSurfaceProps,
  verifyPhysCons, calcTransOverTime, calcRotOverTime, deterColls, deterCollRespOverTime]

simSpaceDesc, inputInitialCondsDesc, 
  inputSurfacePropsDesc, verifyPhysConsDesc,
  calcTransOverTimeDesc, calcRotOverTimeDesc,
  deterCollsDesc, deterCollRespOverTimeDesc :: Sentence

-- | template for requirements
requirementTemplate :: Sentence -> Sentence -> Sentence -> Sentence -> Sentence
requirementTemplate a b x z = foldlSent [S "Determine the", a `S.and_` b, 
  S "over a period of", phrase QP.time, S "of the", x, z]

-- | with added constraint
requirementS :: (NamedIdea a, NamedIdea b) => a -> b -> Sentence -> Sentence
requirementS a b = requirementTemplate (plural a) (plural b) (getAcc twoD
  +:+ plural CP.rigidBody)

-- | without added constraint
requirementS' :: (NamedIdea a, NamedIdea b) => a -> b -> Sentence
requirementS' a b = requirementS a b EmptyS 

-- some requirements look like they could be parametrized
simSpaceDesc = foldlSent [S "Create a", phrase CP.space, S "for all of the",
  pluralNP (CP.rigidBody `inThePS` physicalSim), 
  S "to interact in"]

inputInitialCondsDesc = foldlSent [S "Input the initial", foldlList Comma List
  [plural QPP.mass, plural QP.velocity, plural QM.orientation,
  plural QP.angularVelocity `S.of_` EmptyS, plural QP.force +:+ S "applied on"],
  plural CP.rigidBody]

inputSurfacePropsDesc = foldlSent [S "Input", pluralNP (combineNINI CM.surface
  property) `S.the_ofThe` plural body, S "such as", phrase CP.friction `S.or_`
  phrase CP.elasticity]

verifyPhysConsDesc = foldlSent [S "Verify that the", plural input_,
  S "satisfy the required", plural physicalConstraint, S "from the", 
  namedRef (SRS.solCharSpec [] []) (phrase solutionCharSpec)]

calcTransOverTimeDesc = requirementS QP.position QP.velocity 
  (S "acted upon by a" +:+ phrase QP.force)

calcRotOverTimeDesc = requirementS' QM.orientation QP.angularVelocity

deterCollsDesc = foldlSent [S "Determine if any of the", 
  pluralNP (CP.rigidBody `inThePS` CP.space), 
  S "have collided"]

deterCollRespOverTimeDesc = requirementS QP.position QP.velocity 
  (S "that have undergone a" +:+ phrase CP.collision)

simSpace, inputInitialConds, inputSurfaceProps, verifyPhysCons, calcTransOverTime,
  calcRotOverTime, deterColls, deterCollRespOverTime :: ConceptInstance

simSpace              = cic "simSpace"              simSpaceDesc              "Simulation-Space"                       funcReqDom
inputInitialConds     = cic "inputInitialConds"     inputInitialCondsDesc     "Input-Initial-Conditions"               funcReqDom
inputSurfaceProps     = cic "inputSurfaceProps"     inputSurfacePropsDesc     "Input-Surface-Properties"               funcReqDom
verifyPhysCons        = cic "verifyPhysCons"        verifyPhysConsDesc        "Verify-Physical_Constraints"            funcReqDom
calcTransOverTime     = cic "calcTransOverTime"     calcTransOverTimeDesc     "Calculate-Translation-Over-Time"        funcReqDom
calcRotOverTime       = cic "calcRotOverTime"       calcRotOverTimeDesc       "Calculate-Rotation-Over-Time"           funcReqDom
deterColls            = cic "deterColls"            deterCollsDesc            "Determine-Collisions"                   funcReqDom
deterCollRespOverTime = cic "deterCollRespOverTime" deterCollRespOverTimeDesc "Determine-Collision-Response-Over-Time" funcReqDom

--------------------------------------
-- 5.2 : Nonfunctional Requirements --
--------------------------------------

nonfuncReqs :: [ConceptInstance] 
nonfuncReqs = [performance, correctness, usability, understandability, maintainability]

performance :: ConceptInstance
performance = cic "performance" (foldlSent [
  S "The execution time for collision detection and collision resolution shall be", 
  S "comparable to an existing 2D physics library on the market (e.g. Pymunk)"
  ]) "Performance" nonFuncReqDom

correctness :: ConceptInstance
correctness = cic "correctness" (foldlSent [
  atStartNP (the output_) `S.of_` S "simulation results shall be compared to", 
  S "an existing implementation like Pymunk (please refer to:", 
  S "http://www.pymunk.org/en/latest/)"
  ]) "Correctness" nonFuncReqDom
 
usability :: ConceptInstance
usability = cic "usability" (foldlSent [
  S "Software shall be easy to learn" `S.and_` S "use. Usability shall be measured by", 
  S "how long it takes a user to learn how to use the library to create a small program", 
  S "to simulate the movement" `S.of_` S "2 bodies over time in space. Creating a program", 
  S "should take no less than 30 to 60 minutes for an intermediate to experienced programmer"
  ]) "Usability" nonFuncReqDom

understandability :: ConceptInstance
understandability = cic "understandability" (foldlSent [
  (S "Users" `S.of_` S "Tamias2D shall be able to learn the software with ease" !.), 
  (S "Users shall be able to easily create a small program using the library" !.), 
  S "Creating a small program to simulate the movement of 2 bodies" `S.in_` 
  S "space should take no less that 60 minutes"
  ]) "Understandability" nonFuncReqDom

maintainability :: ConceptInstance
maintainability = cic "maintainability" (foldlSent [
  S "development time for any" `S.the_ofTheC` S "likely changes should not exceed", 
  addPercent (10 :: Integer), S "percent of the original development time"
  ]) "Maintainability" nonFuncReqDom

-- References --
reqRefs :: [Reference]
reqRefs = map ref ([inReq EmptyS] ++ funcReqs ++ nonfuncReqs)
