{-# LANGUAGE PostfixOperators #-}
module Drasil.SWHS.TMods (PhaseChange(Liquid), consThermE, latentHtE,
  nwtnCooling, sensHtE, sensHtETemplate, tMods, tModRefs) where

import qualified Data.List.NonEmpty as NE

import Language.Drasil
import Control.Lens ((^.))
import Theory.Drasil (TheoryModel, tm, 
  ModelKinds(OthModel, EquationalModel, EquationalConstraints),
  ConstraintSet, mkConstraintSet)
import Utils.Drasil
import Utils.Drasil.Concepts
import qualified Utils.Drasil.Sentence as S

import Data.Drasil.Concepts.Documentation (system)
import Data.Drasil.Concepts.Math (equation, rate, rOfChng)
import Data.Drasil.Concepts.Physics (mechEnergy)
import Data.Drasil.Concepts.Thermodynamics (heatTrans, lawConsEnergy,
  lawConvCooling, phaseChange, thermalEnergy)

import Data.Drasil.Quantities.Math (gradient)
import Data.Drasil.Quantities.PhysicalProperties (density, mass)
import Data.Drasil.Quantities.Physics (energy, time)
import Data.Drasil.Quantities.Thermodynamics (boilPt, heatCapSpec,
  htFlux, latentHeat, meltPt, sensHeat, temp)

import Drasil.SWHS.Assumptions (assumpHTCC, assumpTEO)
import Drasil.SWHS.Concepts (transient)
import Drasil.SWHS.DataDefs (ddMeltFrac)
import Drasil.SWHS.References (incroperaEtAl2007)
import Drasil.SWHS.Unitals (deltaT, htCapL, htCapS, htCapV, htTransCoeff,
  meltFrac, tau, tempEnv, thFluxVect, volHtGen)

tMods :: [TheoryModel]
tMods = [consThermE, sensHtE, latentHtE, nwtnCooling]

-------------------------
-- Theoretical Model 1 --
-------------------------
consThermE :: TheoryModel
consThermE = tm (EquationalConstraints consThermECS)
  [qw thFluxVect, qw gradient, qw volHtGen,
    qw density, qw heatCapSpec, qw temp, qw time] ([] :: [ConceptChunk])
  [] [toDispExpr consThermERel] [] [dRef consThemESrc] "consThermE" consThermENotes

consThermECS :: ConstraintSet
consThermECS = mkConstraintSet consCC rels
  where consCC = dccWDS "consThermECS"
          (nounPhraseSP "Conservation of thermal energy") (lawConsEnergy ^. defn)
        rels   = NE.fromList [consThermERel]

consThermERel :: Relation
consThermERel = negVec (sy gradient) $. sy thFluxVect `addRe` sy volHtGen $=
  sy density `mulRe` sy heatCapSpec `mulRe` pderiv (sy temp) time

-- the second argument is a 'ShortName'...
consThemESrc :: Reference
consThemESrc = makeURI "consThemESrc"
  "http://www.efunda.com/formulae/heat_transfer/conduction/overview_cond.cfm" $
  shortname' $ S "Fourier Law of Heat Conduction and Heat Equation"

consThermENotes :: [Sentence]
consThermENotes = map foldlSent [
  [S "The above", phrase equation, S "gives the", phrase lawConsEnergy,
   S "for", phrase transient, phrase heatTrans, S "in a given material"],
  [S "For this", phrase equation, S "to apply" `sC` S "other forms" `S.of_`
   phrase energy `sC` S "such as", phrase mechEnergy `sC` S "are assumed",
   S "to be negligible" `S.inThe` phrase system, sParen (refS assumpTEO)]]

-------------------------
-- Theoretical Model 2 --
-------------------------
sensHtE :: TheoryModel
sensHtE = sensHtETemplate AllPhases sensHtEdesc

data PhaseChange = AllPhases
                 | Liquid

sensHtETemplate :: PhaseChange -> Sentence -> TheoryModel
sensHtETemplate pc desc = tm (EquationalModel qd)
  [qw sensHeat, qw htCapS, qw mass,
    qw deltaT, qw meltPt, qw temp, qw htCapL, qw boilPt, qw htCapV] ([] :: [ConceptChunk])
  [qd] [] [] [dRef sensHtESrc] "sensHtE" [desc]
    where
      qd = sensHtEQD pc eqn desc
      eqn = sensHtEEqn pc


sensHtEQD :: PhaseChange -> Expr -> Sentence -> QDefinition
sensHtEQD pc eqn desc = fromEqnSt' "sensHeat" np desc (symbol sensHeat) (sensHeat ^. typ) eqn
  where np = nounPhraseSP ("Sensible heat energy" ++ case pc of
                                                       Liquid -> " (no state change)"
                                                       AllPhases -> "")

sensHtESrc :: Reference
sensHtESrc = makeURI "sensHtESrc"
  "http://en.wikipedia.org/wiki/Sensible_heat" $
  shortname' $ S "Definition of Sensible Heat"

sensHtEEqn :: PhaseChange -> Expr
sensHtEEqn pChange = case pChange of
  Liquid -> liquidFormula
  AllPhases -> incompleteCase [(sy htCapS `mulRe` sy mass `mulRe` sy deltaT,
      sy temp $< sy meltPt), (liquidFormula, sy meltPt $< sy temp $<
      sy boilPt), (sy htCapV `mulRe` sy mass `mulRe`
      sy deltaT, sy boilPt $< sy temp)]
  where
    liquidFormula = sy htCapL `mulRe` sy mass `mulRe` sy deltaT

--When to call with C? When to call with U, S, Sy, etc? Sometimes confusing.

--Figured out why so many were defn and others were term. The unitals
-- were implemented incorrectly.
sensHtEdesc :: Sentence
sensHtEdesc = foldlSent [
  atStart sensHeat :+: S "ing occurs as long as the material does not reach a",
  phrase temp, S "where a", phrase phaseChange, (S "occurs" !.), atStartNP (a_ phaseChange),
  S "occurs if" +:+. (eS (sy temp $= sy boilPt) `S.or_` eS (sy temp $= sy meltPt)),
  S "If this is the case" `sC` S "refer to", refS latentHtE]

--How to have new lines in the description?
--Can't have relation and eqn chunks together since they are called in a list
----You can, you just can't map "Definition" over a list
---- you have to do each separately
--How to have multiple possible equations?
--How to have conditions in the equation section?

-------------------------
-- Theoretical Model 3 --
-------------------------
latentHtE :: TheoryModel
latentHtE = tm (OthModel latentHtERC)
  [qw latentHeat, qw time, qw tau] ([] :: [ConceptChunk])
  [] [toDispExpr latHtEEqn] [] [dRef latHtESrc] "latentHtE" latentHtENotes

latentHtERC :: RelationConcept
latentHtERC = makeRC "latentHtERC"
  (nounPhraseSP "Latent heat energy") (latentHeat ^. defn) latHtEEqn

latHtEEqn :: Relation
latHtEEqn = apply1 latentHeat time $=
  defint (eqSymb tau) (exactDbl 0) (sy time) (deriv (apply1 latentHeat tau) tau)

-- Integrals need dTau at end

latHtESrc :: Reference
latHtESrc = makeURI "latHtESrc" "http://en.wikipedia.org/wiki/Latent_heat" $
  shortname' $ S "Definition of Latent Heat"

latentHtENotes :: [Sentence]
latentHtENotes = map foldlSent [
  [ch latentHeat `S.isThe` S "change" `S.in_` phrase thermalEnergy,
   sParen (phrase latentHeat +:+ phrase energy)],
  [eS latHtEEqn `S.isThe` phrase rOfChng `S.of_` ch latentHeat `S.wrt` 
   phrase time, ch tau],
  [ch time `S.isThe` phrase time, S "elapsed" `sC` S "as long as the",
   phrase phaseChange, S "is not complete"],
  [S "status" `S.the_ofTheC` phrase phaseChange, S "depends on the",
   phrase meltFrac, sParen (S "from" +:+ refS ddMeltFrac)],
  [atStart latentHeat :+: S "ing stops when all material has changed to the new phase"]]

-------------------------
-- Theoretical Model 4 --
-------------------------
nwtnCooling :: TheoryModel
nwtnCooling = tm (OthModel nwtnCoolingRC)
  [qw latentHeat, qw time, qw htTransCoeff, qw deltaT] ([] :: [ConceptChunk])
  [] [toDispExpr nwtnCoolingEqn] [] [dRefInfo incroperaEtAl2007 $ Page [8]]
  "nwtnCooling" nwtnCoolingNotes

nwtnCoolingRC :: RelationConcept
nwtnCoolingRC = makeRC "nwtnCoolingRC" (nounPhraseSP "Newton's law of cooling")
  EmptyS nwtnCoolingEqn -- nwtnCoolingL

nwtnCoolingEqn :: Relation
nwtnCoolingEqn = apply1 htFlux time $= sy htTransCoeff `mulRe` apply1 deltaT time

nwtnCoolingNotes :: [Sentence]
nwtnCoolingNotes = map foldlSent [
  [atStart lawConvCooling +:+. S "describes convective cooling from a surface" +:
   S "The law is stated as", S "the", phrase rate `S.of_` S "heat loss from a body" `S.is`
   S "proportional to the difference in", plural temp, S "between the body and its surroundings"],
  [ch htTransCoeff, S "is assumed to be independent" `S.of_` ch temp,
   sParen (S "from" +:+ refS assumpHTCC)],
  [E (defines (apply1 deltaT time) (apply1 temp time $- apply1 tempEnv time)) `S.isThe`
   S "time-dependant thermal gradient between the environment and the object"]]

-- References --
tModRefs :: [Reference]
tModRefs = map ref tMods ++ map ref [consThemESrc, sensHtESrc, latHtESrc]
