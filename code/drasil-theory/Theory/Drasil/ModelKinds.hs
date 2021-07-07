{-# LANGUAGE TemplateHaskell, Rank2Types, ScopedTypeVariables, PostfixOperators  #-}
module Theory.Drasil.ModelKinds (ModelKinds(..),
  setMk, elimMk, lensMk, getEqModQds) where

import Control.Lens (makeLenses, set, lens, to, (^.), Setter', Getter, Lens')
import Data.Maybe (mapMaybe)

import Language.Drasil (ConceptDomain(..), Definition(..),
  Idea(..), NamedIdea(..), HasUID(..), Display (toDispExpr),
  DQDefinition, QDefinition, RelationConcept)
import Theory.Drasil.ConstraintSet (ConstraintSet)
import Theory.Drasil.MultiDefn (MultiDefn)

-- | Models can be of different kinds: 
--
--     * 'DEModel's represent differential equations as 'RelationConcept's
--     * 'EquationalConstraint's represent invariants that will hold in a system of equations.
--     * 'EquationalModel's represent quantities that are calculated via a single definition/'QDefinition'.
--     * 'EquationalRealm's represent MultiDefns; quantities that may be calculated using any one of many 'DefiningExpr's (e.g., 'x = A = ... = Z')
--     * 'OthModel's are placeholders for models. No new 'OthModel's should be created, they should be using one of the other kinds.
data ModelKinds = DEModel RelationConcept
                | EquationalConstraints ConstraintSet
                | EquationalModel QDefinition
                | DEqModel DQDefinition
                | EquationalRealm MultiDefn
                | OthModel RelationConcept

makeLenses ''ModelKinds

-- FIXME: The repetition is starting to get bad.

-- | Finds the 'UID' of the 'ModelKinds'.
instance HasUID        ModelKinds where uid        = lensMk uid uid uid uid uid
-- | Finds the term ('NP') of the 'ModelKinds'.
instance NamedIdea     ModelKinds where term       = lensMk term term term term term
-- | Finds the idea of the 'ModelKinds'.
instance Idea          ModelKinds where getA       = elimMk (to getA) (to getA) (to getA) (to getA) (to getA)
-- | Finds the definition of the 'ModelKinds'.
instance Definition    ModelKinds where defn       = lensMk defn defn defn defn defn
-- | Finds the domain of the 'ModelKinds'.
instance ConceptDomain ModelKinds where cdom       = elimMk (to cdom) (to cdom) (to cdom) (to cdom) (to cdom)
-- | Displays 'ModelKind' in DisplayExpr
instance Display       ModelKinds where toDispExpr = elimMk (to toDispExpr) (to toDispExpr) (to toDispExpr) (to toDispExpr) (to toDispExpr)

-- TODO: implement MayHaveUnit for ModelKinds once we've sufficiently removed OthModels & RelationConcepts (else we'd be breaking too much of `stable`)

-- | Retrieve internal data from ModelKinds
elimMk :: Getter RelationConcept a -> Getter ConstraintSet a -> Getter QDefinition a -> Getter DQDefinition a -> Getter MultiDefn a -> ModelKinds -> a
elimMk f _ _ _ _ (DEModel q)               = q ^. f
elimMk _ f _ _ _ (EquationalConstraints q) = q ^. f
elimMk _ _ f _ _ (EquationalModel q)       = q ^. f
elimMk _ _ _ f _ (DEqModel q)              = q ^. f
elimMk _ _ _ _ f (EquationalRealm q)       = q ^. f
elimMk f _ _ _ _ (OthModel q)              = q ^. f

-- | Map into internal representations of ModelKinds
setMk :: ModelKinds -> Setter' RelationConcept a -> Setter' ConstraintSet a -> Setter' QDefinition a -> Setter' DQDefinition a -> Setter' MultiDefn a -> a -> ModelKinds
setMk (DEModel q)               f _ _ _ _ x = DEModel               $ set f x q
setMk (EquationalConstraints q) _ f _ _ _ x = EquationalConstraints $ set f x q
setMk (EquationalModel q)       _ _ f _ _ x = EquationalModel       $ set f x q
setMk (DEqModel q)              _ _ _ f _ x = DEqModel              $ set f x q
setMk (EquationalRealm q)       _ _ _ _ f x = EquationalRealm       $ set f x q
setMk (OthModel q)              f _ _ _ _ x = OthModel              $ set f x q

-- | Make a 'Lens' for 'ModelKinds'.
lensMk :: forall a. Lens' RelationConcept a -> Lens' ConstraintSet a -> Lens' QDefinition a -> Lens' DQDefinition a -> Lens' MultiDefn a -> Lens' ModelKinds a
lensMk lr lcs lq ldq lqd = lens g s
    where g :: ModelKinds -> a
          g mk = elimMk lr lcs lq ldq lqd mk
          s :: ModelKinds -> a -> ModelKinds
          s mk_ x = setMk mk_ lr lcs lq ldq lqd x

-- | Extract a list of 'QDefinition's from a list of 'ModelKinds'.
getEqModQds :: [ModelKinds] -> [QDefinition]
getEqModQds = mapMaybe isEqMod
  where
    isEqMod (EquationalModel f) = Just f
    isEqMod _                   = Nothing
