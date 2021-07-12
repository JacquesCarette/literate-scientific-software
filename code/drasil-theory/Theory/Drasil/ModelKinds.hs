{-# LANGUAGE TemplateHaskell, Rank2Types, ScopedTypeVariables, PostfixOperators  #-}
module Theory.Drasil.ModelKinds (
    ModelKind(..), ModelKinds(..),
    deModel, equationalConstraints, equationalModel, equationalRealm, othModel,
    deModel', equationalConstraints', equationalModel', equationalRealm', othModel',
    equationalModelU, equationalModelN, equationalRealmU, equationalRealmN,
    setMk, elimMk, lensMk, getEqModQds
  ) where

import Control.Lens (makeLenses, set, lens, to, (^.), Setter', Getter, Lens')
import Data.Maybe (mapMaybe)

import Language.Drasil (NamedIdea(..), NP, QDefinition, HasUID(..),
  RelationConcept, ConceptDomain(..), Definition(..), Idea(..), Display(..), UID)
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
                | EquationalRealm MultiDefn
                | OthModel RelationConcept

makeLenses ''ModelKinds

-- | 'ModelKinds' carrier, used to carry commonly overwritten information from the IMs/TMs/GDs.
data ModelKind = MK {
  _mk     :: ModelKinds,
  _mkUid  :: UID,
  _mkTerm :: NP
}

makeLenses ''ModelKind

-- | Smart constructor for 'DEModel's
deModel :: UID -> NP -> RelationConcept -> ModelKind
deModel u n rc = MK (DEModel rc) u n

-- | Smart constructor for 'DEModel's, deriving UID+Term from the 'RelationConcept'
deModel' :: RelationConcept -> ModelKind
deModel' rc = MK (DEModel rc) (rc ^. uid) (rc ^. term)

-- | Smart constructor for 'EquationalConstraints'
equationalConstraints :: UID -> NP -> ConstraintSet-> ModelKind
equationalConstraints u n qs = MK (EquationalConstraints qs) u n

-- | Smart constructor for 'EquationalConstraints', deriving UID+Term from the 'ConstraintSet'
equationalConstraints' :: ConstraintSet-> ModelKind
equationalConstraints' qs = MK (EquationalConstraints qs) (qs ^. uid) (qs ^. term)

-- | Smart constructor for 'EquationalModel's
equationalModel :: UID -> NP -> QDefinition -> ModelKind
equationalModel u n qd = MK (EquationalModel qd) u n

-- | Smart constructor for 'EquationalModel's, deriving UID+Term from the 'QDefinition'
equationalModel' :: QDefinition -> ModelKind
equationalModel' qd = MK (EquationalModel qd) (qd ^. uid) (qd ^. term)

-- | Smart constructor for 'EquationalModel's, deriving Term from the 'QDefinition'
equationalModelU :: UID -> QDefinition -> ModelKind
equationalModelU u qd = MK (EquationalModel qd) u (qd ^. term)

-- | Smart constructor for 'EquationalModel's, deriving UID from the 'QDefinition'
equationalModelN :: NP -> QDefinition -> ModelKind
equationalModelN n qd = MK (EquationalModel qd) (qd ^. uid) n

-- | Smart constructor for 'EquationalRealm's
equationalRealm :: UID -> NP -> MultiDefn -> ModelKind
equationalRealm u n md = MK (EquationalRealm md) u n

-- | Smart constructor for 'EquationalRealm's
equationalRealm' :: MultiDefn -> ModelKind
equationalRealm' md = MK (EquationalRealm md) (md ^. uid) (md ^. term)

-- | Smart constructor for 'EquationalRealm's
equationalRealmU :: UID -> MultiDefn -> ModelKind
equationalRealmU u md = MK (EquationalRealm md) u (md ^. term)

-- | Smart constructor for 'EquationalRealm's, deriving UID from the 'MultiDefn'
equationalRealmN :: NP -> MultiDefn -> ModelKind
equationalRealmN n md = MK (EquationalRealm md) (md ^. uid) n

-- | Smart constructor for 'OthModel's
othModel :: UID -> NP -> RelationConcept -> ModelKind
othModel u n rc = MK (OthModel rc) u n

-- | Smart constructor for 'OthModel's, deriving UID+Term from the 'RelationConcept'
othModel' :: RelationConcept -> ModelKind
othModel' rc = MK (OthModel rc) (rc ^. uid) (rc ^. term)

-- | Finds the 'UID' of the 'ModelKinds'.
instance HasUID        ModelKinds where uid        = lensMk uid uid uid uid
-- | Finds the term ('NP') of the 'ModelKinds'.
instance NamedIdea     ModelKinds where term       = lensMk term term term term
-- | Finds the idea of the 'ModelKinds'.
instance Idea          ModelKinds where getA       = elimMk (to getA) (to getA) (to getA) (to getA)
-- | Finds the definition of the 'ModelKinds'.
instance Definition    ModelKinds where defn       = lensMk defn defn defn defn
-- | Finds the domain of the 'ModelKinds'.
instance ConceptDomain ModelKinds where cdom       = elimMk (to cdom) (to cdom) (to cdom) (to cdom)
-- | Rewrites the underlying model using DisplayExpr
instance Display       ModelKinds where toDispExpr = elimMk (to toDispExpr) (to toDispExpr) (to toDispExpr) (to toDispExpr)

-- TODO: implement MayHaveUnit for ModelKinds once we've sufficiently removed OthModels & RelationConcepts (else we'd be breaking too much of `stable`)

-- | Finds the 'UID' of the 'ModelKind'.
instance HasUID        ModelKind where uid        = mkUid
-- | Finds the term ('NP') of the 'ModelKind'.
instance NamedIdea     ModelKind where term       = mkTerm
-- | Finds the idea of the 'ModelKind'.
instance Idea          ModelKind where getA       = getA . (^. mk)
-- | Finds the definition of the 'ModelKind'.
instance Definition    ModelKind where defn       = mk . defn
-- | Finds the domain of the 'ModelKind'.
instance ConceptDomain ModelKind where cdom       = cdom . (^. mk)
-- | Rewrites the underlying model using DisplayExpr
instance Display       ModelKind where toDispExpr = toDispExpr . (^. mk)


-- | Retrieve internal data from ModelKinds
elimMk :: Getter RelationConcept a -> Getter ConstraintSet a -> Getter QDefinition a -> Getter MultiDefn a -> ModelKinds -> a
elimMk f _ _ _ (DEModel q)               = q ^. f
elimMk _ f _ _ (EquationalConstraints q) = q ^. f
elimMk _ _ f _ (EquationalModel q)       = q ^. f
elimMk _ _ _ f (EquationalRealm q)       = q ^. f
elimMk f _ _ _ (OthModel q)              = q ^. f

-- | Map into internal representations of ModelKinds
setMk :: ModelKinds -> Setter' RelationConcept a -> Setter' ConstraintSet a -> Setter' QDefinition a -> Setter' MultiDefn a -> a -> ModelKinds
setMk (DEModel q)               f _ _ _ x = DEModel               $ set f x q
setMk (EquationalConstraints q) _ f _ _ x = EquationalConstraints $ set f x q
setMk (EquationalModel q)       _ _ f _ x = EquationalModel       $ set f x q
setMk (EquationalRealm q)       _ _ _ f x = EquationalRealm       $ set f x q
setMk (OthModel q)              f _ _ _ x = OthModel              $ set f x q

-- | Make a 'Lens' for 'ModelKinds'.
lensMk :: forall a. Lens' RelationConcept a -> Lens' ConstraintSet a -> Lens' QDefinition a -> Lens' MultiDefn a -> Lens' ModelKinds a
lensMk lr lcs lq lqd = lens g s
    where g :: ModelKinds -> a
          g mk_ = elimMk lr lcs lq lqd mk_
          s :: ModelKinds -> a -> ModelKinds
          s mk_ x = setMk mk_ lr lcs lq lqd x

-- | Extract a list of 'QDefinition's from a list of 'ModelKinds'.
getEqModQds :: [ModelKind] -> [QDefinition]
getEqModQds = mapMaybe eqMod
  where
    eqMod (MK (EquationalModel f) _ _) = Just f
    eqMod _                            = Nothing
