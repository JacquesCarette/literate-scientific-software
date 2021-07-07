{-# LANGUAGE TemplateHaskell #-}

module Language.Drasil.Chunk.DQDefinition (DQDefinition, fromEqnD, fromEqnD', fromEqnDSt,
  fromEqnDSt', mkDQDefSt, mkDQuantDef, mkDQuantDef') where

import Control.Lens ((^.), makeLenses, view)
import Language.Drasil.Chunk.UnitDefn (unitWrapper, MayHaveUnit(getUnit), UnitDefn)

import Language.Drasil.Classes.Core (HasUID(uid), HasSymbol(symbol))
import Language.Drasil.Classes (NamedIdea(term), Idea(getA),
  IsUnit, Definition(defn), Quantity, HasSpace(typ),
  ConceptDomain(cdom), Display(toDispExpr))
import Language.Drasil.Chunk.Quantity (QuantityDict, mkQuant, mkQuant')

import Language.Drasil.DisplayExpr
import Language.Drasil.NounPhrase.Core (NP)
import Language.Drasil.Space (Space)
import Language.Drasil.Sentence (Sentence(EmptyS))
import Language.Drasil.Stages (Stage)
import Language.Drasil.Symbol (Symbol)
import Language.Drasil.UID (UID)

-- | A DQDefinition is a 'QuantityDict' with a defining expression ('DisplayExpr'), a definition ('Sentence'), and a domain (['UID']).
data DQDefinition = EC
  { _qua   :: QuantityDict
  , _defn' :: Sentence
  , _equat :: DisplayExpr
  , cd     :: [UID]
  }
makeLenses ''DQDefinition

-- | Finds the 'UID' of the 'QuantityDict' used to make the 'DQDefinition'.
instance HasUID        DQDefinition where uid = qua . uid
-- | Finds the term ('NP') of the 'QuantityDict' used to make the 'DQDefinition'.
instance NamedIdea     DQDefinition where term = qua . term
-- | Finds the idea contained in the 'QuantityDict' used to make the 'DQDefinition'.
instance Idea          DQDefinition where getA c = getA $ c ^. qua
-- | Finds the 'Space' of the 'QuantityDict' used to make the 'DQDefinition'.
instance HasSpace      DQDefinition where typ = qua . typ
-- | Finds the 'Symbol' of the 'QuantityDict' used to make the 'DQDefinition'.
instance HasSymbol     DQDefinition where symbol e = symbol (e ^. qua)
-- | Finds the definition of 'DQDefinition'.
instance Definition    DQDefinition where defn = defn'
-- | 'DQDefinition's have a 'Quantity'.
instance Quantity      DQDefinition where

-- TODO: remove definingexpr completely?
-- | Finds the defining expression of 'DQDefinition'.
-- instance DefiningExpr  DQDefinition where defnExpr = equat

-- | Equal if 'UID's are equal.
instance Eq            DQDefinition where a == b = (a ^. uid) == (b ^. uid)
-- | Finds the units of the 'QuantityDict' used to make the 'DQDefinition'.
instance MayHaveUnit   DQDefinition where getUnit = getUnit . view qua
-- | Displays the relation given by the expression in 'DQDefinition'.
instance Display       DQDefinition where toDispExpr q = defines (sy q) (q ^. equat)
-- | Finds the domain of a 'DQDefinition'.
instance ConceptDomain DQDefinition where cdom = cd

-- | Create a 'DQDefinition' with a 'UID', term ('NP'), definition ('Sentence'), 'Symbol',
-- 'Space', unit, and defining expression.
fromEqnD :: (IsUnit u) => String -> NP -> Sentence -> Symbol -> Space -> u -> DisplayExpr -> DQDefinition
fromEqnD nm desc def symb sp un expr =
  EC (mkQuant nm desc symb sp (Just $ unitWrapper un) Nothing) def expr []

-- | Same as 'fromEqn', but has no units.
fromEqnD' :: String -> NP -> Sentence -> Symbol -> Space -> DisplayExpr -> DQDefinition
fromEqnD' nm desc def symb sp expr =
  EC (mkQuant nm desc symb sp Nothing Nothing) def expr []

-- | Same as 'fromEqn', but symbol depends on stage.
fromEqnDSt :: (IsUnit u) => String -> NP -> Sentence -> (Stage -> Symbol) ->
  Space -> u -> DisplayExpr -> DQDefinition
fromEqnDSt nm desc def symb sp un expr =
  EC (mkQuant' nm desc Nothing sp symb (Just $ unitWrapper un)) def expr []

-- | Same as 'fromEqn', but symbol depends on stage and has no units.
fromEqnDSt' :: String -> NP -> Sentence -> (Stage -> Symbol) -> Space -> DisplayExpr ->
  DQDefinition
fromEqnDSt' nm desc def symb sp expr =
  EC (mkQuant' nm desc Nothing sp symb Nothing) def expr []

-- | Wrapper for fromEqnSt and fromEqnSt'
mkDQDefSt :: UID -> NP -> Sentence -> (Stage -> Symbol) -> Space ->
  Maybe UnitDefn -> DisplayExpr -> DQDefinition
mkDQDefSt u n s symb sp (Just ud) e = fromEqnDSt u n s symb sp ud e
mkDQDefSt u n s symb sp Nothing   e = fromEqnDSt' u n s symb sp e

-- | Used to help make 'DQDefinition's when 'UID', term, and 'Symbol' come from the same source.
mkDQuantDef :: (Quantity c, MayHaveUnit c) => c -> DisplayExpr -> DQDefinition
mkDQuantDef c = mkDQDefSt (c ^. uid) (c ^. term) EmptyS (symbol c) (c ^. typ) (getUnit c)

-- | Used to help make 'DQDefinition's when 'UID' and 'Symbol' come from the same source, with the term separate.
mkDQuantDef' :: (Quantity c, MayHaveUnit c) => c -> NP -> DisplayExpr -> DQDefinition
mkDQuantDef' c t = mkDQDefSt (c ^. uid) t EmptyS (symbol c) (c ^. typ) (getUnit c)
