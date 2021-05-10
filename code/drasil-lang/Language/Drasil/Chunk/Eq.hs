{-# LANGUAGE TemplateHaskell #-}
module Language.Drasil.Chunk.Eq (QDefinition, fromEqn, fromEqn', fromEqnSt,
  fromEqnSt', equat, mkQuantDef, mkQuantDef', ec) where

import Control.Lens ((^.), makeLenses, view)
import Language.Drasil.Chunk.UnitDefn (unitWrapper, MayHaveUnit(getUnit))

import Language.Drasil.Classes.Core (HasUID(uid), HasSymbol(symbol))
import Language.Drasil.Classes (NamedIdea(term), Idea(getA),
  IsUnit, DefiningExpr(defnExpr), Definition(defn), Quantity, HasSpace(typ), ExprRelat(relat), ConceptDomain(cdom))
import Language.Drasil.Chunk.Quantity (QuantityDict, mkQuant, mkQuant', qw)

import Language.Drasil.Expr (Expr, ($=))
import Language.Drasil.Expr.Math (sy)
import Language.Drasil.NounPhrase (NP)
import Language.Drasil.Space (Space)
import Language.Drasil.Sentence (Sentence(EmptyS))
import Language.Drasil.Stages (Stage)
import Language.Drasil.Symbol (Symbol)
import Language.Drasil.UID (UID)

-- | A QDefinition is a 'Quantity' with a defining expression, and a definition
data QDefinition = EC
  { _qua :: QuantityDict
  , _defn' :: Sentence
  , _equat :: Expr
  , cd :: [UID]
  }
makeLenses ''QDefinition

-- this works because UnitalChunk is a Chunk
instance HasUID        QDefinition where uid = qua . uid
instance NamedIdea     QDefinition where term = qua . term
instance Idea          QDefinition where getA c = getA $ c ^. qua
instance HasSpace      QDefinition where typ = qua . typ
instance HasSymbol     QDefinition where symbol e = symbol (e ^. qua)
instance Definition    QDefinition where defn = defn'
instance Quantity      QDefinition where
instance DefiningExpr  QDefinition where defnExpr = equat
instance Eq            QDefinition where a == b = (a ^. uid) == (b ^. uid)
instance MayHaveUnit   QDefinition where getUnit = getUnit . view qua
instance ExprRelat     QDefinition where relat z = sy z $= z ^. equat
instance ConceptDomain QDefinition where cdom = cd

-- | Create a 'QDefinition' with a uid, noun phrase (term), definition, symbol,
-- unit, and defining equation.
fromEqn :: (IsUnit u) => String -> NP -> Sentence -> Symbol -> Space -> u -> Expr -> QDefinition
fromEqn nm desc def symb sp un expr =
  EC (mkQuant nm desc symb sp (Just $ unitWrapper un) Nothing) def expr []

-- | Same as fromEqn, but has no units.
fromEqn' :: String -> NP -> Sentence -> Symbol -> Space -> Expr -> QDefinition
fromEqn' nm desc def symb sp expr =
  EC (mkQuant nm desc symb sp Nothing Nothing) def expr []

-- | Same as fromEqn, but symbol depends on stage
fromEqnSt :: (IsUnit u) => String -> NP -> Sentence -> (Stage -> Symbol) ->
  Space -> u -> Expr -> QDefinition
fromEqnSt nm desc def symb sp un expr =
  EC (mkQuant' nm desc Nothing sp symb (Just $ unitWrapper un)) def expr []

-- | Same as fromEqn', but symbol depends on stage
fromEqnSt' :: String -> NP -> Sentence -> (Stage -> Symbol) -> Space -> Expr ->
  QDefinition
fromEqnSt' nm desc def symb sp expr =
  EC (mkQuant' nm desc Nothing sp symb Nothing) def expr []

-- Used to help make Qdefinitions when uid, term, and symbol come from the same source
mkQuantDef :: (Quantity c, MayHaveUnit c) => c -> Expr -> QDefinition
mkQuantDef c e = datadef $ getUnit c
  where datadef (Just a) = fromEqnSt  (c ^. uid) (c ^. term) EmptyS (symbol c) (c^.typ) a e
        datadef Nothing  = fromEqnSt' (c ^. uid) (c ^. term) EmptyS (symbol c) (c^.typ) e

-- Used to help make Qdefinitions when uid and symbol come from the same source, with the term is separate
mkQuantDef' :: (Quantity c, MayHaveUnit c) => c -> NP -> Expr -> QDefinition
mkQuantDef' c t e = datadef $ getUnit c
  where datadef (Just a) = fromEqnSt  (c ^. uid) t EmptyS (symbol c) (c^.typ) a e
        datadef Nothing  = fromEqnSt' (c ^. uid) t EmptyS (symbol c) (c^.typ) e

-- | Smart constructor for QDefinitions. Requires a quantity and its defining 
-- equation. HACK - makes the definition EmptyS !!! FIXME
ec :: (Quantity c, MayHaveUnit c) => c -> Expr -> QDefinition
ec c eqn = EC (qw c) EmptyS eqn []
