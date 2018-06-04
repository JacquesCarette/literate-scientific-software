{-# LANGUAGE TemplateHaskell, TypeFamilies #-}

module Language.Drasil.Chunk.DefinedQuantity
  ( dqd, dqd', DefinedQuantityDict, dqdWr
  ) where

import Language.Drasil.Classes (HasUID(uid), NamedIdea(term), Idea(getA),
  Definition(defn), ConceptDomain(cdom, DOM), Concept, HasSymbol(symbol),
  HasSpace(typ), IsUnit, HasDerivation(derivations))
import Language.Drasil.Chunk.Concept (ConceptChunk, cw)
import qualified Language.Drasil.Chunk.Quantity as Q

import Language.Drasil.Symbol (Symbol, Stage)
import Language.Drasil.Space (Space)
import Language.Drasil.Unit (UnitDefn, unitWrapper)
import Language.Drasil.Chunk.Attribute.Derivation

import Control.Lens ((^.), makeLenses, view)

-- | DefinedQuantity = Concept + Quantity
data DefinedQuantityDict = DQD { _con :: ConceptChunk
                               , _symb :: Stage -> Symbol
                               , _spa :: Space
                               , _unit' :: Maybe UnitDefn
                               , _deri :: Derivation
                               }
  
makeLenses ''DefinedQuantityDict

instance HasUID        DefinedQuantityDict where uid = con . uid
instance Eq            DefinedQuantityDict where a == b = (a ^. uid) == (b ^. uid)
instance NamedIdea     DefinedQuantityDict where term = con . term
instance Idea          DefinedQuantityDict where getA = getA . view con
instance Definition    DefinedQuantityDict where defn = con . defn
instance ConceptDomain DefinedQuantityDict where
  type DOM DefinedQuantityDict = ConceptChunk
  cdom = con . cdom
instance Concept       DefinedQuantityDict where
instance Q.HasSpace    DefinedQuantityDict where typ = spa
instance HasSymbol     DefinedQuantityDict where symbol = view symb
instance Q.Quantity    DefinedQuantityDict where getUnit = view unit'
instance HasDerivation DefinedQuantityDict where derivations = deri

-- For when the symbol is constant through stages
dqd :: ConceptChunk -> Symbol -> Space -> Maybe UnitDefn -> DefinedQuantityDict
dqd c s sp un = DQD c (\_ -> s) sp un []

-- For when the symbol changes depending on the stage
dqd' :: ConceptChunk -> (Stage -> Symbol) -> Space -> Maybe UnitDefn -> DefinedQuantityDict
dqd' c s sp un = DQD c s sp un []

dqdWr :: (Q.Quantity c, Concept c, Q.HasSpace c, HasSymbol c, DOM c ~ ConceptChunk) => c -> DefinedQuantityDict
dqdWr c = DQD (cw c) (symbol c) (c ^. typ) (Q.getUnit c) []
