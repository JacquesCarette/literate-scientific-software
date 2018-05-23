{-# Language TemplateHaskell, TypeFamilies #-}
module Language.Drasil.Chunk.GenDefn
  ( GenDefn, gd, gdUnit
  ) where

import Language.Drasil.Classes (HasUID(uid), NamedIdea(term), Idea(getA),
  Definition(defn), ConceptDomain(cdom,DOM), Concept, IsUnit, 
  HasAttributes(attributes), ExprRelat(relat), HasShortName(shortname))
import Language.Drasil.Chunk.Attribute.Core (Attributes)
import Language.Drasil.Chunk.Concept (ConceptChunk)
import Language.Drasil.Chunk.Relation (RelationConcept)
import Language.Drasil.Unit (unitWrapper, UnitDefn)

import Control.Lens (makeLenses)

-- | A GenDefn is a RelationConcept that may have units
data GenDefn = GD { _relC :: RelationConcept
                  , gdUnit :: Maybe UnitDefn
                  , _attribs :: Attributes
                  }
makeLenses ''GenDefn

instance HasUID        GenDefn where uid = relC . uid
instance NamedIdea     GenDefn where term = relC . term
instance Idea          GenDefn where getA (GD a _ _) = getA a
instance Concept       GenDefn where
instance Definition    GenDefn where defn = relC . defn
instance ConceptDomain GenDefn where
  type DOM GenDefn = ConceptChunk
  cdom = relC . cdom
instance ExprRelat     GenDefn where relat = relC . relat
instance HasAttributes GenDefn where attributes = attribs
instance HasShortName  GenDefn where
  shortname _ = error "No explicit name given for general definition -- build a custom Ref"

gd :: (IsUnit u, DOM u ~ ConceptChunk) => RelationConcept -> Maybe u -> Attributes -> GenDefn
gd r (Just u) ats = GD r (Just (unitWrapper u)) ats
gd r Nothing ats = GD r Nothing ats
