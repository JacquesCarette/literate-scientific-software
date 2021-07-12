{-# Language TypeFamilies, ConstraintKinds #-}
-- | Defining all the classes which represent knowledge-about-knowledge
module Language.Drasil.Classes (
  -- the classes
    NamedIdea(term)
  , HasSpace(typ)
  , HasUnitSymbol(usymb)
  , HasReference(getReferences)
  , HasDecRef(getDecRefs)
  , HasReasVal(reasVal)
  , HasDerivation(derivations)
  , HasAdditionalNotes(getNotes)
  , Idea(getA)
  , Definition(defn)
  , ConceptDomain(cdom)
  , Constrained(constraints)
  , CommonIdea(abrv)
  , DefiningExpr(defnExpr)
  , Display(toDispExpr)
  , Quantity
  , HasUncertainty(unc)
  , Concept
  , Callable
  , IsArgumentName

  -- the unsorted rest
  , IsUnit(udefn, getUnits)
  , UnitEq(uniteq)
  ) where

-- some classes are so 'core' that they are defined elswhere
-- also helps with cycles...
import Language.Drasil.Classes.Core

import Language.Drasil.Constraint (ConstraintE)
import Language.Drasil.Derivation (Derivation)
import Language.Drasil.UnitLang (UDefn, USymb)
import Language.Drasil.DisplayClasses (Display(toDispExpr))
import Language.Drasil.Expr (Expr)
import Language.Drasil.NounPhrase.Core (NP)
import Language.Drasil.Reference (Reference)
import Language.Drasil.DecoratedReference(DecRef)
import Language.Drasil.Space (Space)
import Language.Drasil.Sentence (Sentence)
import Language.Drasil.UID (UID)
import Language.Drasil.Uncertainty.Core (Uncertainty)

import Control.Lens (Lens')

-- | A NamedIdea is a 'term' that we've identified (has a 'UID') as 
-- being worthy of naming.
class HasUID c => NamedIdea c where
  -- | Lens to the term (a noun phrase).
  term :: Lens' c NP

-- | An 'Idea' is the combination of a 'NamedIdea' and a 'CommonIdea'.
-- In other words, it /may/ have an acronym/abbreviation.
class NamedIdea c => Idea c where
  -- | Gets the acronym/abbreviation.
  getA :: c -> Maybe String
  --Get Abbreviation/Acronym? These might need to be separated 
  --depending on contexts, but for now I don't see a problem with it.

-- | Defines a chunk.
class Definition c where
  -- | Provides (a 'Lens' to) the definition for a chunk.
  defn :: Lens' c Sentence

-- Temporary hack to avoid loss of information
-- | Records any additional notes needed to avoid losing information
class HasAdditionalNotes c where
  -- | Provides a 'Lens' to the notes.
  getNotes :: Lens' c [Sentence]

-- | Some concepts have a domain (related information encoded in 'UID's to other chunks).
class ConceptDomain c where
  -- | Provides Getter for the concept domain tags for a chunk
  cdom :: c -> [UID]
  -- ^ /cdom/ should be exported for use by the
  -- Drasil framework, but should not be exported beyond that.

-- | Concepts are 'Idea's with definitions and domains.
type Concept c = (Idea c, Definition c, ConceptDomain c)

-- | HasSpace is anything which has a 'Space'.
class HasSpace c where
  -- | Provides a 'Lens' to the 'Space'.
  typ      :: Lens' c Space

-- | A class that contains a list of 'Reference's.
class HasReference c where
  -- | Provides a 'Lens' to the 'Reference's.
  getReferences :: Lens' c [Reference]

-- | A class that contains a list of decorated references ('DecRef's).
class HasDecRef c where
  -- | Provides a 'Lens' to the 'DecRef's.
  getDecRefs :: Lens' c [DecRef]

-- | A class that might have a 'Derivation'.
class HasDerivation c where
  -- | Provides a 'Lens' to a possible derivation.
  derivations :: Lens' c (Maybe Derivation)

-- | CommonIdea is a 'NamedIdea' with the additional
-- constraint that it __must__ have an abbreviation.
class NamedIdea c => CommonIdea c where
  -- | Introduces abrv which necessarily provides an abbreviation.
  abrv :: c -> String

-- | The Constrained class is a 'Quantity' that has value constraints.
-- It does not enforce 'Quantity' at this point.
class Constrained c where
  -- | Provides a 'Lens' to the 'Constraint's.
  constraints :: Lens' c [ConstraintE]

-- | A 'Quantity' that could have a reasonable value.
class HasReasVal c where
  -- | Provides a 'Lens' to the possible reasonable value.
  reasVal     :: Lens' c (Maybe Expr)

-- | A Quantity is an 'Idea' with a 'Space' and a 'Symbol'.
-- In theory, it should also restrict to being a part of 'MayHaveUnit', but that causes
-- all sorts of import cycles (or lots of orphans).
class (Idea c, HasSpace c, HasSymbol c) => Quantity c where

--  An UncertainQuantity is just a Quantity with some uncertainty associated to it.
-- This uncertainty is represented as a decimal value between 0 and 1 (percentage).
--
-- class Quantity c => UncertainQuantity c where
--   uncert :: Lens' c (Uncertainty)
--   replaced with HasUncertainty

-- | HasUncertainty is just a chunk with some uncertainty associated to it.
-- This uncertainty is represented as a decimal value between 0 and 1 (percentage).
class HasUncertainty c where
  -- | Provides the 'Lens' to an 'Uncertainty'.
  unc  :: Lens' c Uncertainty

-- | Some chunks can be called like functions.
class (HasSymbol c) => Callable c

-----------------------------------------------------
-- Below are for units only
-- | Some chunks store a unit symbol.
class HasUnitSymbol u where
  -- | Provides the ability to hold a unit symbol ('USymb').
  usymb :: u -> USymb

-- | Units are 'Idea's with a 'Definition' which store a unit symbol.
-- They must also be explicitly declared to be instances of IsUnit.
class (Idea u, Definition u, HasUnitSymbol u) => IsUnit u where
  -- | May have a unit definition.
  udefn :: u -> Maybe UDefn
  -- | Holds units as a list of 'UID'.
  getUnits :: u -> [UID]

-- Investigate (TODO): is this really needed?
-- | Contains a 'UDefn'
class UnitEq u where
  -- | Provides the 'Lens' to a unit definition.
  uniteq :: Lens' u UDefn

-----------------------------------------------------

-- TODO: I think that this DefiningExpr is missing a "what it defines" component...
--       It's also only used for CodeDefinitions, QDefinitions, and DataDefinitions (food for thought on naming if we want to add ^)
-- | A better version of 'ExprRelat' that holds an 'Expr'.
class DefiningExpr c where
  -- | Provides a 'Lens' to the expression.
  defnExpr :: Lens' c Expr

-- | Members must have a named argument.
class (HasSymbol c) => IsArgumentName c where
