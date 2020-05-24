-- | Defining all the classes which represent knowledge-about-theories
module Theory.Drasil.Classes (
  -- the classes
  HasInputs(..)
  ) where

import Language.Drasil

import Control.Lens (Lens')

-- a class may have Inputs
class HasInputs c where
  inputs :: Lens' c [QuantityDict]
  inp_constraints :: Lens' c [Relation]

{-
-- | A NamedIdea is a 'term' that we've identified (has an 'id') as 
-- being worthy of naming.
class HasUID c => NamedIdea c where
  -- | Lens to the term (a noun phrase)
  term :: Lens' c NP

-- | An |Idea| is the 'meet' of |NamedIdea| and |CommonIdea|.
-- In other words, it /may/ have an acronym/abbreviation.
class NamedIdea c => Idea c where
  getA :: c -> Maybe String
  --Get Abbreviation/Acronym? These might need to be separated 
  --depending on contexts, but for now I don't see a problem with it.

class Definition c where
  -- | defn provides (a 'Lens' to) the definition for a chunk
  defn :: Lens' c Sentence

-- Temporary hack to avoid loss of information
class HasAdditionalNotes c where
  getNotes :: Lens' c [Sentence]

class ConceptDomain c where
  -- | cdom provides Getter for the concept domain tags for a chunk
  cdom :: c -> [UID]
  -- ^ /cdom/ should be exported for use by the
  -- Drasil framework, but should not be exported beyond that.

-- | Concepts are 'Idea's with definitions and domains
type Concept c = (Idea c, Definition c, ConceptDomain c)

-- | HasSpace is anything which has a Space...
class HasSpace c where
  typ      :: Lens' c Space

class HasReference c where
  getReferences :: Lens' c [Reference]

class HasDerivation c where
  derivations :: Lens' c (Maybe Derivation)

-- | CommonIdea is a 'NamedIdea' with the additional
-- constraint that it __must__ have an abbreviation.
class NamedIdea c => CommonIdea c where
  -- | Introduces abrv which necessarily provides an abbreviation.
  abrv :: c -> String

-- | A Constrained is a 'Quantity' that has value constraints
-- but do not enforce Quantity at this point
class Constrained c where
  constraints :: Lens' c [Constraint]

-- | A HasReasVal is a 'Quantity' that could have a reasonable value
class HasReasVal c where
  reasVal     :: Lens' c (Maybe Expr)

-- | A Quantity is an 'Idea' with a 'Space' and a symbol.
-- In theory, it should also have MayHaveUnit, but that causes
-- all sorts of import cycles (or lots of orphans)
class (Idea c, HasSpace c, HasSymbol c) => Quantity c where

-- | An UncertainQuantity is just a Quantity with some uncertainty associated to it.
-- This uncertainty is represented as a decimal value between 0 and 1 (percentage).

-- class Quantity c => UncertainQuantity c where
--   uncert :: Lens' c (Uncertainty)
--   replaced with HasUncertainty

class HasUncertainty c where
  unc  :: Lens' c Uncertainty

class HasUID s => Referable s where
  refAdd    :: s -> String  -- The referencing address (what we're linking to).
                            -- Only visible in the source (tex/html).
  renderRef :: s -> LblType -- alternate

-- | Some chunks can be called like functions
class (HasSymbol c) => Callable c

-----------------------------------------------------
-- Below are for units only
-- | Some chunks store a unit symbol
class HasUnitSymbol u where
   usymb ::u -> USymb

-- | Units are Ideas with a Definition which store a unit symbol.
-- They must also be explicitly declared to be instances of IsUnit
class (Idea u, Definition u, HasUnitSymbol u) => IsUnit u where
   udefn :: u -> Maybe UDefn
   getUnits :: u -> [UID]

-- Investigate (TODO): is this really needed?
class UnitEq u where
   uniteq :: Lens' u UDefn

-----------------------------------------------------
-- TODO : there is a design bug here not at all apparent from its definition; have to come back to it (Pull Request #532)
class ExprRelat c where
  relat :: Lens' c Expr

-- This is the 'correct' version of ExprRelat.
class DefiningExpr c where
  defnExpr :: Lens' c Expr

class (HasSymbol c) => IsArgumentName c where
-}