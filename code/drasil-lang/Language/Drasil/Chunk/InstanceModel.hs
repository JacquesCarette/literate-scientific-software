{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
module Language.Drasil.Chunk.InstanceModel 
  ( InstanceModel
  , inCons, outCons, imOutput, imInputs
  , im, imQD, im', imQD'
  )where

import Language.Drasil.Classes (HasUID(uid), NamedIdea(term), Idea(getA),
  Definition(defn),ConceptDomain(cdom), Concept, ExprRelat(relat),
  HasDerivation(derivations), HasReference(getReferences), HasAdditionalNotes(getNotes),
  HasLabel(getLabel))
import Language.Drasil.Chunk.References (References)
import Language.Drasil.Chunk.Derivation (Derivation)
import Language.Drasil.Chunk.ShortName (HasShortName(shortname))
import Language.Drasil.Chunk.Constrained.Core (TheoryConstraint)
import Language.Drasil.Chunk.Eq (QDefinition, equat)
import Language.Drasil.Chunk.Relation (RelationConcept, makeRC)
import Language.Drasil.Chunk.Quantity (QuantityDict, qw)
import Language.Drasil.ChunkDB (HasSymbolTable)
import Language.Drasil.Expr (($=))
import Language.Drasil.Expr.Math (sy)
import Language.Drasil.Expr.Extract (vars)
import Language.Drasil.Spec (Sentence)
import Language.Drasil.Label.Core (Label)

import Control.Lens (makeLenses, (^.))

type Inputs = [QuantityDict]
type Output = QuantityDict

type InputConstraints  = [TheoryConstraint]
type OutputConstraints = [TheoryConstraint]

-- | An Instance Model is a RelationConcept that may have specific input/output
-- constraints. It also has attributes like derivation, source, etc.
data InstanceModel = IM { _rc :: RelationConcept
                        , _imInputs :: Inputs
                        , _inCons :: InputConstraints
                        , _imOutput :: Output
                        , _outCons :: OutputConstraints
                        , _ref :: References
                        , _deri :: Derivation
                        , _lb :: Label
                        , _notes :: Maybe [Sentence]
                        }
makeLenses ''InstanceModel
  
instance HasUID             InstanceModel where uid = rc . uid
instance NamedIdea          InstanceModel where term = rc . term
instance Idea               InstanceModel where getA (IM a _ _ _ _ _ _ _ _) = getA a
instance Concept            InstanceModel where
instance Definition         InstanceModel where defn = rc . defn
instance ConceptDomain      InstanceModel where cdom = rc . cdom
instance ExprRelat          InstanceModel where relat = rc . relat
instance HasDerivation      InstanceModel where derivations = deri
instance HasReference       InstanceModel where getReferences = ref
instance HasLabel           InstanceModel where getLabel = lb
instance HasShortName       InstanceModel where shortname = lb . shortname
instance HasAdditionalNotes InstanceModel where getNotes = notes

-- | Smart constructor for instance models
im :: RelationConcept -> Inputs -> InputConstraints -> Output -> 
  OutputConstraints -> Label -> InstanceModel
im rc i ic o oc lbe = IM rc i ic o oc [] [] lbe Nothing

-- | Same as `im`, with an additional field for notes to be passed in
im' :: RelationConcept -> Inputs -> InputConstraints -> Output -> 
  OutputConstraints -> Label -> [Sentence] -> InstanceModel
im' rc i ic o oc lbe notes = IM rc i ic o oc [] [] lbe (Just notes)

-- | Smart constructor for instance model from qdefinition 
-- (Sentence is the "concept" definition for the relation concept)
-- FIXME: get the shortname from the QDefinition?
imQD :: HasSymbolTable ctx => ctx -> QDefinition -> Sentence -> 
  InputConstraints -> OutputConstraints -> Label -> Maybe Label -> InstanceModel
imQD ctx qd dfn incon ocon lblForIM lblForRC = IM (makeRC (qd ^. uid) (qd ^. term) dfn 
  (sy qd $= qd ^. equat) lblForRC) (vars (qd^.equat) ctx) incon (qw qd) ocon [] [] 
  lblForIM Nothing 

-- Same as `imQD`, with an additional field for notes to be passed in
-- FIXME: get the shortname from the QDefinition?
imQD' :: HasSymbolTable ctx => ctx -> QDefinition -> Sentence -> 
  InputConstraints -> OutputConstraints -> Label -> Maybe [Sentence] -> 
  Maybe Label -> InstanceModel
imQD' ctx qd dfn incon ocon lblForIM notes lblForRC = IM (makeRC (qd ^. uid) (qd ^. term) dfn 
  (sy qd $= qd ^. equat) lblForRC) (vars (qd^.equat) ctx) incon (qw qd) ocon [] [] 
  lblForIM notes