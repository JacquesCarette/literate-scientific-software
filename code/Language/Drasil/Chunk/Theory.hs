{-# Language TemplateHaskell, TypeFamilies #-}
module Language.Drasil.Chunk.Theory 
  ( tc', Theory(..), TheoryChunk, TheoryModel, tm,
  )where

import Language.Drasil.Classes (HasUID(uid), NamedIdea(term), Idea(getA),
  Definition(defn),ConceptDomain(cdom,DOM),Concept)
import Language.Drasil.Chunk.Concept
import Language.Drasil.Chunk.Constrained.Core (TheoryConstraint)
import Language.Drasil.Chunk.Eq
import Language.Drasil.Chunk.Quantity

import Control.Lens (Lens', view, makeLenses)

class HasUID t => Theory t where
  valid_context :: Lens' t [TheoryChunk]
  spaces        :: Lens' t [SpaceDefn] 
  quantities    :: Lens' t [QuantityDict]
  operations    :: Lens' t [ConceptChunk] -- FIXME: Should not be Concept
  defined_quant :: Lens' t [QDefinition]
  invariants    :: Lens' t [TheoryConstraint]
  defined_fun   :: Lens' t [QDefinition]
  
data SpaceDefn -- FIXME: This should be defined.
  
data TheoryChunk = TC {
  _tid :: String,
  _vctx :: [TheoryChunk],
  _spc  :: [SpaceDefn],
  _quan :: [QuantityDict],
  _ops  :: [ConceptChunk],
  _defq :: [QDefinition],
  _invs :: [TheoryConstraint],
  _dfun :: [QDefinition] }
makeLenses ''TheoryChunk
  
instance Theory TheoryChunk where
  valid_context = vctx
  spaces        = spc
  quantities    = quan
  operations    = ops
  defined_quant = defq
  invariants    = invs
  defined_fun   = dfun

instance HasUID TheoryChunk where uid = tid

-- use the id of the TheoryModel as the uid. FIXME ?
data TheoryModel = TM {_con :: ConceptChunk, _thy :: TheoryChunk }
makeLenses ''TheoryModel
  
instance HasUID TheoryModel where uid = con . uid
instance NamedIdea TheoryModel where term = con . term
instance Idea TheoryModel where getA = getA . view con
instance Definition TheoryModel where defn = con . defn
instance ConceptDomain TheoryModel where
  type DOM TheoryModel = ConceptChunk
  cdom = con . cdom
instance Concept TheoryModel where
instance Theory TheoryModel where
  valid_context = thy . valid_context
  spaces        = thy . spaces
  quantities    = thy . quantities
  operations    = thy . operations
  defined_quant = thy . defined_quant
  invariants    = thy . invariants
  defined_fun   = thy . defined_fun

tc :: (Quantity q, Concept c, DOM c ~ ConceptChunk) => String -> [TheoryChunk] -> 
  [SpaceDefn] -> [q] -> [c] -> [QDefinition] -> [TheoryConstraint] -> 
  [QDefinition] -> TheoryChunk
tc cid t s q c = TC cid t s (map qw q) (map cw c)

tc' :: (Quantity q, Concept c, DOM c ~ ConceptChunk) =>
  String -> [q] -> [c] -> [QDefinition] -> 
  [TheoryConstraint] -> [QDefinition] -> TheoryChunk
tc' cid q c = tc cid ([] :: [TheoryChunk]) [] q c

tm :: (Concept c, DOM c ~ ConceptChunk) => c -> TheoryChunk -> TheoryModel
tm c t = TM (cw c) t
