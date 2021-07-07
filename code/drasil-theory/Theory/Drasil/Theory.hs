{-# Language TemplateHaskell #-}
module Theory.Drasil.Theory (Theory(..), TheoryModel, tm, tmNoRefs, tm', tmNoRefs') where

import Theory.Drasil.ModelKinds (ModelKinds)

import Language.Drasil
import Data.Drasil.TheoryConcepts (thModel)

import Control.Lens (Lens', view, makeLenses, (^.))

-- | Theories are the basis for building models with context,
-- spaces, quantities, operations, invariants, etc.
class Theory t where
  valid_context :: Lens' t [TheoryModel]
  spaces        :: Lens' t [SpaceDefn]
  quantities    :: Lens' t [QuantityDict]
  operations    :: Lens' t [ConceptChunk] -- FIXME: Should not be Concept
  defined_quant :: Lens' t [DQDefinition]
  invariants    :: Lens' t [DisplayExpr]  -- TODO: temporary hack until designed, previously `Lens' t [Relation]`
  defined_fun   :: Lens' t [DQDefinition]

data SpaceDefn -- FIXME: This should be defined.

-- | A TheoryModel is a collection of:
--
--      * tUid - a UID,
--      * con - a ConceptChunk,
--      * vctx - definition context ('TheoryModel's),
--      * spc - type definitions ('SpaceDefn's),
--      * quan - quantities ('QuantityDict's),
--      * ops - operations ('ConceptChunk's),
--      * defq - definitions ('QDefinition's),
--      * invs - invariants ('DisplayExpr's),
--      * dfun - defined functions ('QDefinition's),
--      * ref - accompanying references ('Reference's),
--      * lb - a label ('SpaceDefn'),
--      * ra - reference address ('SpaceDefn'),
--      * notes - additional notes ('Sentence's).
-- 
-- Right now, neither the definition context (vctx) nor the
-- spaces (spc) are ever defined.
data TheoryModel = TM 
  { _tUid  :: UID
  , _con   :: ConceptChunk
  , _vctx  :: [TheoryModel]
  , _spc   :: [SpaceDefn]
  , _quan  :: [QuantityDict]
  , _ops   :: [ConceptChunk]
  , _defq  :: [DQDefinition]
  , _invs  :: [DisplayExpr]
  , _dfun  :: [DQDefinition]
  , _rf    :: [Reference]
  ,  lb    :: ShortName
  ,  ra    :: String
  , _notes :: [Sentence]
  }
makeLenses ''TheoryModel

-- | Finds the 'UID' of a 'TheoryModel'.
instance HasUID             TheoryModel where uid = tUid
-- | Finds the term ('NP') of the 'TheoryModel'.
instance NamedIdea          TheoryModel where term = con . term
-- | Finds the idea of the 'ConceptChunk' contained in the 'TheoryModel'.
instance Idea               TheoryModel where getA = getA . view con
-- | Finds the definition of the 'ConceptChunk' contained in a 'TheoryModel'.
instance Definition         TheoryModel where defn = con . defn
-- | Finds 'Reference's contained in the 'TheoryModel'.
instance HasReference       TheoryModel where getReferences = rf
-- | Finds the domain of the 'ConceptChunk' contained in a 'TheoryModel'.
instance ConceptDomain      TheoryModel where cdom = cdom . view con
-- | Finds any additional notes for the 'TheoryModel'.
instance HasAdditionalNotes TheoryModel where getNotes = notes

-- TODO: I think we should be gathering these from the ModelKinds of the TheoryModel.
--       If we need "more than 1 ModelKind" in the TheoryModel, we may need to create 
--       a "stacked model" that allows for composing them.

-- | Finds the aspects of the 'Theory' behind the 'TheoryModel'.
instance Theory             TheoryModel where
  valid_context = vctx
  spaces        = spc
  quantities    = quan
  operations    = ops
  defined_quant = defq
  invariants    = invs
  defined_fun   = dfun
-- | Finds the 'ShortName' of the 'TheoryModel'.
instance HasShortName       TheoryModel where shortname = lb
-- | Finds the reference address of the 'TheoryModel'.
instance HasRefAddress      TheoryModel where getRefAdd = ra
-- | Finds the idea of a 'TheoryModel' (abbreviation).
instance CommonIdea         TheoryModel where abrv _ = abrv thModel
-- | Finds the reference address of a 'TheoryModel'.
instance Referable TheoryModel where
  refAdd      = getRefAdd
  renderRef l = RP (prepend $ abrv l) (getRefAdd l)

-- TODO: Theory Models should generally be using their own UID, instead of
--       having their UIDs derived by the model kind.

-- This "smart" constructor is really quite awful, it takes way too many arguments.
-- This should likely be re-arranged somehow. Especially since since of the arguments
-- have the same type!
-- | Constructor for theory models.
tm :: (Quantity q, MayHaveUnit q, Concept c) => ModelKinds ->
    [q] -> [c] -> [DQDefinition] ->
    [DisplayExpr] -> [DQDefinition] -> [Reference] ->
    String -> [Sentence] -> TheoryModel
tm mk = tm' (mk ^. uid) mk

-- | Constructor for theory models with no references. 
tmNoRefs :: (Quantity q, MayHaveUnit q, Concept c) => ModelKinds ->
    [q] -> [c] -> [DQDefinition] -> [DisplayExpr] -> [DQDefinition] -> 
    String -> [Sentence] -> TheoryModel
tmNoRefs mk = tmNoRefs' (mk ^. uid) mk

-- | Constructor for theory models. Must have a source. Uses the shortname of the reference address.
tm' :: (Quantity q, MayHaveUnit q, Concept c) => UID -> ModelKinds ->
    [q] -> [c] -> [DQDefinition] ->
    [DisplayExpr] -> [DQDefinition] -> [Reference] ->
    String -> [Sentence] -> TheoryModel
tm' u _  _ _ _  _   _   [] _   = error $ "Source field of " ++ u ++ " is empty"
tm' u mk q c dq inv dfn r  lbe = 
  TM u (cw mk) [] [] (map qw q) (map cw c) dq inv dfn r (shortname' $ S lbe)
      (prependAbrv thModel lbe)

-- | Constructor for theory models. Uses the shortname of the reference address.
tmNoRefs' :: (Quantity q, MayHaveUnit q, Concept c) => UID -> ModelKinds ->
    [q] -> [c] -> [DQDefinition] -> [DisplayExpr] -> [DQDefinition] -> 
    String -> [Sentence] -> TheoryModel
tmNoRefs' u mk q c dq inv dfn lbe = 
  TM u (cw mk) [] [] (map qw q) (map cw c) dq inv dfn [] (shortname' $ S lbe)
      (prependAbrv thModel lbe)
