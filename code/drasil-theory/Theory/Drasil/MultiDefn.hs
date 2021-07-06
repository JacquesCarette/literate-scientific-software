{-# LANGUAGE TemplateHaskell, Rank2Types, ScopedTypeVariables, PostfixOperators  #-}

module Theory.Drasil.MultiDefn (MultiDefn, DefiningExpr,
    mkMultiDefn, mkMultiDefnForQuant, mkDefiningExpr,
    multiDefnGenQD, multiDefnGenQDByUID) where

import Control.Lens ((^.), view, makeLenses)
import Data.List (union)
import qualified Data.List.NonEmpty as NE

import Language.Drasil hiding (DefiningExpr)
import qualified Language.Drasil.DisplayExpr as DE

-- | 'DefiningExpr' are the data that make up a (quantity) definition, namely
--   the description, the defining (rhs) expression and the context domain(s).
--   These are meant to be 'alternate' but equivalent definitions for a single concept.
data DefiningExpr = DefiningExpr {
  _deUid  :: UID,      -- ^ UID
  _cd     :: [UID],    -- ^ Concept domain
  _rvDesc :: Sentence, -- ^ Defining description/statement
  _expr   :: Expr      -- ^ Defining expression
}
makeLenses ''DefiningExpr

instance Eq            DefiningExpr where a == b = a ^. uid == b ^. uid
instance HasUID        DefiningExpr where uid    = deUid
instance ConceptDomain DefiningExpr where cdom   = (^. cd)
instance Definition    DefiningExpr where defn   = rvDesc

-- | 'MultiDefn's are QDefinition factories, used for showing one or more ways we
--   can define a QDefinition.
data MultiDefn = MultiDefn {
    _rUid  :: UID,                     -- ^ UID
    _qd    :: QuantityDict,            -- ^ Underlying quantity it defines
    _rDesc :: Sentence,                -- ^ Defining description/statement
    _rvs   :: NE.NonEmpty DefiningExpr -- ^ All possible/omitted ways we can define the related quantity
}
makeLenses ''MultiDefn


instance HasUID        MultiDefn where uid      = rUid
instance HasSymbol     MultiDefn where symbol   = symbol . (^. qd)
instance NamedIdea     MultiDefn where term     = qd . term
instance Idea          MultiDefn where getA     = getA . (^. qd)
instance HasSpace      MultiDefn where typ      = qd . typ
instance Quantity      MultiDefn where
instance MayHaveUnit   MultiDefn where getUnit  = getUnit . view qd
-- | The concept domain of a MultiDefn is the union of the concept domains of the underlying variants.
instance ConceptDomain MultiDefn where cdom     = foldr1 union . NE.toList . NE.map (^. cd) . (^. rvs)
instance Definition    MultiDefn where defn     = rDesc
-- | The complete Relation of a MultiDefn is defined as the quantity and the related expressions being equal
--   e.g., `q $= a $= b $= ... $= z`
instance Display       MultiDefn where
  toDispExpr q = DE.equiv $ sy q : NE.toList (NE.map (^. expr) (q ^. rvs))

-- | Smart constructor for MultiDefns, does nothing special at the moment
mkMultiDefn :: UID -> QuantityDict -> Sentence -> NE.NonEmpty DefiningExpr -> MultiDefn
mkMultiDefn u q s des
  | length des == dupsRemovedLen = MultiDefn u q s des
  | otherwise                    = error $
    "MultiDefn '" ++ u ++ "' created with non-unique list of expressions"
  where dupsRemovedLen = length $ NE.nub des

-- | Smart constructor for MultiDefns defining UIDs using that of the QuantityDict
mkMultiDefnForQuant :: QuantityDict -> Sentence -> NE.NonEmpty DefiningExpr -> MultiDefn
mkMultiDefnForQuant q = mkMultiDefn (q ^. uid) q

-- | Smart constructor for DefiningExprs
mkDefiningExpr :: UID -> [UID] -> Sentence -> Expr -> DefiningExpr
mkDefiningExpr = DefiningExpr

-- | Convert MultiDefns into QDefinitions via a specific DefiningExpr 
multiDefnGenQD :: MultiDefn -> DefiningExpr -> QDefinition
multiDefnGenQD md de = mkQDefSt (md ^. qd . uid) (md ^. term) (md ^. defn)
                                (symbol md) (md ^. typ) (getUnit md) (de ^. expr)

-- | Convert MultiDefns into QDefinitions via a specific DefiningExpr (by UID)
multiDefnGenQDByUID :: MultiDefn -> UID -> QDefinition
multiDefnGenQDByUID md u | length matches == 1 = multiDefnGenQD md matched
                         | otherwise           = error $ "Invalid UID for multiDefn QD generation; " ++ u
  where matches = NE.filter (\x -> x ^. uid == u) (md ^. rvs)
        matched = head matches
