module Language.Drasil.Display.Extract (deDep, deDep') where

import Data.List (nub)

import Language.Drasil.Display.Expr (DisplayExpr(..))
import Language.Drasil.Space (RealInterval(..))

-- | Generic traverse of all display expressions that could lead to names.
deNames :: DisplayExpr -> [String]
deNames (SpaceExpr _)    = []
deNames (Assoc _ l)      = concatMap deNames l
deNames (Deriv _ a b)    = b : deNames a
deNames (C c)            = [c]
deNames Int{}            = []
deNames Dbl{}            = []
deNames ExactDbl{}       = []
deNames Str{}            = []
deNames Perc{}           = []
deNames (FCall f x ns)   = f : concatMap deNames x ++ map fst ns 
    ++ concatMap (deNames . snd) ns
deNames (Case _ ls)      = concatMap (deNames . fst) ls
    ++ concatMap (deNames . snd) ls
deNames (UnaryOp _ u)    = deNames u
deNames (BinaryOp _ a b) = deNames a ++ deNames b
deNames (Operator _ _ e) = deNames e
deNames (Matrix a)       = concatMap (concatMap deNames) a
deNames (RealI c b)      = c : eNamesRI b

-- | Generic traversal of everything that could come from an interval to names (similar to 'eNames').
eNamesRI :: RealInterval DisplayExpr DisplayExpr -> [String]
eNamesRI (Bounded (_, il) (_, iu)) = deNames il ++ deNames iu
eNamesRI (UpTo (_, iu))            = deNames iu
eNamesRI (UpFrom (_, il))          = deNames il

-- | Generic traverse of all positions that could lead to 'eNames' without
-- functions.  FIXME : this should really be done via post-facto filtering, but
-- right now the information needed to do this is not available!
deNames' :: DisplayExpr -> [String]
deNames' (SpaceExpr _)      = []
deNames' (Assoc _ l)      = concatMap deNames' l
deNames' (Deriv _ a b)    = b : deNames' a
deNames' (C c)            = [c]
deNames' Int{}            = []
deNames' Dbl{}            = []
deNames' ExactDbl{}       = []
deNames' Str{}            = []
deNames' Perc{}           = []
deNames' (FCall _ x ns)   = concatMap deNames' x ++ map fst ns
    ++ concatMap (deNames' . snd) ns
deNames' (Case _ ls)      = concatMap (deNames' . fst) ls
    ++ concatMap (deNames' .  snd) ls
deNames' (UnaryOp _ u)    = deNames' u
deNames' (BinaryOp _ a b) = deNames' a ++ deNames' b
deNames' (Operator _ _ e) = deNames' e
deNames' (Matrix a)       = concatMap (concatMap deNames') a
deNames' (RealI c b)      = c : eNamesRI' b

-- | Generic traversal of everything that could come from an interval to names without functions (similar to 'eNames'').
eNamesRI' :: RealInterval DisplayExpr DisplayExpr -> [String]
eNamesRI' (Bounded il iu) = deNames' (snd il) ++ deNames' (snd iu)
eNamesRI' (UpTo iu)       = deNames' (snd iu)
eNamesRI' (UpFrom il)     = deNames' (snd il)

---------------------------------------------------------------------------
-- And now implement the exported traversals all in terms of the above

-- | Get dependencies from a display expression.
deDep :: DisplayExpr -> [String]
deDep = nub . deNames

-- | Get dependencies from a display expression, ignoring functions.
deDep' :: DisplayExpr -> [String]
deDep' = nub . deNames'
