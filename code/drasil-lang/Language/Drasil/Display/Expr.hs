{-# LANGUAGE GADTs #-}

module Language.Drasil.Display.Expr (DisplayExpr(..),
  UOp(..), BinOp(..), AssocBinOp(..),
  DerivType(..)) where

import Control.Lens ((^.))

import Language.Drasil.Classes.Core (HasSymbol, HasUID(..))
import Language.Drasil.Expr (Completeness(..))
import Language.Drasil.Space (RealInterval, DomainDesc, Space)
import Language.Drasil.UID (UID)


-- | Unary operators & common functions.
data UOp = Abs | Log | Ln | Sin | Cos | Tan | Sec | Csc | Cot | Arcsin
  | Arccos | Arctan | Exp | Sqrt | Neg | Not | Norm | Dim
  deriving (Eq, Show)

-- | Binary operators. 
data BinOp = Frac | Pow | Subt | Eq | NEq | Impl | Iff | Index 
  | Lt | Gt | LEq | GEq | Cross | Dot | Defines | IsIn
  deriving (Eq, Show)

-- | Associative binary operators.
data AssocBinOp = Add | Mul | And | Or | Equivalence
  deriving (Eq, Show)

-- | Determines the type of the derivative (either a partial derivative or a total derivative).
-- TODO: Realistically, this isn't just for "Display", this should eventually
--       be something that we do _something_ with. I'd say this should go into the
--       4th DisplayExpr language.
data DerivType = Part | Total
  deriving (Eq, Show)


-- | A variant of the expression language that allows for extensions of DisplayExpr
--   that wouldn't normally type check, or that we only care for displaying.
data DisplayExpr where
  -- | Turns a decimal value ('Double') into an expression.
  Dbl      :: Double -> DisplayExpr
  -- | Turns an integer into an expression.
  Int      :: Integer -> DisplayExpr
  -- | Represents decimal values that are exact as integers.
  ExactDbl :: Integer -> DisplayExpr 
  -- | Turns a string into an expression.
  Str      :: String -> DisplayExpr
  -- | Turns two integers into a fraction (or percent).
  Perc     :: Integer -> Integer -> DisplayExpr
  -- | Takes an associative operator with a list of expressions.
  Assoc   :: AssocBinOp -> [DisplayExpr] -> DisplayExpr
  -- | C stands for "Chunk", for referring to a chunk in an expression.
  --   Implicitly assumes that the chunk has a symbol.
  C        :: UID -> DisplayExpr
  -- | A function call accepts a list of parameters and a list of named parameters.
  --   For example
  --
  --   * F(x) is (FCall F [x] []).
  --   * F(x,y) would be (FCall F [x,y]).
  --   * F(x,n=y) would be (FCall F [x] [(n,y)]).
  FCall    :: UID -> [DisplayExpr] -> [(UID, DisplayExpr)] -> DisplayExpr
  -- | For multi-case expressions, each pair represents one case.
  Case     :: Completeness -> [(DisplayExpr, DisplayExpr)] -> DisplayExpr
  -- | Represents a matrix of expressions.
  Matrix   :: [[DisplayExpr]] -> DisplayExpr
  
  -- | Unary operation for operators & common functions.
  UnaryOp       :: UOp -> DisplayExpr -> DisplayExpr
  -- | Binary operators.
  BinaryOp :: BinOp -> DisplayExpr -> DisplayExpr -> DisplayExpr

  -- | Operators are generalized arithmetic operators over a 'DomainDesc'
  --   of an 'Expr'.  Could be called BigOp.
  --   ex: Summation is represented via 'Add' over a discrete domain.
  Operator :: AssocBinOp -> DomainDesc DisplayExpr DisplayExpr -> DisplayExpr -> DisplayExpr
  -- | A different kind of 'IsIn'. A 'UID' is an element of an interval.
  RealI    :: UID -> RealInterval DisplayExpr DisplayExpr -> DisplayExpr


  -- | TODO: Currently only used for Discrete Spaces as a display hack for the constraints of nominal thickness in GlassBR
  SpaceExpr     :: Space -> DisplayExpr
  -- | Derivative syntax is:
  --   Type ('Part'ial or 'Total') -> principal part of change -> with respect to
  --   For example: Deriv Part y x1 would be (dy/dx1).
  Deriv         :: DerivType -> DisplayExpr -> UID -> DisplayExpr


-- TODO: -- This also wants a symbol constraint.
-- TODO: -- | Gets the derivative of an 'Expr' with respect to a 'Symbol'.
-- TODO: deriv, pderiv :: (HasUID c, HasSymbol c) => DisplayExpr -> c -> DisplayExpr
-- TODO: deriv e c = Deriv Total e (c ^. uid)
-- TODO: pderiv e c = Deriv Part e (c ^. uid)


