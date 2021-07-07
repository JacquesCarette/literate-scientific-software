{-# LANGUAGE GADTs #-}

module Language.Drasil.Code.Expr where

import Language.Drasil (UID, DomainDesc, Completeness, RealInterval)

-- Binary functions

-- | Arithmetic operators (fractional, power, and subtraction).
data ArithBinOp = Frac | Pow | Subt
  deriving Eq

-- | Equality operators (equal or not equal).
data EqBinOp = Eq | NEq
  deriving Eq

-- | Conditional and Biconditional operators (Expressions can imply
-- one another, or exist if and only if another expression exists).
data BoolBinOp = Impl | Iff
  deriving Eq

-- | Index operator.
data LABinOp = Index
  deriving Eq

-- | Ordered binary operators (less than, greater than, less than or equal to, greater than or equal to).
data OrdBinOp = Lt | Gt | LEq | GEq
  deriving Eq

-- | @Vector x Vector -> Vector@ binary operations (cross product).
data VVVBinOp = Cross
  deriving Eq

-- | @Vector x Vector -> Number@ binary operations (dot product).
data VVNBinOp = Dot
  deriving Eq

-- | Associative operators (adding and multiplication). Also specifies whether it is for integers or for real numbers.
data AssocArithOper = AddI | AddRe | MulI | MulRe
  deriving Eq

-- | Associative boolean operators (and, or).
data AssocBoolOper = And | Or
  deriving Eq

-- | Unary functions (abs, log, ln, sin, etc.).
data UFunc = Abs | Log | Ln | Sin | Cos | Tan | Sec | Csc | Cot | Arcsin
  | Arccos | Arctan | Exp | Sqrt | Neg
  deriving Eq

-- | @Bool -> Bool@ operators.
data UFuncB = Not
  deriving Eq

-- | @Vector -> Vector@ operators.
data UFuncVV = NegV
  deriving Eq

-- | @Vector -> Number@ operators.
data UFuncVN = Norm | Dim
  deriving Eq

-- | A one-to-one clone of Expr, with extra OO/code-related functionality.
data CodeExpr where
  -- | Turns a decimal value ('Double') into an expression.
  Dbl      :: Double -> CodeExpr
  -- | Turns an integer into an expression.
  Int      :: Integer -> CodeExpr
  -- | Represents decimal values that are exact as integers.
  ExactDbl :: Integer -> CodeExpr 
  -- | Turns a string into an expression.
  Str      :: String -> CodeExpr
  -- | Turns two integers into a fraction (or percent).
  Perc     :: Integer -> Integer -> CodeExpr
  -- | Takes an associative arithmetic operator with a list of expressions.
  AssocA   :: AssocArithOper -> [CodeExpr] -> CodeExpr
  -- | Takes an associative boolean operator with a list of expressions.
  AssocB   :: AssocBoolOper  -> [CodeExpr] -> CodeExpr
  -- | C stands for "Chunk", for referring to a chunk in an expression.
  --   Implicitly assumes that the chunk has a symbol.
  C        :: UID -> CodeExpr
  -- | A function call accepts a list of parameters and a list of named parameters.
  --   For example
  --
  --   * F(x) is (FCall F [x] []).
  --   * F(x,y) would be (FCall F [x,y]).
  --   * F(x,n=y) would be (FCall F [x] [(n,y)]).
  FCall    :: UID -> [CodeExpr] -> [(UID, CodeExpr)] -> CodeExpr
  -- | Actor creation given 'UID', parameters, and named parameters.
  New      :: UID -> [CodeExpr] -> [(UID, CodeExpr)] -> CodeExpr
  -- | Message an actor:
  --
  --   * 1st 'UID' is the actor,
  --   * 2nd 'UID' is the method.
  Message  :: UID -> UID -> [CodeExpr] -> [(UID, CodeExpr)] -> CodeExpr
  -- | Access a field of an actor:
  --
  --   * 1st 'UID' is the actor,
  --   * 2nd 'UID' is the field.
  Field    :: UID -> UID -> CodeExpr
  -- | For multi-case expressions, each pair represents one case.
  Case     :: Completeness -> [(CodeExpr, CodeExpr)] -> CodeExpr
  -- | Represents a matrix of expressions.
  Matrix   :: [[CodeExpr]] -> CodeExpr
  
  -- | Unary operation for most functions (eg. sin, cos, log, etc.).
  UnaryOp       :: UFunc -> CodeExpr -> CodeExpr
  -- | Unary operation for @Bool -> Bool@ operations.
  UnaryOpB      :: UFuncB -> CodeExpr -> CodeExpr
  -- | Unary operation for @Vector -> Vector@ operations.
  UnaryOpVV     :: UFuncVV -> CodeExpr -> CodeExpr
  -- | Unary operation for @Vector -> Number@ operations.
  UnaryOpVN     :: UFuncVN -> CodeExpr -> CodeExpr

  -- | Binary operator for arithmetic between expressions (fractional, power, and subtraction).
  ArithBinaryOp :: ArithBinOp -> CodeExpr -> CodeExpr -> CodeExpr
  -- | Binary operator for boolean operators (implies, iff).
  BoolBinaryOp  :: BoolBinOp -> CodeExpr -> CodeExpr -> CodeExpr
  -- | Binary operator for equality between expressions.
  EqBinaryOp    :: EqBinOp -> CodeExpr -> CodeExpr -> CodeExpr
  -- | Binary operator for indexing two expressions.
  LABinaryOp    :: LABinOp -> CodeExpr -> CodeExpr -> CodeExpr
  -- | Binary operator for ordering expressions (less than, greater than, etc.).
  OrdBinaryOp   :: OrdBinOp -> CodeExpr -> CodeExpr -> CodeExpr
  -- | Binary operator for @Vector x Vector -> Vector@ operations (cross product).
  VVVBinaryOp   :: VVVBinOp -> CodeExpr -> CodeExpr -> CodeExpr
  -- | Binary operator for @Vector x Vector -> Number@ operations (dot product).
  VVNBinaryOp   :: VVNBinOp -> CodeExpr -> CodeExpr -> CodeExpr

  -- | Operators are generalized arithmetic operators over a 'DomainDesc'
  --   of an 'Expr'.  Could be called BigOp.
  --   ex: Summation is represented via 'Add' over a discrete domain.
  Operator :: AssocArithOper -> DomainDesc CodeExpr CodeExpr -> CodeExpr -> CodeExpr
  -- | The expression is an element of a space.
  -- IsIn     :: Expr -> Space -> Expr
  -- | A different kind of 'IsIn'. A 'UID' is an element of an interval.
  RealI    :: UID -> RealInterval CodeExpr CodeExpr -> CodeExpr
