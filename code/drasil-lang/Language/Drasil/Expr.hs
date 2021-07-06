{-# LANGUAGE GADTs #-}
-- | The Drasil Expression language
module Language.Drasil.Expr where

import Prelude hiding (sqrt)

import Language.Drasil.Space (DomainDesc, RealInterval)
import Language.Drasil.UID (UID)

-- | A relation is just an expression ('Expr').
type Relation = Expr

infixr 8 $^
infixl 7 $/
infixr 4 $=
infixr 9 $&&
infixr 9 $||

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

-- | For case expressions (either complete or incomplete).
data Completeness = Complete | Incomplete
  deriving Eq

-- | Drasil expressions.
data Expr where
  -- | Turns a decimal value ('Double') into an expression.
  Dbl      :: Double -> Expr
  -- | Turns an integer into an expression.
  Int      :: Integer -> Expr
  -- | Represents decimal values that are exact as integers.
  ExactDbl :: Integer -> Expr 
  -- | Turns a string into an expression.
  Str      :: String -> Expr
  -- | Turns two integers into a fraction (or percent).
  Perc     :: Integer -> Integer -> Expr
  -- | Takes an associative arithmetic operator with a list of expressions.
  AssocA   :: AssocArithOper -> [Expr] -> Expr
  -- | Takes an associative boolean operator with a list of expressions.
  AssocB   :: AssocBoolOper  -> [Expr] -> Expr
  -- | C stands for "Chunk", for referring to a chunk in an expression.
  --   Implicitly assumes that the chunk has a symbol.
  C        :: UID -> Expr
  -- | A function call accepts a list of parameters and a list of named parameters.
  --   For example
  --
  --   * F(x) is (FCall F [x] []).
  --   * F(x,y) would be (FCall F [x,y]).
  --   * F(x,n=y) would be (FCall F [x] [(n,y)]).
  FCall    :: UID -> [Expr] -> [(UID, Expr)] -> Expr
  -- | For multi-case expressions, each pair represents one case.
  Case     :: Completeness -> [(Expr,Relation)] -> Expr
  -- | Represents a matrix of expressions.
  Matrix   :: [[Expr]] -> Expr
  
  -- | Unary operation for most functions (eg. sin, cos, log, etc.).
  UnaryOp       :: UFunc -> Expr -> Expr
  -- | Unary operation for @Bool -> Bool@ operations.
  UnaryOpB      :: UFuncB -> Expr -> Expr
  -- | Unary operation for @Vector -> Vector@ operations.
  UnaryOpVV     :: UFuncVV -> Expr -> Expr
  -- | Unary operation for @Vector -> Number@ operations.
  UnaryOpVN     :: UFuncVN -> Expr -> Expr

  -- | Binary operator for arithmetic between expressions (fractional, power, and subtraction).
  ArithBinaryOp :: ArithBinOp -> Expr -> Expr -> Expr
  -- | Binary operator for boolean operators (implies, iff).
  BoolBinaryOp  :: BoolBinOp -> Expr -> Expr -> Expr
  -- | Binary operator for equality between expressions.
  EqBinaryOp    :: EqBinOp -> Expr -> Expr -> Expr
  -- | Binary operator for indexing two expressions.
  LABinaryOp    :: LABinOp -> Expr -> Expr -> Expr
  -- | Binary operator for ordering expressions (less than, greater than, etc.).
  OrdBinaryOp   :: OrdBinOp -> Expr -> Expr -> Expr
  -- | Binary operator for @Vector x Vector -> Vector@ operations (cross product).
  VVVBinaryOp   :: VVVBinOp -> Expr -> Expr -> Expr
  -- | Binary operator for @Vector x Vector -> Number@ operations (dot product).
  VVNBinaryOp   :: VVNBinOp -> Expr -> Expr -> Expr

  -- | Operators are generalized arithmetic operators over a 'DomainDesc'
  --   of an 'Expr'.  Could be called BigOp.
  --   ex: Summation is represented via 'Add' over a discrete domain.
  Operator :: AssocArithOper -> DomainDesc Expr Expr -> Expr -> Expr
  -- | A different kind of 'IsIn'. A 'UID' is an element of an interval.
  RealI    :: UID -> RealInterval Expr Expr -> Expr

($=), ($!=) :: Expr -> Expr -> Expr
-- | Smart constructor for equating two expressions.
($=)  = EqBinaryOp Eq
-- | Smart constructor for showing that two expressions are not equal.
($!=) = EqBinaryOp NEq

-- | Smart constructor for ordering two equations.
($<), ($>), ($<=), ($>=) :: Expr -> Expr -> Expr
-- | Less than.
($<)  = OrdBinaryOp Lt
-- | Greater than.
($>)  = OrdBinaryOp Gt
-- | Less than or equal to.
($<=) = OrdBinaryOp LEq
-- | Greater than or equal to.
($>=) = OrdBinaryOp GEq

-- | Smart constructor for the dot product of two equations.
($.) :: Expr -> Expr -> Expr
($.) = VVNBinaryOp Dot

-- Generate below 4 functions with TH?
-- | Add two expressions (Integers).
addI :: Expr -> Expr -> Expr
addI l (Int 0) = l
addI (Int 0) r = r
addI (AssocA AddI l) (AssocA AddI r) = AssocA AddI (l ++ r)
addI (AssocA AddI l) r = AssocA AddI (l ++ [r])
addI l (AssocA AddI r) = AssocA AddI (l : r)
addI l r = AssocA AddI [l, r]

-- | Add two expressions (Real numbers).
addRe :: Expr -> Expr -> Expr
addRe l (Dbl 0)      = l
addRe (Dbl 0) r      = r
addRe l (ExactDbl 0) = l
addRe (ExactDbl 0) r = r
addRe (AssocA AddRe l) (AssocA AddRe r) = AssocA AddRe (l ++ r)
addRe (AssocA AddRe l) r = AssocA AddRe (l ++ [r])
addRe l (AssocA AddRe r) = AssocA AddRe (l : r)
addRe l r = AssocA AddRe [l, r]

-- | Multiply two expressions (Integers).
mulI :: Expr -> Expr -> Expr
mulI l (Int 1) = l
mulI (Int 1) r = r
mulI (AssocA MulI l) (AssocA MulI r) = AssocA MulI (l ++ r)
mulI (AssocA MulI l) r = AssocA MulI (l ++ [r])
mulI l (AssocA MulI r) = AssocA MulI (l : r)
mulI l r = AssocA MulI [l, r]

-- | Multiply two expressions (Real numbers).
mulRe :: Expr -> Expr -> Expr
mulRe l (Dbl 1)      = l
mulRe (Dbl 1) r      = r
mulRe l (ExactDbl 1) = l
mulRe (ExactDbl 1) r = r
mulRe (AssocA MulRe l) (AssocA MulRe r) = AssocA MulRe (l ++ r)
mulRe (AssocA MulRe l) r = AssocA MulRe (l ++ [r])
mulRe l (AssocA MulRe r) = AssocA MulRe (l : r)
mulRe l r = AssocA MulRe [l, r]

($-), ($/), ($^) :: Expr -> Expr -> Expr
-- | Smart constructor for subtracting two expressions.
($-) = ArithBinaryOp Subt
-- | Smart constructor for dividing two expressions.
($/) = ArithBinaryOp Frac
-- | Smart constructor for rasing the first expression to the power of the second.
($^) = ArithBinaryOp Pow

($=>), ($<=>) :: Expr -> Expr -> Expr
-- | Smart constructor to show that one expression implies the other (conditional operator).
($=>)  = BoolBinaryOp Impl
-- | Smart constructor to show that an expression exists if and only if another expression exists (biconditional operator).
($<=>) = BoolBinaryOp Iff

($&&), ($||) :: Expr -> Expr -> Expr
-- | Smart constructor for the boolean /and/ operator.
a $&& b = AssocB And [a, b]
-- | Smart constructor for the boolean /or/ operator.
a $|| b = AssocB Or  [a, b]

-- | The variable type is just a renamed 'String'.
type Variable = String

-- instance Num Expr where
--   (Int 0)        + b              = b
--   a              + (Int 0)        = a
--   (AssocA Add l) + (AssocA Add m) = AssocA Add (l ++ m)
--   (AssocA Add l) + b              = AssocA Add (l ++ [b])
--   a              + (AssocA Add l) = AssocA Add (a : l)
--   a              + b              = AssocA Add [a, b]

--   (AssocA Mul l) * (AssocA Mul m) = AssocA Mul (l ++ m)
--   (AssocA Mul l) * b              = AssocA Mul (l ++ [b])
--   a              * (AssocA Mul l) = AssocA Mul (a : l)
--   a              * b              = AssocA Mul [a, b]

--   a - b = ArithBinaryOp Subt a b
  
--   fromInteger = Int
--   abs         = UnaryOp Abs
--   negate      = UnaryOp Neg

--   -- this is a Num wart
--   signum _ = error "should not use signum in expressions"

-- | Expressions are equal if their constructors and contents are equal.
instance Eq Expr where
  Dbl a               == Dbl b               =   a == b
  Int a               == Int b               =   a == b
  Str a               == Str b               =   a == b
  AssocA o1 l1        == AssocA o2 l2        =  o1 == o2 && l1 == l2
  AssocB o1 l1        == AssocB o2 l2        =  o1 == o2 && l1 == l2
  C a                 == C b                 =   a == b
  FCall a b c         == FCall d e f         =   a == d && b == e && c == f
  Case a b            == Case c d            =   a == c && b == d 
  UnaryOp a b         == UnaryOp c d         =   a == c && b == d
  UnaryOpB a b        == UnaryOpB c d        =   a == c && b == d
  UnaryOpVV a b       == UnaryOpVV c d       =   a == c && b == d
  UnaryOpVN a b       == UnaryOpVN c d       =   a == c && b == d
  ArithBinaryOp o a b == ArithBinaryOp p c d =   o == p && a == c && b == d
  BoolBinaryOp o a b  == BoolBinaryOp p c d  =   o == p && a == c && b == d
  EqBinaryOp o a b    == EqBinaryOp p c d    =   o == p && a == c && b == d
  OrdBinaryOp o a b   == OrdBinaryOp p c d   =   o == p && a == c && b == d
  LABinaryOp o a b    == LABinaryOp p c d    =   o == p && a == c && b == d
  VVVBinaryOp o a b   == VVVBinaryOp p c d   =   o == p && a == c && b == d
  VVNBinaryOp o a b   == VVNBinaryOp p c d   =   o == p && a == c && b == d
  _                   == _                   =   False
-- ^ TODO: This needs to add more equality checks

-- instance Fractional Expr where
--   a / b = ArithBinaryOp Frac a b
--   fromRational r = ArithBinaryOp Frac (fromInteger $ numerator   r)
--                                       (fromInteger $ denominator r)
