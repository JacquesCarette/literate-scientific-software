{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
module Language.Drasil.Expr.Math where

import Prelude hiding (sqrt)
import Control.Lens ((^.))
import Language.Drasil.Symbol (Symbol)
import Language.Drasil.Expr (Expr(..), Relation, ($^), ($/), AssocArithOper(..),
  LABinOp(..), VVVBinOp(..), UFunc(..), UFuncB(..), UFuncVN(..), UFuncVV(..), Completeness(..), addRe)
import Language.Drasil.Space (RTopology(..), DomainDesc(..), RealInterval)
import Language.Drasil.Classes.Core (HasUID(uid), HasSymbol)
import Language.Drasil.Classes (IsArgumentName)

-- | Smart constructor for taking the absolute value of an expression.
abs_ :: Expr -> Expr
abs_ = UnaryOp Abs

-- | Smart constructor for negating an expression.
neg :: Expr -> Expr 
neg = UnaryOp Neg

-- | Smart constructor to take the log of an expression.
log :: Expr -> Expr
log = UnaryOp Log

-- | Smart constructor to take the ln of an expression.
ln :: Expr -> Expr
ln = UnaryOp Ln

-- | Smart constructor to take the square root of an expression.
sqrt :: Expr -> Expr
sqrt = UnaryOp Sqrt

-- | Smart constructor to apply sin to an expression.
sin :: Expr -> Expr
sin = UnaryOp Sin

-- | Smart constructor to apply cos to an expression.
cos :: Expr -> Expr 
cos = UnaryOp Cos

-- | Smart constructor to apply tan to an expression.
tan :: Expr -> Expr
tan = UnaryOp Tan

-- | Smart constructor to apply sec to an expression.
sec :: Expr -> Expr 
sec = UnaryOp Sec

-- | Smart constructor to apply csc to an expression.
csc :: Expr -> Expr
csc = UnaryOp Csc

-- | Smart constructor to apply cot to an expression.
cot :: Expr -> Expr 
cot = UnaryOp Cot

-- | Smart constructor to apply arcsin to an expression.
arcsin :: Expr -> Expr 
arcsin = UnaryOp Arcsin

-- | Smart constructor to apply arccos to an expression.
arccos :: Expr -> Expr 
arccos = UnaryOp Arccos

-- | Smart constructor to apply arctan to an expression.
arctan :: Expr -> Expr 
arctan = UnaryOp Arctan

-- | Smart constructor for the exponential (base e) function.
exp :: Expr -> Expr
exp = UnaryOp Exp

-- | Smart constructor for calculating the dimension of a vector.
dim :: Expr -> Expr
dim = UnaryOpVN Dim

-- | Smart constructor for calculating the normal form of a vector.
norm :: Expr -> Expr
norm = UnaryOpVN Norm

-- | Smart constructor for negating vectors.
negVec :: Expr -> Expr
negVec = UnaryOpVV NegV

-- | Smart constructor for applying logical negation to an expression.
not_ :: Expr -> Expr
not_ = UnaryOpB Not

-- | Smart constructor for indexing.
idx :: Expr -> Expr -> Expr
idx = LABinaryOp Index

-- | Smart constructor for integers.
int :: Integer -> Expr
int = Int

-- | Smart constructor for doubles.
dbl :: Double -> Expr
dbl = Dbl

-- | Smart constructor for exact doubles.
exactDbl :: Integer -> Expr
exactDbl = ExactDbl

-- | Smart constructor for fractions.
frac :: Integer -> Integer -> Expr
frac l r = exactDbl l $/ exactDbl r

-- | Smart constructor for rational expressions (only in 1/x form).
recip_ :: Expr -> Expr
recip_ denom = exactDbl 1 $/ denom

-- | Smart constructor for strings.
str :: String -> Expr
str = Str

-- | Smart constructors for percents.
perc :: Integer -> Integer -> Expr
perc = Perc

-- | Smart constructor for the summation, product, and integral functions over an interval.
defint, defsum, defprod :: Symbol -> Expr -> Expr -> Expr -> Expr
-- | Smart constructor for the summation, product, and integral functions over all Real numbers.
intAll, sumAll, prodAll :: Symbol -> Expr -> Expr

defint v low high = Operator AddRe (BoundedDD v Continuous low high)
intAll v = Operator AddRe (AllDD v Continuous)

defsum v low high = Operator AddRe (BoundedDD v Discrete low high)
sumAll v = Operator AddRe (AllDD v Discrete)

defprod v low high = Operator MulRe (BoundedDD v Discrete low high)
prodAll v = Operator MulRe (AllDD v Discrete)
-- TODO: Above only does for Reals

-- | Smart constructor for 'real interval' membership.
realInterval :: HasUID c => c -> RealInterval Expr Expr -> Expr
realInterval c = RealI (c ^. uid)

-- | Euclidean function : takes a vector and returns the sqrt of the sum-of-squares.
euclidean :: [Expr] -> Expr
euclidean = sqrt . foldr1 addRe . map square

{-# ANN sum' "HLint: ignore Use sum" #-}
-- | Used by 'euclidean' function (in place of 'sum') to fix representation of computation.
sum' :: (Num a, Foldable t) => t a -> a
sum' = foldr1 (+)
  
-- | Smart constructor to cross product two expressions.
cross :: Expr -> Expr -> Expr
cross = VVVBinaryOp Cross

-- | Smart constructor for case statement with complete set of cases.
completeCase :: [(Expr,Relation)] -> Expr
completeCase = Case Complete

-- | Smart constructor for case statement with incomplete set of cases.
incompleteCase :: [(Expr,Relation)] -> Expr
incompleteCase = Case Incomplete

-- | Smart constructor to square a function.
square :: Expr -> Expr
square x = x $^ exactDbl 2

-- | Smart constructor to half a function exactly.
half :: Expr -> Expr
half x = x $/ exactDbl 2

-- | Constructs 1/2.
oneHalf :: Expr
oneHalf = frac 1 2

-- | Constructs 1/3.
oneThird :: Expr
oneThird = frac 1 3

-- | Matrix helper function.
m2x2 :: Expr -> Expr -> Expr -> Expr -> Expr
m2x2 a b c d = Matrix [[a,b],[c,d]]

-- | Matrix helper function.
vec2D :: Expr -> Expr -> Expr
vec2D a b    = Matrix [[a],[b]]

-- | Matrix helper function.
dgnl2x2 :: Expr -> Expr -> Expr
dgnl2x2 a  = m2x2 a (Int 0) (Int 0)

-- Some helper functions to do function application

-- FIXME: These constructors should check that the UID is associated with a
-- chunk that is actually callable.
-- | Applies a given function with a list of parameters.
apply :: (HasUID f, HasSymbol f) => f -> [Expr] -> Expr
apply f ps = FCall (f ^. uid) ps []

-- | Similar to 'apply', but converts second argument into 'Symbol's.
apply1 :: (HasUID f, HasSymbol f, HasUID a, HasSymbol a) => f -> a -> Expr
apply1 f a = FCall (f ^. uid) [sy a] []

-- | Similar to 'apply', but the applied function takes two parameters (which are both 'Symbol's).
apply2 :: (HasUID f, HasSymbol f, HasUID a, HasSymbol a, HasUID b, HasSymbol b) 
  => f -> a -> b -> Expr
apply2 f a b = FCall (f ^. uid) [sy a, sy b] []

-- | Similar to 'apply', but takes a relation to apply to 'FCall'.
applyWithNamedArgs :: (HasUID f, HasSymbol f, HasUID a, IsArgumentName a) => f 
  -> [Expr] -> [(a, Expr)] -> Expr
applyWithNamedArgs f ps ns = FCall (f ^. uid) ps (zip (map ((^. uid) . fst) ns) 
  (map snd ns))

-- Note how |sy| 'enforces' having a symbol
-- | Get an 'Expr' from a 'Symbol'.
sy :: (HasUID c, HasSymbol c) => c -> Expr
sy x = C (x ^. uid)
