{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Language.Drasil.DisplayExpr where

import Prelude hiding (sqrt)

import Control.Lens ((^.))

import Language.Drasil.Expr (Completeness(..))
import Language.Drasil.Display.Expr
import Language.Drasil.DisplayClasses (Display(..))
import Language.Drasil.Symbol (Symbol)
import Language.Drasil.Space (Space, RTopology(..), DomainDesc(..), RealInterval)
import Language.Drasil.Classes.Core (HasUID(uid), HasSymbol)
import Language.Drasil.Classes (IsArgumentName)



-- | One expression is "defined" by another.
defines :: (Display a, Display b) => a -> b -> DisplayExpr
defines a b = BinaryOp Defines (toDispExpr a) (toDispExpr b)

-- | Bring a Space into the DisplayExpr.
space :: Space -> DisplayExpr
space = SpaceExpr

isIn :: Display a => a -> DisplayExpr -> DisplayExpr
isIn a s@(SpaceExpr _) = BinaryOp IsIn (toDispExpr a) s
isIn _ _               = error "isIn target must be a Space"

-- | Helper for creating new smart constructors for Associative Binary
--   operations that require at least 1 expression.
assocCreate :: Display d => AssocBinOp -> [d] -> DisplayExpr
assocCreate b [] = error $ "Need at least 1 expression to create associative " ++ show b
assocCreate _ [x] = toDispExpr x
assocCreate b des = Assoc b $ assocSanitize b $ map toDispExpr des

-- | Helper for associative operations, removes embedded variants of the same kind
assocSanitize :: AssocBinOp -> [DisplayExpr] -> [DisplayExpr]
assocSanitize _ [] = []
assocSanitize b (it@(Assoc c des):r)
  | b == c    = assocSanitize b des ++ assocSanitize b r
  | otherwise = it : assocSanitize b r
assocSanitize b (de:des) = de : assocSanitize b des

-- | Binary associative "And".
and :: Display d => [d] -> DisplayExpr
and = assocCreate And

-- | Binary associative "Equivalence".
equiv :: Display a => [a] -> DisplayExpr
equiv des
  | length des >= 2 = assocCreate Equivalence des
  | otherwise       = error $ "Need at least 2 expressions to create " ++ show Equivalence


($=), ($!=) :: DisplayExpr -> DisplayExpr -> DisplayExpr
-- | Smart constructor for equating two expressions.
($=)  = BinaryOp Eq
-- | Smart constructor for showing that two expressions are not equal.
($!=) = BinaryOp NEq

-- | Smart constructor for ordering two equations.
($<), ($>), ($<=), ($>=) :: DisplayExpr -> DisplayExpr -> DisplayExpr
-- | Less than.
($<)  = BinaryOp Lt
-- | Greater than.
($>)  = BinaryOp Gt
-- | Less than or equal to.
($<=) = BinaryOp LEq
-- | Greater than or equal to.
($>=) = BinaryOp GEq

-- | Smart constructor for the dot product of two equations.
($.) :: DisplayExpr -> DisplayExpr -> DisplayExpr
($.) = BinaryOp Dot

-- Generate below 4 functions with TH?
-- | Add two expressions (Integers).
add :: DisplayExpr -> DisplayExpr -> DisplayExpr
add l (Int 0) = l
add (Int 0) r = r
add l (Dbl 0)      = l
add (Dbl 0) r      = r
add l (ExactDbl 0) = l
add (ExactDbl 0) r = r
add (Assoc Add l) (Assoc Add r) = Assoc Add (l ++ r)
add (Assoc Add l) r = Assoc Add (l ++ [r])
add l (Assoc Add r) = Assoc Add (l : r)
add l r = Assoc Add [l, r]

-- | Multiply two expressions.
mul :: DisplayExpr -> DisplayExpr -> DisplayExpr
mul l (Int 1) = l
mul (Int 1) r = r
mul l (Dbl 1)      = l
mul (Dbl 1) r      = r
mul l (ExactDbl 1) = l
mul (ExactDbl 1) r = r
mul (Assoc Mul l) (Assoc Mul r) = Assoc Mul (l ++ r)
mul (Assoc Mul l) r = Assoc Mul (l ++ [r])
mul l (Assoc Mul r) = Assoc Mul (l : r)
mul l r = Assoc Mul [l, r]


($-), ($/), ($^) :: DisplayExpr -> DisplayExpr -> DisplayExpr
-- | Smart constructor for subtracting two expressions.
($-) = BinaryOp Subt
-- | Smart constructor for dividing two expressions.
($/) = BinaryOp Frac
-- | Smart constructor for rasing the first expression to the power of the second.
($^) = BinaryOp Pow

($=>), ($<=>) :: DisplayExpr -> DisplayExpr -> DisplayExpr
-- | Smart constructor to show that one expression implies the other (conditional operator).
($=>)  = BinaryOp Impl
-- | Smart constructor to show that an expression exists if and only if another expression exists (biconditional operator).
($<=>) = BinaryOp Iff

($&&), ($||) :: DisplayExpr -> DisplayExpr -> DisplayExpr
-- | Smart constructor for the boolean /and/ operator.
a $&& b = Assoc And [a, b]
-- | Smart constructor for the boolean /or/ operator.
a $|| b = Assoc Or  [a, b]
-- TODO: Other Assoc Bin ops


-- | Smart constructor for taking the absolute value of an expression.
abs_ :: DisplayExpr -> DisplayExpr
abs_ = UnaryOp Abs

-- | Smart constructor for negating an expression.
neg :: DisplayExpr -> DisplayExpr 
neg = UnaryOp Neg

-- | Smart constructor to take the log of an expression.
log :: DisplayExpr -> DisplayExpr
log = UnaryOp Log

-- | Smart constructor to take the ln of an expression.
ln :: DisplayExpr -> DisplayExpr
ln = UnaryOp Ln

-- | Smart constructor to take the square root of an expression.
sqrt :: DisplayExpr -> DisplayExpr
sqrt = UnaryOp Sqrt

-- | Smart constructor to apply sin to an expression.
sin :: DisplayExpr -> DisplayExpr
sin = UnaryOp Sin

-- | Smart constructor to apply cos to an expression.
cos :: DisplayExpr -> DisplayExpr 
cos = UnaryOp Cos

-- | Smart constructor to apply tan to an expression.
tan :: DisplayExpr -> DisplayExpr
tan = UnaryOp Tan

-- | Smart constructor to apply sec to an expression.
sec :: DisplayExpr -> DisplayExpr 
sec = UnaryOp Sec

-- | Smart constructor to apply csc to an expression.
csc :: DisplayExpr -> DisplayExpr
csc = UnaryOp Csc

-- | Smart constructor to apply cot to an expression.
cot :: DisplayExpr -> DisplayExpr 
cot = UnaryOp Cot

-- | Smart constructor to apply arcsin to an expression.
arcsin :: DisplayExpr -> DisplayExpr 
arcsin = UnaryOp Arcsin

-- | Smart constructor to apply arccos to an expression.
arccos :: DisplayExpr -> DisplayExpr 
arccos = UnaryOp Arccos

-- | Smart constructor to apply arctan to an expression.
arctan :: DisplayExpr -> DisplayExpr 
arctan = UnaryOp Arctan

-- | Smart constructor for the exponential (base e) function.
exp :: DisplayExpr -> DisplayExpr
exp = UnaryOp Exp

-- | Smart constructor for calculating the dimension of a vector.
dim :: DisplayExpr -> DisplayExpr
dim = UnaryOp Dim

-- | Smart constructor for calculating the normal form of a vector.
norm :: DisplayExpr -> DisplayExpr
norm = UnaryOp Norm

-- | Smart constructor for applying logical negation to an expression.
not_ :: DisplayExpr -> DisplayExpr
not_ = UnaryOp Not

-- | Smart constructor for indexing.
idx :: DisplayExpr -> DisplayExpr -> DisplayExpr
idx = BinaryOp Index

-- | Smart constructor for integers.
int :: Integer -> DisplayExpr
int = Int

-- | Smart constructor for doubles.
dbl :: Double -> DisplayExpr
dbl = Dbl

-- | Smart constructor for exact doubles.
exactDbl :: Integer -> DisplayExpr
exactDbl = ExactDbl

-- | Smart constructor for fractions.
frac :: Integer -> Integer -> DisplayExpr
frac l r = exactDbl l $/ exactDbl r

-- | Smart constructor for rational expressions (only in 1/x form).
recip_ :: DisplayExpr -> DisplayExpr
recip_ denom = exactDbl 1 $/ denom

-- | Smart constructor for strings.
str :: String -> DisplayExpr
str = Str

-- | Smart constructors for percents.
perc :: Integer -> Integer -> DisplayExpr
perc = Perc

-- | Smart constructor for the summation, product, and integral functions over an interval.
defint, defsum, defprod :: Symbol -> DisplayExpr -> DisplayExpr -> DisplayExpr -> DisplayExpr
-- | Smart constructor for the summation, product, and integral functions over all Real numbers.
intAll, sumAll, prodAll :: Symbol -> DisplayExpr -> DisplayExpr

defint v low high = Operator Add (BoundedDD v Continuous low high)
intAll v = Operator Add (AllDD v Continuous)

defsum v low high = Operator Add (BoundedDD v Discrete low high)
sumAll v = Operator Add (AllDD v Discrete)

defprod v low high = Operator Mul (BoundedDD v Discrete low high)
prodAll v = Operator Mul (AllDD v Discrete)
-- TODO: Above only does for Reals

-- | Smart constructor for 'real interval' membership.
realInterval :: HasUID c => c -> RealInterval DisplayExpr DisplayExpr -> DisplayExpr
realInterval c = RealI (c ^. uid)

-- | Euclidean function : takes a vector and returns the sqrt of the sum-of-squares.
euclidean :: [DisplayExpr] -> DisplayExpr
euclidean = sqrt . foldr1 add . map square

{-# ANN sum' "HLint: ignore Use sum" #-}
-- | Used by 'euclidean' function (in place of 'sum') to fix representation of computation.
sum' :: (Num a, Foldable t) => t a -> a
sum' = foldr1 (+)
  
-- | Smart constructor to cross product two expressions.
cross :: DisplayExpr -> DisplayExpr -> DisplayExpr
cross = BinaryOp Cross

-- | Smart constructor for case statement with complete set of cases.
completeCase :: [(DisplayExpr, DisplayExpr)] -> DisplayExpr
completeCase = Case Complete

-- | Smart constructor for case statement with incomplete set of cases.
incompleteCase :: [(DisplayExpr, DisplayExpr)] -> DisplayExpr
incompleteCase = Case Incomplete

-- | Smart constructor to square a function.
square :: DisplayExpr -> DisplayExpr
square x = x $^ exactDbl 2

-- | Smart constructor to half a function exactly.
half :: DisplayExpr -> DisplayExpr
half x = x $/ exactDbl 2

-- | Constructs 1/2.
oneHalf :: DisplayExpr
oneHalf = frac 1 2

-- | Constructs 1/3.
oneThird :: DisplayExpr
oneThird = frac 1 3

-- | Matrix helper function.
m2x2 :: DisplayExpr -> DisplayExpr -> DisplayExpr -> DisplayExpr -> DisplayExpr
m2x2 a b c d = Matrix [[a,b],[c,d]]

-- | Matrix helper function.
vec2D :: DisplayExpr -> DisplayExpr -> DisplayExpr
vec2D a b    = Matrix [[a],[b]]

-- | Matrix helper function.
dgnl2x2 :: DisplayExpr -> DisplayExpr -> DisplayExpr
dgnl2x2 a  = m2x2 a (Int 0) (Int 0)

-- Some helper functions to do function application

-- FIXME: These constructors should check that the UID is associated with a
-- chunk that is actually callable.
-- | Applies a given function with a list of parameters.
apply :: (HasUID f, HasSymbol f) => f -> [DisplayExpr] -> DisplayExpr
apply f ps = FCall (f ^. uid) ps []

-- | Similar to 'apply', but converts second argument into 'Symbol's.
apply1 :: (HasUID f, HasSymbol f, HasUID a, HasSymbol a) => f -> a -> DisplayExpr
apply1 f a = FCall (f ^. uid) [sy a] []

-- | Similar to 'apply', but the applied function takes two parameters (which are both 'Symbol's).
apply2 :: (HasUID f, HasSymbol f, HasUID a, HasSymbol a, HasUID b, HasSymbol b) 
  => f -> a -> b -> DisplayExpr
apply2 f a b = FCall (f ^. uid) [sy a, sy b] []

-- | Similar to 'apply', but takes a relation to apply to 'FCall'.
applyWithNamedArgs :: (HasUID f, HasSymbol f, HasUID a, IsArgumentName a) => f 
  -> [DisplayExpr] -> [(a, DisplayExpr)] -> DisplayExpr
applyWithNamedArgs f ps ns = FCall (f ^. uid) ps (zip (map ((^. uid) . fst) ns) 
  (map snd ns))

-- Note how |sy| 'enforces' having a symbol
-- | Get an 'DisplayExpr' from a 'Symbol'.
sy :: (HasUID c, HasSymbol c) => c -> DisplayExpr
sy x = C (x ^. uid)
