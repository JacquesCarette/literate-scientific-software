module Language.Drasil.Display.Precedence (dePrec, dePrecAssoc) where

import Language.Drasil.Display.Expr

-- These precedences are inspired from Haskell/F# 
-- as documented at http://kevincantu.org/code/operators.html
-- They are all multiplied by 10, to leave room to weave things in between

-- | dePrec2- precedence for binary operations.
dePrec2 :: BinOp -> Int
dePrec2 Frac = 190
dePrec2 Pow = 200
dePrec2 Subt = 180
dePrec2 Impl = 130
dePrec2 Iff = 130
dePrec2 Eq  = 130
dePrec2 NEq = 130
dePrec2 Index = 250
dePrec2 Lt = 130
dePrec2 Gt = 130
dePrec2 LEq = 130
dePrec2 GEq = 130
dePrec2 Cross = 190
dePrec2 Dot = 190
dePrec2 IsIn = 170
dePrec2 Defines = 130

-- | dePrec2- precedence for binary operations.
dePrecAssoc :: AssocBinOp -> Int
dePrecAssoc Add = 180
dePrecAssoc Mul = 190
dePrecAssoc And = 120
dePrecAssoc Or = 110
dePrecAssoc Equivalence = 120

-- | prec1 - precedence of unary operators.
dePrec1 :: UOp -> Int
dePrec1 Abs = 250
dePrec1 Log = 250
dePrec1 Ln = 250
dePrec1 Sin = 250
dePrec1 Cos = 250
dePrec1 Tan = 250
dePrec1 Sec = 250
dePrec1 Csc = 250
dePrec1 Cot = 250
dePrec1 Arcsin = 250
dePrec1 Arccos = 250
dePrec1 Arctan = 250
dePrec1 Sqrt = 250
dePrec1 Neg = 230
dePrec1 Not = 230
dePrec1 Norm = 230
dePrec1 Dim = 230
dePrec1 Exp = 200

-- | eprec - "Expression" precedence.
dePrec :: DisplayExpr -> Int
dePrec Int{}             = 500
dePrec Dbl{}             = 500
dePrec ExactDbl{}        = 500
dePrec Str{}             = 500
dePrec Perc{}            = 500
dePrec (Assoc op _)      = dePrecAssoc op
dePrec C{}               = 500
dePrec Deriv{}           = dePrec2 Frac
dePrec FCall{}           = 210
dePrec Case{}            = 200
dePrec Matrix{}          = 220
dePrec (UnaryOp fn _)    = dePrec1 fn
dePrec (Operator o _ _)  = dePrecAssoc o
dePrec (BinaryOp bo _ _) = dePrec2 bo
dePrec RealI{}                = 170
dePrec (SpaceExpr _)     = 170


