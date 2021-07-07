module Language.Drasil.Code.Expr.Convert (
    expr, realInterval, constraint
) where

import qualified Language.Drasil as L
import qualified Language.Drasil.Development as LD

import Data.Bifunctor (Bifunctor(bimap, second))

import Language.Drasil.Code.Expr

-- | Render an algebraic expression into our code expression language
expr :: L.Expr -> CodeExpr
expr (L.Dbl d) = Dbl d
expr (L.Int i) = Int i
expr (L.ExactDbl i) = ExactDbl i 
expr (L.Str s) = Str s
expr (L.Perc n d) = Perc n d
expr (L.AssocA ao es) = AssocA (assocArithOp ao) $ map expr es
expr (L.AssocB bo es) = AssocB (assocBoolOp bo) $ map expr es
expr (L.C u) = C u
expr (L.FCall u es ns) = FCall u (map expr es) (map (second expr) ns)
expr (L.Case c es) = Case c $ map (bimap expr expr) es
expr (L.Matrix es) = Matrix $ map (map expr) es
expr (L.UnaryOp uo e) = UnaryOp (uFunc uo) (expr e)
expr (L.UnaryOpB uo e) = UnaryOpB (uFuncB uo) (expr e)
expr (L.UnaryOpVV uo e) = UnaryOpVV (uFuncVV uo) (expr e)
expr (L.UnaryOpVN uo e) = UnaryOpVN (uFuncVN uo) (expr e)
expr (L.ArithBinaryOp bo l r) = ArithBinaryOp (arithBinOp bo) (expr l) (expr r)
expr (L.BoolBinaryOp bo l r) = BoolBinaryOp (boolBinOp bo) (expr l) (expr r)
expr (L.EqBinaryOp bo l r) = EqBinaryOp (eqBinOp bo) (expr l) (expr r)
expr (L.LABinaryOp bo l r) = LABinaryOp (laBinOp bo) (expr l) (expr r)
expr (L.OrdBinaryOp bo l r) = OrdBinaryOp (ordBinOp bo) (expr l) (expr r)
expr (L.VVVBinaryOp bo l r) = VVVBinaryOp (vvvBinOp bo) (expr l) (expr r)
expr (L.VVNBinaryOp bo l r) = VVNBinaryOp (vvnBinOp bo) (expr l) (expr r)
expr (L.Operator aao dd e) = Operator (assocArithOp aao) (renderDomainDesc dd) (expr e)
expr (L.RealI u ri) = RealI u (realInterval ri)

-- | Convert 'RealInterval Expr Expr's into 'RealInterval CodeExpr CodeExpr's.
realInterval :: L.RealInterval L.Expr L.Expr -> L.RealInterval CodeExpr CodeExpr
realInterval (L.Bounded (il, el) (ir, er)) = L.Bounded (il, expr el) (ir, expr er)
realInterval (L.UpTo (i, e)) = L.UpTo (i, expr e)
realInterval (L.UpFrom (i, e)) = L.UpFrom (i, expr e)

-- | Convert 'Constraint Expr's into 'Constraint CodeExpr's.
constraint :: L.ConstraintE -> L.Constraint CodeExpr
constraint (L.Range r ri) = L.Range r (realInterval ri)

-- | Convert 'DomainDesc Expr Expr' into 'DomainDesc CodeExpr CodeExpr's.
renderDomainDesc :: L.DomainDesc L.Expr L.Expr -> L.DomainDesc CodeExpr CodeExpr
renderDomainDesc (L.BoundedDD s t l r) = L.BoundedDD s t (expr l) (expr r)
renderDomainDesc (L.AllDD s t) = L.AllDD s t

arithBinOp :: L.ArithBinOp -> ArithBinOp
arithBinOp LD.Frac = Frac
arithBinOp LD.Pow = Pow
arithBinOp LD.Subt = Subt

eqBinOp :: L.EqBinOp -> EqBinOp
eqBinOp LD.Eq = Eq
eqBinOp LD.NEq = NEq

boolBinOp :: L.BoolBinOp -> BoolBinOp
boolBinOp LD.Impl = Impl
boolBinOp LD.Iff = Iff

laBinOp :: L.LABinOp -> LABinOp
laBinOp LD.Index = Index

ordBinOp :: L.OrdBinOp -> OrdBinOp
ordBinOp LD.Lt  = Lt
ordBinOp LD.Gt  = Gt
ordBinOp LD.LEq = LEq
ordBinOp LD.GEq = GEq

vvvBinOp :: L.VVVBinOp -> VVVBinOp
vvvBinOp LD.Cross = Cross

vvnBinOp :: L.VVNBinOp -> VVNBinOp
vvnBinOp LD.Dot = Dot

assocArithOp :: L.AssocArithOper -> AssocArithOper
assocArithOp L.AddI = AddI -- TODO: These L.'s should be exported through L.D.Development
assocArithOp L.AddRe = AddRe
assocArithOp L.MulI = MulI
assocArithOp L.MulRe = MulRe

assocBoolOp :: L.AssocBoolOper -> AssocBoolOper
assocBoolOp L.And = And -- TODO: These L.'s should be exported through L.D.Development
assocBoolOp L.Or = Or

uFunc :: L.UFunc -> UFunc
uFunc L.Abs = Abs -- TODO: These L.'s should be exported through L.D.Development
uFunc L.Log = Log
uFunc L.Ln = Ln 
uFunc L.Sin = Sin 
uFunc L.Cos = Cos
uFunc L.Tan = Tan
uFunc L.Sec = Sec
uFunc L.Csc = Csc
uFunc L.Cot = Cot
uFunc L.Arcsin = Arcsin
uFunc L.Arccos = Arccos
uFunc L.Arctan = Arctan 
uFunc L.Exp = Exp 
uFunc L.Sqrt = Sqrt 
uFunc L.Neg = Neg 

uFuncB :: L.UFuncB -> UFuncB
uFuncB LD.Not = Not

uFuncVV :: L.UFuncVV -> UFuncVV
uFuncVV LD.NegV = NegV

uFuncVN :: L.UFuncVN -> UFuncVN
uFuncVN LD.Norm = Norm
uFuncVN LD.Dim = Dim
