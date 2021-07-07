module Language.Drasil.Printing.Import.CodeExpr (codeExpr) where

import Language.Drasil (DomainDesc(..), Inclusive(..),
  RTopology(..), RealInterval(..), UID )
import Language.Drasil.Display (Symbol(..))
import Language.Drasil.CodeExpr (dbl)
import Language.Drasil.Code.Expr.Development

import qualified Language.Drasil.Printing.AST as P
import Language.Drasil.Printing.PrintingInformation (HasPrintingOptions(..),
  PrintingInformation, Notation(Scientific, Engineering), ckdb, stg)

import Language.Drasil.Printing.Import.Helpers
    (digitsProcess, lookupC, parens, processExpo)
import Language.Drasil.Printing.Import.Symbol (symbol)

import Control.Lens ((^.))
import Data.List (intersperse)
import Numeric (floatToDigits)


-- | Helper that creates an expression row given printing information, an operator, and an expression.
mkCall :: PrintingInformation -> P.Ops -> CodeExpr -> P.Expr
mkCall s o e = P.Row [P.MO o, parens $ codeExpr e s]

-- | Helper that creates a binary expression row given printing information, an operator, and two expressions.
mkBOp :: PrintingInformation -> P.Ops -> CodeExpr -> CodeExpr -> P.Expr
mkBOp sm o a b = P.Row [codeExpr a sm, P.MO o, codeExpr b sm]

-- | Helper that adds parenthesis to an expression where appropriate.
expr' :: PrintingInformation -> Int -> CodeExpr -> P.Expr
expr' s p e = fence $ codeExpr e s
  where fence = if eprec e < p then parens else id

-- | Helper for properly rendering negation of expressions.
neg' :: CodeExpr -> Bool
neg' (Dbl     _)            = True
neg' (Int     _)            = True
neg' Operator{}             = True
neg' (AssocA MulI _)        = True
neg' (AssocA MulRe _)       = True
neg' (LABinaryOp Index _ _) = True
neg' (UnaryOp _ _)          = True
neg' (UnaryOpB _ _)         = True
neg' (UnaryOpVV _ _)        = True
neg' (C _)                  = True
neg' _                      = False

-- | Render negated expressions.
neg :: PrintingInformation -> CodeExpr -> P.Expr
neg sm a = P.Row [P.MO P.Neg, (if neg' a then id else parens) $ codeExpr a sm]

-- | For printing indexes.
indx :: PrintingInformation -> CodeExpr -> CodeExpr -> P.Expr
indx sm (C c) i = f s
  where
    i' = codeExpr i sm
    s = lookupC (sm ^. stg) (sm ^. ckdb) c
    f (Corners [] [] [] [b] e) =
      let e' = symbol e
          b' = symbol b in
      P.Row [P.Row [e', P.Sub (P.Row [b', P.MO P.Comma, i'])]] -- FIXME, extra Row
    f a@(Variable _) = P.Row [symbol a, P.Sub i']
    f a@(Label _)    = P.Row [symbol a, P.Sub i']
--    f a@(Greek _)  = P.Row [symbol a, P.Sub i']
    f   e          = let e' = symbol e in P.Row [P.Row [e'], P.Sub i']
indx sm a i = P.Row [P.Row [codeExpr a sm], P.Sub $ codeExpr i sm]

-- | For printing expressions that call something.
call :: PrintingInformation -> UID -> [CodeExpr] -> [(UID, CodeExpr)] -> P.Expr
call sm f ps ns = P.Row [symbol $ lookupC (sm ^. stg) (sm ^. ckdb) f,
  parens $ P.Row $ intersperse (P.MO P.Comma) $ map (`codeExpr` sm) ps ++
  zipWith (\n a -> P.Row [symbol $ lookupC (sm ^. stg) (sm ^. ckdb) n,
  P.MO P.Eq, codeExpr a sm]) (map fst ns) (map snd ns)]

-- | Helper function for addition 'EOperator's.
eopAdds :: PrintingInformation -> DomainDesc CodeExpr CodeExpr -> CodeExpr -> P.Expr
eopAdds sm (BoundedDD v Continuous l h) e =
  P.Row [P.MO P.Inte, P.Sub (codeExpr l sm), P.Sup (codeExpr h sm),
         P.Row [codeExpr e sm], P.Spc P.Thin, P.Ident "d", symbol v]
eopAdds sm (AllDD v Continuous) e =
  P.Row [P.MO P.Inte, P.Sub (symbol v), P.Row [codeExpr e sm], P.Spc P.Thin,
         P.Ident "d", symbol v]
eopAdds sm (BoundedDD v Discrete l h) e =
  P.Row [P.MO P.Summ, P.Sub (P.Row [symbol v, P.MO P.Eq, codeExpr l sm]), P.Sup (codeExpr h sm),
         P.Row [codeExpr e sm]]
eopAdds sm (AllDD _ Discrete) e = P.Row [P.MO P.Summ, P.Row [codeExpr e sm]]

-- | Helper function for multiplicative 'EOperator's.
eopMuls :: PrintingInformation -> DomainDesc CodeExpr CodeExpr -> CodeExpr -> P.Expr
eopMuls sm (BoundedDD v Discrete l h) e =
  P.Row [P.MO P.Prod, P.Sub (P.Row [symbol v, P.MO P.Eq, codeExpr l sm]), P.Sup (codeExpr h sm),
         P.Row [codeExpr e sm]]
eopMuls sm (AllDD _ Discrete) e = P.Row [P.MO P.Prod, P.Row [codeExpr e sm]]
eopMuls _ (AllDD _ Continuous) _ = error "Printing/Import.hs Product-Integral not implemented."
eopMuls _ (BoundedDD _ Continuous _ _) _ = error "Printing/Import.hs Product-Integral not implemented."


-- | Helper function for translating 'EOperator's.
eop :: PrintingInformation -> AssocArithOper -> DomainDesc CodeExpr CodeExpr -> CodeExpr -> P.Expr
eop sm AddI = eopAdds sm
eop sm AddRe = eopAdds sm
eop sm MulI = eopMuls sm
eop sm MulRe = eopMuls sm

-- | Translate Exprs to printable layout AST.
codeExpr :: CodeExpr -> PrintingInformation -> P.Expr
codeExpr (Dbl d)                  sm = case sm ^. getSetting of
  Engineering -> P.Row $ digitsProcess (map toInteger $ fst $ floatToDigits 10 d)
     (fst $ processExpo $ snd $ floatToDigits 10 d) 0
     (toInteger $ snd $ processExpo $ snd $ floatToDigits 10 d)
  Scientific  ->  P.Dbl d
codeExpr (Int i)                   _ = P.Int i
codeExpr (ExactDbl d)              _ = P.Int d
codeExpr (Str s)                   _ = P.Str s
codeExpr (Perc a b)               sm = P.Row [codeExpr (dbl val) sm, P.MO P.Perc]
  where
    val = fromIntegral a / (10 ** fromIntegral (b - 2))
codeExpr (AssocB And l)           sm = assocExpr P.And (precB And) l sm
codeExpr (AssocB Or l)            sm = assocExpr P.Or (precB Or) l sm
codeExpr (AssocA AddI l)          sm = assocExpr P.Add (precA AddI) l sm
codeExpr (AssocA AddRe l)         sm = assocExpr P.Add (precA AddRe) l sm
codeExpr (AssocA MulI l)          sm = P.Row $ mulExpr l MulI sm
codeExpr (AssocA MulRe l)         sm = P.Row $ mulExpr l MulRe sm
codeExpr (C c)                    sm = symbol $ lookupC (sm ^. stg) (sm ^. ckdb) c
codeExpr (FCall f [x] [])         sm =
  P.Row [symbol $ lookupC (sm ^. stg) (sm ^. ckdb) f, parens $ codeExpr x sm]
codeExpr (FCall f l ns)           sm = call sm f l ns
codeExpr (New c l ns)             sm = call sm c l ns
codeExpr (Message a m l ns)       sm =
  P.Row [symbol $ lookupC (sm ^. stg) (sm ^. ckdb) a, P.MO P.Point, call sm m l ns]
codeExpr (Field o f)              sm = P.Row [symbol $ lookupC (sm ^. stg) (sm ^. ckdb) o,
  P.MO P.Point, symbol $ lookupC (sm ^. stg) (sm ^. ckdb) f]
codeExpr (Case _ ps)              sm =
  if length ps < 2
    then error "Attempting to use multi-case codeExpr incorrectly"
    else P.Case (zip (map (flip codeExpr sm . fst) ps) (map (flip codeExpr sm . snd) ps))
codeExpr (Matrix a)               sm = P.Mtx $ map (map (`codeExpr` sm)) a
codeExpr (UnaryOp Log u)          sm = mkCall sm P.Log u
codeExpr (UnaryOp Ln u)           sm = mkCall sm P.Ln u
codeExpr (UnaryOp Sin u)          sm = mkCall sm P.Sin u
codeExpr (UnaryOp Cos u)          sm = mkCall sm P.Cos u
codeExpr (UnaryOp Tan u)          sm = mkCall sm P.Tan u
codeExpr (UnaryOp Sec u)          sm = mkCall sm P.Sec u
codeExpr (UnaryOp Csc u)          sm = mkCall sm P.Csc u
codeExpr (UnaryOp Cot u)          sm = mkCall sm P.Cot u
codeExpr (UnaryOp Arcsin u)       sm = mkCall sm P.Arcsin u
codeExpr (UnaryOp Arccos u)       sm = mkCall sm P.Arccos u
codeExpr (UnaryOp Arctan u)       sm = mkCall sm P.Arctan u
codeExpr (UnaryOp Exp u)          sm = P.Row [P.MO P.Exp, P.Sup $ codeExpr u sm]
codeExpr (UnaryOp Abs u)          sm = P.Fenced P.Abs P.Abs $ codeExpr u sm
codeExpr (UnaryOpB Not u)         sm = P.Row [P.MO P.Not, codeExpr u sm]
codeExpr (UnaryOpVN Norm u)       sm = P.Fenced P.Norm P.Norm $ codeExpr u sm
codeExpr (UnaryOpVN Dim u)        sm = mkCall sm P.Dim u
codeExpr (UnaryOp Sqrt u)         sm = P.Sqrt $ codeExpr u sm
codeExpr (UnaryOp Neg u)          sm = neg sm u
codeExpr (UnaryOpVV NegV u)       sm = neg sm u
codeExpr (ArithBinaryOp Frac a b) sm = P.Div (codeExpr a sm) (codeExpr b sm)
codeExpr (ArithBinaryOp Pow a b)  sm = pow sm a b
codeExpr (ArithBinaryOp Subt a b) sm = P.Row [codeExpr a sm, P.MO P.Subt, codeExpr b sm]
codeExpr (BoolBinaryOp Impl a b)  sm = mkBOp sm P.Impl a b
codeExpr (BoolBinaryOp Iff a b)   sm = mkBOp sm P.Iff a b
codeExpr (EqBinaryOp Eq a b)      sm = mkBOp sm P.Eq a b
codeExpr (EqBinaryOp NEq a b)     sm = mkBOp sm P.NEq a b
codeExpr (LABinaryOp Index a b)   sm = indx sm a b
codeExpr (OrdBinaryOp Lt a b)     sm = mkBOp sm P.Lt a b
codeExpr (OrdBinaryOp Gt a b)     sm = mkBOp sm P.Gt a b
codeExpr (OrdBinaryOp LEq a b)    sm = mkBOp sm P.LEq a b
codeExpr (OrdBinaryOp GEq a b)    sm = mkBOp sm P.GEq a b
codeExpr (VVNBinaryOp Dot a b)    sm = mkBOp sm P.Dot a b
codeExpr (VVVBinaryOp Cross a b)  sm = mkBOp sm P.Cross a b
codeExpr (Operator o d e)         sm = eop sm o d e
codeExpr (RealI c ri)             sm = renderRealInt sm (lookupC (sm ^. stg)
  (sm ^. ckdb) c) ri

-- | Common method of converting associative operations into printable layout AST.
assocExpr :: P.Ops -> Int -> [CodeExpr] -> PrintingInformation -> P.Expr
assocExpr op prec exprs sm = P.Row $ intersperse (P.MO op) $ map (expr' sm prec) exprs

-- | Helper for rendering printable expressions.
mulExpr ::  [CodeExpr] -> AssocArithOper -> PrintingInformation -> [P.Expr]
mulExpr (hd1:hd2:tl) o sm = case (hd1, hd2) of
  (a, Int _)      ->  [expr' sm (precA o) a, P.MO P.Dot] ++ mulExpr (hd2 : tl) o sm
  (a, ExactDbl _) ->  [expr' sm (precA o) a, P.MO P.Dot] ++ mulExpr (hd2 : tl) o sm
  (a, Dbl _)      ->  [expr' sm (precA o) a, P.MO P.Dot] ++ mulExpr (hd2 : tl) o sm
  (a, _)          ->  [expr' sm (precA o) a, P.MO P.Mul] ++ mulExpr (hd2 : tl) o sm
mulExpr [hd]         o sm = [expr' sm (precA o) hd]
mulExpr []           o sm = [expr' sm (precA o) (Int 1)]


-- | Helper that adds parenthesis to the first expression. The second expression
-- is written as a superscript attached to the first.
withParens :: PrintingInformation -> CodeExpr -> CodeExpr -> P.Expr
withParens prI a b = P.Row [parens (codeExpr a prI), P.Sup (codeExpr b prI)]

-- | Helper for properly rendering exponents.
pow :: PrintingInformation -> CodeExpr -> CodeExpr -> P.Expr
pow prI a@(AssocA AddI _)          b = withParens prI a b
pow prI a@(AssocA AddRe _)         b = withParens prI a b
pow prI a@(AssocA MulI _)          b = withParens prI a b
pow prI a@(AssocA MulRe _)         b = withParens prI a b
pow prI a@(ArithBinaryOp Subt _ _) b = withParens prI a b
pow prI a@(ArithBinaryOp Frac _ _) b = withParens prI a b
pow prI a@(ArithBinaryOp Pow _ _)  b = withParens prI a b
pow prI a                          b = P.Row [codeExpr a prI, P.Sup (codeExpr b prI)]

-- | Print a 'RealInterval'.
renderRealInt :: PrintingInformation -> Symbol -> RealInterval CodeExpr CodeExpr -> P.Expr
renderRealInt st s (Bounded (Inc,a) (Inc,b)) =
  P.Row [codeExpr a st, P.MO P.LEq, symbol s, P.MO P.LEq, codeExpr b st]
renderRealInt st s (Bounded (Inc,a) (Exc,b)) =
  P.Row [codeExpr a st, P.MO P.LEq, symbol s, P.MO P.Lt, codeExpr b st]
renderRealInt st s (Bounded (Exc,a) (Inc,b)) =
  P.Row [codeExpr a st, P.MO P.Lt, symbol s, P.MO P.LEq, codeExpr b st]
renderRealInt st s (Bounded (Exc,a) (Exc,b)) =
  P.Row [codeExpr a st, P.MO P.Lt, symbol s, P.MO P.Lt, codeExpr b st]
renderRealInt st s (UpTo (Inc,a))   = P.Row [symbol s, P.MO P.LEq, codeExpr a st]
renderRealInt st s (UpTo (Exc,a))   = P.Row [symbol s, P.MO P.Lt,  codeExpr a st]
renderRealInt st s (UpFrom (Inc,a)) = P.Row [symbol s, P.MO P.GEq, codeExpr a st]
renderRealInt st s (UpFrom (Exc,a)) = P.Row [symbol s, P.MO P.Gt,  codeExpr a st]
