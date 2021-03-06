module Language.Drasil.Printing.Import.Space where

import Language.Drasil (dbl, Space(..))

import qualified Language.Drasil.Printing.AST as P
import Language.Drasil.Printing.PrintingInformation (PrintingInformation)

import Language.Drasil.Printing.Import.Expr (expr)

import Data.List (intersperse)

-- | Render a 'Space'.
space :: PrintingInformation -> Space -> P.Expr
space _ Integer = P.MO P.Integer
space _ Rational = P.MO P.Rational
space _ Real = P.MO P.Real
space _ Natural = P.MO P.Natural
space _ Boolean = P.MO P.Boolean
space _ Char = P.Ident "Char"
space _ String = P.Ident "String"
space _ Radians = error "Radians not translated"
space _ (Vect _) = error "Vector space not translated"
space _ (Array _) = error "Array space not translated"
space _ (Actor s) = P.Ident s
space sm (DiscreteD l) = P.Fenced P.Curly P.Curly $ P.Row $ intersperse (P.MO P.Comma) $ map (flip expr sm . dbl) l -- [Double]
space _ (DiscreteS l) = P.Fenced P.Curly P.Curly $ P.Row $ intersperse (P.MO P.Comma) $ map P.Str l --ex. let Meal = {"breakfast", "lunch", "dinner"}
space _ Void = error "Void not translated"
