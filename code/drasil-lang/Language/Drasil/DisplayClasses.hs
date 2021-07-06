module Language.Drasil.DisplayClasses where

import Language.Drasil.Display.Expr (DisplayExpr)
import Language.Drasil.Expr (Expr)

-- | Data that can convert into a Displayable 'Expr'.
class Display c where
  toDispExpr :: c -> DisplayExpr

-- | Basic wrapping in 'AlgebraicExpr'.
instance Display Expr where
  toDispExpr = undefined -- TODO

-- | No change, it's already a 'DisplayExpr'.
instance Display DisplayExpr where
  toDispExpr = id
