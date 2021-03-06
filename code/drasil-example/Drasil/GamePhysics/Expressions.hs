module Drasil.GamePhysics.Expressions where

import Language.Drasil

import Drasil.GamePhysics.Unitals (massj, velj, torquej, forcej, angAccj)
import Data.Drasil.Quantities.Physics (time, momentOfInertia, 
    gravitationalAccel, angularVelocity)

transMotExpr :: Expr
transMotExpr = sy gravitationalAccel `addRe` (apply1 forcej time $/ sy massj)

transMotExprDeriv1 :: DisplayExpr
transMotExprDeriv1 = defines angAccj $ deriv (apply1 velj time) time

rotMotExpr :: Expr
rotMotExpr = apply1 torquej time $/ sy momentOfInertia

rotMotExprDeriv1 :: DisplayExpr
rotMotExprDeriv1 = defines angAccj $ deriv (apply1 angularVelocity time) time
