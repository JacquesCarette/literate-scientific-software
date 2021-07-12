{-# LANGUAGE PostfixOperators #-}
module Drasil.PDController.IModel where

import Data.Drasil.Quantities.Physics (time)
import Drasil.PDController.Assumptions
import Drasil.PDController.Concepts
import Drasil.PDController.DataDefs
import Drasil.PDController.GenDefs
import Drasil.PDController.References
import Drasil.PDController.TModel
import Language.Drasil
import Theory.Drasil (InstanceModel, im, qwC, ModelKinds (DEModel))
import Utils.Drasil
import Utils.Drasil.Concepts
import qualified Utils.Drasil.Sentence as S
import Drasil.PDController.Unitals

instanceModels :: [InstanceModel]
instanceModels = [imPD]

----------------------------------------------

imPD :: InstanceModel
imPD
  = im (DEModel imPDRC)
      [qwC qdSetPointTD $ UpFrom (Exc, exactDbl 0), qwC qdPropGain $ UpFrom (Exc, exactDbl 0),
       qwC qdDerivGain $ UpFrom (Exc, exactDbl 0)]
      (qw qdProcessVariableTD)
      [UpFrom (Exc, exactDbl 0)]
      [dRef abbasi2015, dRef johnson2008]
      (Just imDeriv)
      "pdEquationIM"
      []

imPDRC :: RelationConcept
imPDRC
  = makeRC "imPDRC"
      (nounPhraseSP
         "Computation of the Process Variable as a function of time")
      EmptyS
      eqn

eqn :: Expr
eqn
  = deriv (deriv (sy qdProcessVariableTD) time) time `addRe`
      ((exactDbl 1 `addRe` sy qdDerivGain) `mulRe` deriv (sy qdProcessVariableTD) time)
      `addRe` ((exactDbl 20 `addRe` sy qdPropGain) `mulRe` sy qdProcessVariableTD)
      $- (sy qdSetPointTD `mulRe` sy qdPropGain) $= exactDbl 0

imDeriv :: Derivation
imDeriv
  = mkDerivName (phrase processVariable)
      (weave [imDerivStmts, map eS imDerivEqns])

imDerivStmts :: [Sentence]
imDerivStmts = [derivStmt1, derivStmt2, derivStmt3, derivStmt4]

imDerivEqns :: [Expr]
imDerivEqns = [derivEqn1, derivEqn2, derivEqn3, derivEqn4]

derivStmt1 :: Sentence
derivStmt1
  = foldlSent
      [atStartNP (the processVariable), eS qdProcessVariableFD, S "in a", phrase pidCL +:+
         S "is the product of the", phrase processError, fromSource ddErrSig `sC`
         phrase controlVariable, fromSource ddCtrlVar `sC` EmptyS
         `S.andThe` phrase powerPlant, fromSource gdPowerPlant]

derivEqn1 :: Expr
derivEqn1
  = sy qdProcessVariableFD
      $= (sy qdSetPointFD $- sy qdProcessVariableFD)
      `mulRe` (sy qdPropGain `addRe` (sy qdDerivGain `mulRe` sy qdFreqDomain))
      `mulRe` recip_ (square (sy qdFreqDomain) `addRe` sy qdFreqDomain `addRe` exactDbl 20)

derivStmt2 :: Sentence
derivStmt2 = (S "Substituting the values and rearranging the equation" !.)

derivEqn2 :: Expr
derivEqn2
  = square (sy qdFreqDomain) `mulRe` sy qdProcessVariableFD
      `addRe` ((exactDbl 1 `addRe` sy qdDerivGain) `mulRe` sy qdProcessVariableFD `mulRe` sy qdFreqDomain)
      `addRe` ((exactDbl 20 `addRe` sy qdPropGain) `mulRe` sy qdProcessVariableFD)
      $- (sy qdSetPointFD `mulRe` sy qdFreqDomain `mulRe` sy qdDerivGain)
      $- (sy qdSetPointFD `mulRe` sy qdPropGain) $= exactDbl 0

derivStmt3 :: Sentence
derivStmt3
  = S "Computing the" +:+ phrase qdInvLaplaceTransform +:+
     fromSource tmInvLaplace +:+. S "of the equation"

derivEqn3 :: Expr
derivEqn3
  = deriv (deriv (sy qdProcessVariableTD) time) time `addRe`
      (((exactDbl 1 `addRe` sy qdDerivGain) `mulRe` deriv (sy qdProcessVariableTD) time)
      `addRe` ((exactDbl 20 `addRe` sy qdPropGain) `mulRe` sy qdProcessVariableTD))
      $- (sy qdDerivGain `mulRe` deriv (sy qdSetPointTD) time)
      $- (sy qdSetPointTD `mulRe` sy qdPropGain) $= exactDbl 0

derivStmt4 :: Sentence
derivStmt4
  = foldlSent_
      [atStartNP (the setPoint), eS qdSetPointTD, S "is a step function and a constant" +:+.
         fromSource aSP,
       S "Therefore the",
         S "differential of the set point is zero. Hence the equation",
         S "reduces to"]

derivEqn4 :: Expr
derivEqn4
  = deriv (deriv (sy qdProcessVariableTD) time) time `addRe`
      ((exactDbl 1 `addRe` sy qdDerivGain) `mulRe` deriv (sy qdProcessVariableTD) time)
      `addRe` ((exactDbl 20 `addRe` sy qdPropGain) `mulRe` sy qdProcessVariableTD)
      $- (sy qdSetPointTD `mulRe` sy qdPropGain) $= exactDbl 0

-- References -- 
iModRefs :: [Reference]
iModRefs = map ref instanceModels