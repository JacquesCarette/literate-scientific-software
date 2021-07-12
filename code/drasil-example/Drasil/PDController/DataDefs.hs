{-# LANGUAGE PostfixOperators #-}
module Drasil.PDController.DataDefs where

import Drasil.PDController.Concepts
import Drasil.PDController.Assumptions
import Drasil.PDController.Unitals
import Drasil.PDController.References
import Drasil.PDController.TModel
import Data.Drasil.Concepts.Math (equation)
import Language.Drasil
import Theory.Drasil (DataDefinition, dd)
import Utils.Drasil
import Utils.Drasil.Concepts
import qualified Utils.Drasil.Sentence as S

dataDefinitions :: [DataDefinition]
dataDefinitions = [ddErrSig, ddPropCtrl, ddDerivCtrl, ddCtrlVar]

----------------------------------------------

ddErrSig :: DataDefinition
ddErrSig
  = dd ddErrSigDefn [dRef johnson2008] Nothing "ddProcessError"
      [ddErrSigNote]

ddErrSigDefn :: QDefinition
ddErrSigDefn = mkQuantDef qdProcessErrorFD ddErrSigEqn

ddErrSigEqn :: Expr
ddErrSigEqn = sy qdSetPointFD $- sy qdProcessVariableFD

ddErrSigNote :: Sentence
ddErrSigNote
  = foldlSent
      [atStartNP (the processError), S "is the difference between the Set-Point and" +:+.
         phrase processVariable,
       atStartNP (the equation), S "is converted to the", phrase ccFrequencyDomain,
         S "by applying the", atStart ccLaplaceTransform +:+. fromSource tmLaplace,
       atStartNP (the setPoint), S "is assumed to be constant throughout the",
         phrase simulation +:+. fromSource aSP,
       S "The initial value of the", phrase processVariable, S "is assumed",
         S "to be zero", fromSource aInitialValue]

----------------------------------------------

ddPropCtrl :: DataDefinition
ddPropCtrl
  = dd ddPropCtrlDefn [dRef johnson2008] Nothing "ddPropCtrl"
      [ddPropCtrlNote]

ddPropCtrlDefn :: QDefinition
ddPropCtrlDefn = mkQuantDef qdPropControlFD ddPropCtrlEqn

ddPropCtrlEqn :: Expr
ddPropCtrlEqn = sy qdPropGain `mulRe` sy qdProcessErrorFD

ddPropCtrlNote :: Sentence
ddPropCtrlNote
  = foldlSent
      [S "The Proportional Controller is the product of the", phraseNP (propGain `andThe`
         processError) +:+. fromSource ddErrSig,
         atStartNP (the equation), S "is converted to the", phrase ccFrequencyDomain,
         S "by applying the", atStart ccLaplaceTransform, fromSource tmLaplace]

----------------------------------------------

ddDerivCtrl :: DataDefinition
ddDerivCtrl
  = dd ddDerivCtrlDefn [dRef johnson2008] Nothing "ddDerivCtrl"
      [ddDerivCtrlNote]

ddDerivCtrlDefn :: QDefinition
ddDerivCtrlDefn = mkQuantDef qdDerivativeControlFD ddDerivCtrlEqn

ddDerivCtrlEqn :: Expr
ddDerivCtrlEqn
  =  sy qdDerivGain `mulRe` sy qdProcessErrorFD `mulRe` sy qdFreqDomain

ddDerivCtrlNote :: Sentence
ddDerivCtrlNote
  = foldlSent
      [S "The Derivative Controller is the product of the", phrase derGain
         `S.andThe` S "differential" `S.ofThe` phrase processError +:+. fromSource ddErrSig,
       atStartNP (the equation), S "is converted to the", phrase ccFrequencyDomain,
         S "by applying the", atStart ccLaplaceTransform +:+. fromSource tmLaplace,
       S "A pure form of the Derivative controller is used in this",
         S "application", fromSource aUnfilteredDerivative]

----------------------------------------------

ddCtrlVar :: DataDefinition
ddCtrlVar
  = dd ddCtrlVarDefn [dRef johnson2008] Nothing "ddCtrlVar" [ddCtrlNote]

ddCtrlVarDefn :: QDefinition
ddCtrlVarDefn = mkQuantDef qdCtrlVarFD ddCtrlEqn

ddCtrlEqn :: Expr
ddCtrlEqn
  =  sy qdProcessErrorFD `mulRe` (sy qdPropGain `addRe` 
        (sy qdDerivGain `mulRe` sy qdFreqDomain))

ddCtrlNote :: Sentence
ddCtrlNote
  = foldlSent
      [atStartNP (the controlVariable) +:+. S "is the output of the controller",
       S "In this case" `sC` S "it is the sum of the Proportional", fromSource ddPropCtrl,
         S "and Derivative", fromSource ddDerivCtrl +:+.
         S "controllers",
       S "The parallel", fromSource aParallelEq,
         S "and de-coupled", fromSource aDecoupled,
         S "form of the PD equation is",
         S "used in this document"]

-- References --
dataDefRefs :: [Reference]
dataDefRefs = map ref dataDefinitions