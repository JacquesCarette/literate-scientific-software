module Drasil.NoPCM.GenDefs (rocTempSimp, genDefs, genDefRefs) where

import Language.Drasil
import Theory.Drasil (GenDefn, gdNoRefs, ModelKinds (OthModel))

import Drasil.NoPCM.Assumptions (assumpDWCoW, assumpSHECoW)
import Drasil.SWHS.Assumptions (assumpCWTAT)
import Drasil.SWHS.GenDefs (htFluxWaterFromCoil, rocTempSimpRC, rocTempSimpDeriv)

genDefs :: [GenDefn]
genDefs = [rocTempSimp, htFluxWaterFromCoil] 

rocTempSimp :: GenDefn
rocTempSimp = gdNoRefs (OthModel rocTempSimpRC) (Nothing :: Maybe UnitDefn)
  (Just $ rocTempSimpDeriv EmptyS [assumpCWTAT, assumpDWCoW, assumpSHECoW])
  "rocTempSimp" [{-Notes-}]

-- References--
genDefRefs :: [Reference]
genDefRefs = map ref genDefs