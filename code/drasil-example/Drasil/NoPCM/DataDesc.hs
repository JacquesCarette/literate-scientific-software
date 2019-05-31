module Drasil.NoPCM.DataDesc (inputMod) where

import Language.Drasil.Code (Func, Mod(Mod), funcData, junkLine, singleton)
import Drasil.SWHS.Unitals (tankLength, diam, coilSA, tempC, wDensity,
  htCapW, coilHTC, tempInit, tau, timeFinal, absTol, relTol, cons_tol)

inputMod :: Mod
inputMod = Mod "InputFormat" [inputData]

inputData :: Func
inputData = funcData "get_inputs"
  [ junkLine,
    singleton tankLength,
    junkLine,
    singleton diam,
    junkLine,
    singleton coilSA,
    junkLine,
    singleton tempC,
    junkLine,
    singleton wDensity,
    junkLine,
    singleton htCapW,
    junkLine,
    singleton coilHTC,
    junkLine,
    singleton tempInit,
    junkLine,
    singleton tau,
    junkLine,
    singleton timeFinal,
    junkLine,
    singleton absTol,
    junkLine,
    singleton relTol,
    junkLine,
    singleton cons_tol
  ]
