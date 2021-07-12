module Drasil.GlassBR.DataDefs (aspRat, dataDefs, dimLL, qDefns, glaTyFac,
  hFromt, loadDF, nonFL, risk, standOffDis, strDisFac, tolPre, tolStrDisFac,
  eqTNTWDD, probOfBreak, calofCapacity, calofDemand, pbTolUsr, qRef,configFp, dataDefRefs)
  where

import Control.Lens ((^.))
import Language.Drasil
import Language.Drasil.Code (asVC')
import Prelude hiding (log, exp, sqrt)
import Theory.Drasil (DataDefinition, dd)
import Database.Drasil (Block(Parallel))
import Utils.Drasil
import qualified Utils.Drasil.Sentence as S

import Data.Drasil.Concepts.Documentation (datum, user)
import Data.Drasil.Concepts.Math (parameter)
import Data.Drasil.Concepts.PhysicalProperties (dimension)

import Data.Drasil.Citations (campidelli)

import Drasil.GlassBR.Assumptions (assumpSV, assumpLDFC)
import Drasil.GlassBR.Concepts (annealed, fullyT, glass, heatS)
import Drasil.GlassBR.Figures (demandVsSDFig, dimlessloadVsARFig)
import Drasil.GlassBR.ModuleDefs (interpY, interpZ)
import Drasil.GlassBR.References (astm2009, beasonEtAl1998)
import Drasil.GlassBR.Unitals (actualThicknesses, aspectRatio, charWeight,
  demand, demandq, dimlessLoad, eqTNTWeight, gTF, glassType, glassTypeCon,
  glassTypeFactors, lDurFac, lRe, loadDur, loadSF, minThick, modElas, nomThick,
  nominalThicknesses, nonFactorL, pbTol, plateLen, plateWidth, probBr, riskFun,
  sdfTol, sdx, sdy, sdz, sflawParamK, sflawParamM, standOffDist, stressDistFac,
  tNT, tolLoad)

----------------------
-- DATA DEFINITIONS --
----------------------

dataDefs :: [DataDefinition]
dataDefs = [risk, hFromt, loadDF, strDisFac, nonFL, glaTyFac,
  dimLL, tolPre, tolStrDisFac, standOffDis, aspRat, eqTNTWDD, probOfBreak,
  calofCapacity, calofDemand]

qDefns :: [Block QDefinition]
qDefns = Parallel hFromtQD {-DD2-} [glaTyFacQD {-DD6-}] : --can be calculated on their own
  map (`Parallel` []) [dimLLQD {-DD7-}, strDisFacQD {-DD4-}, riskQD {-DD1-},
  tolStrDisFacQD {-DD9-}, tolPreQD {-DD8-}, nonFLQD {-DD5-}]

--DD1--

riskEq :: Expr
riskEq = (sy sflawParamK $/
  (mulRe (sy plateLen) (sy plateWidth) $^ (sy sflawParamM $- exactDbl 1))) `mulRe`
  ((sy modElas `mulRe` square (sy minThick)) $^ sy sflawParamM) `mulRe` sy lDurFac `mulRe` exp (sy stressDistFac)

-- FIXME [4] !!!
riskQD :: QDefinition
riskQD = mkQuantDef riskFun riskEq

risk :: DataDefinition
risk = dd riskQD
  [dRef astm2009, dRefInfo beasonEtAl1998 $ Equation [4, 5],
  dRefInfo campidelli $ Equation [14]]
  Nothing "riskFun" [aGrtrThanB, hRef, ldfRef, jRef]

--DD2--

hFromtEq :: Relation
hFromtEq = frac 1 1000 `mulRe` incompleteCase (zipWith hFromtHelper
  actualThicknesses nominalThicknesses)

hFromtHelper :: Double -> Double -> (Expr, Relation)
hFromtHelper result condition = (dbl result, sy nomThick $= dbl condition)

hFromtQD :: QDefinition
hFromtQD = mkQuantDef minThick hFromtEq

hFromt :: DataDefinition
hFromt = dd hFromtQD [dRef astm2009] Nothing "minThick" [hMin]

--DD3-- (#749)

loadDFEq :: Expr
loadDFEq = (sy loadDur $/ exactDbl 60) $^ (sy sflawParamM $/ exactDbl 16)

loadDFQD :: QDefinition
loadDFQD = mkQuantDef lDurFac loadDFEq

loadDF :: DataDefinition
loadDF = dd loadDFQD [dRef astm2009] Nothing "loadDurFactor"
  [stdVals [loadDur, sflawParamM], ldfConst]

--DD4--

strDisFacEq :: Expr
-- strDisFacEq = apply (sy stressDistFac)
--   [sy dimlessLoad, sy aspectRatio]
strDisFacEq = apply (asVC' interpZ) [Str "SDF.txt", sy aspectRatio, sy dimlessLoad]

strDisFacQD :: QDefinition
strDisFacQD = mkQuantDef stressDistFac strDisFacEq

strDisFac :: DataDefinition
strDisFac = dd strDisFacQD [dRef astm2009] Nothing "stressDistFac"
  [interpolating stressDistFac dimlessloadVsARFig, arRef, qHtRef]

--DD5--

nonFLEq :: Expr
nonFLEq = mulRe (mulRe (sy tolLoad) (sy modElas)) (sy minThick $^ exactDbl 4) $/
  square (mulRe (sy plateLen) (sy plateWidth))

nonFLQD :: QDefinition
nonFLQD = mkQuantDef nonFactorL nonFLEq

nonFL :: DataDefinition
nonFL = dd nonFLQD [dRef astm2009] Nothing "nFL"
  [qHtTlTolRef, stdVals [modElas], hRef, aGrtrThanB]

--DD6--

glaTyFacEq :: Expr
glaTyFacEq = incompleteCase (zipWith glaTyFacHelper glassTypeFactors $ map (getAccStr . snd) glassType)

glaTyFacHelper :: Integer -> String -> (Expr, Relation)
glaTyFacHelper result condition = (int result, sy glassTypeCon $= str condition)

glaTyFacQD :: QDefinition
glaTyFacQD = mkQuantDef gTF glaTyFacEq

glaTyFac :: DataDefinition
glaTyFac = dd glaTyFacQD [dRef astm2009] Nothing "gTF"
  [anGlass, ftGlass, hsGlass]

--DD7--

dimLLEq :: Expr
dimLLEq = mulRe (sy demand) (square (mulRe (sy plateLen) (sy plateWidth)))
  $/ mulRe (mulRe (sy modElas) (sy minThick $^ exactDbl 4)) (sy gTF)

dimLLQD :: QDefinition
dimLLQD = mkQuantDef dimlessLoad dimLLEq

dimLL :: DataDefinition
dimLL = dd dimLLQD [dRef astm2009, dRefInfo campidelli $ Equation [7]] Nothing "dimlessLoad"
  [qRef, aGrtrThanB, stdVals [modElas], hRef, gtfRef]

--DD8--

tolPreEq :: Expr
--tolPreEq = apply (sy tolLoad) [sy sdfTol, (sy plateLen) / (sy plateWidth)]
tolPreEq = apply (asVC' interpY) [Str "SDF.txt", sy aspectRatio, sy sdfTol]

tolPreQD :: QDefinition
tolPreQD = mkQuantDef tolLoad tolPreEq

tolPre :: DataDefinition
tolPre = dd tolPreQD [dRef astm2009] Nothing "tolLoad"
  [interpolating tolLoad dimlessloadVsARFig, arRef, jtolRef]

--DD9--

tolStrDisFacEq :: Expr
tolStrDisFacEq = ln (ln (recip_ (exactDbl 1 $- sy pbTol))
  `mulRe` ((sy plateLen `mulRe` sy plateWidth) $^ (sy sflawParamM $- exactDbl 1) $/
    (sy sflawParamK `mulRe` ((sy modElas `mulRe`
    square (sy minThick)) $^ sy sflawParamM) `mulRe` sy lDurFac)))

tolStrDisFacQD :: QDefinition
tolStrDisFacQD = mkQuantDef sdfTol tolStrDisFacEq

tolStrDisFac :: DataDefinition
tolStrDisFac = dd tolStrDisFacQD [dRef astm2009] Nothing "sdfTol"
  [pbTolUsr, aGrtrThanB, stdVals [sflawParamM, sflawParamK, mkUnitary modElas],
   hRef, ldfRef]

--DD10--

standOffDisEq :: Expr
standOffDisEq = sqrt (square (sy sdx) `addRe` square (sy sdy) `addRe` square (sy sdz))

standOffDisQD :: QDefinition
standOffDisQD = mkQuantDef standOffDist standOffDisEq

standOffDis :: DataDefinition
standOffDis = dd standOffDisQD [dRef astm2009] Nothing "standOffDist" []

--DD11--

aspRatEq :: Expr
aspRatEq = sy plateLen $/ sy plateWidth

aspRatQD :: QDefinition
aspRatQD = mkQuantDef aspectRatio aspRatEq

aspRat :: DataDefinition
aspRat = dd aspRatQD [dRef astm2009] Nothing "aspectRatio" [aGrtrThanB]

--DD12--
eqTNTWEq :: Expr
eqTNTWEq = mulRe (sy charWeight) (sy tNT)

eqTNTWQD :: QDefinition
eqTNTWQD = mkQuantDef eqTNTWeight eqTNTWEq

eqTNTWDD :: DataDefinition
eqTNTWDD = dd eqTNTWQD [dRef astm2009] Nothing "eqTNTW" []

--DD13--
probOfBreakEq :: Expr
probOfBreakEq = exactDbl 1 $- exp (neg (sy risk))

probOfBreakQD :: QDefinition
probOfBreakQD = mkQuantDef probBr probOfBreakEq

probOfBreak :: DataDefinition
probOfBreak = dd probOfBreakQD (map dRef [astm2009, beasonEtAl1998]) Nothing "probOfBreak" [riskRef]

--DD14--
calofCapacityEq :: Expr
calofCapacityEq = sy nonFL `mulRe` sy glaTyFac `mulRe` sy loadSF

calofCapacityQD :: QDefinition
calofCapacityQD = mkQuantDef lRe calofCapacityEq

calofCapacity :: DataDefinition
calofCapacity = dd calofCapacityQD [dRef astm2009] Nothing "calofCapacity"
  [lrCap, nonFLRef, gtfRef]

--DD15--
calofDemandEq :: Expr
calofDemandEq = apply (asVC' interpY) [Str "TSD.txt", sy standOffDist, sy eqTNTWeight]

calofDemandQD :: QDefinition
calofDemandQD = mkQuantDef demand calofDemandEq

calofDemand :: DataDefinition
calofDemand = dd calofDemandQD [dRef astm2009] Nothing "calofDemand" [calofDemandDesc]


--Additional Notes--
calofDemandDesc :: Sentence
calofDemandDesc =
  foldlSent [ch demand `sC` EmptyS `S.or_` phrase demandq `sC` EmptyS `S.isThe`
  (demandq ^. defn), S "obtained from", refS demandVsSDFig,
  S "by interpolation using", phrase standOffDist, sParen (ch standOffDist)
  `S.and_` ch eqTNTWeight, S "as" +:+. plural parameter, ch eqTNTWeight,
  S "is defined in" +:+. refS eqTNTWDD, ch standOffDist `S.isThe`
  phrase standOffDist, S "as defined in", refS standOffDis]

aGrtrThanB :: Sentence
aGrtrThanB = ch plateLen `S.and_` ch plateWidth `S.are` (plural dimension `S.the_ofThe` S "plate") `sC`
  S "where" +:+. sParen (eS (sy plateLen $>= sy plateWidth))

anGlass :: Sentence
anGlass = getAcc annealed `S.is` phrase annealed +:+. phrase glass

ftGlass :: Sentence
ftGlass = getAcc fullyT `S.is` phrase fullyT +:+. phrase glass

hMin :: Sentence
hMin = ch nomThick `S.is` S "a function that maps from the nominal thickness"
  +:+. (sParen (ch minThick) `S.toThe` phrase minThick)

hsGlass :: Sentence
hsGlass = getAcc heatS `S.is` phrase heatS +:+. phrase glass

ldfConst :: Sentence
ldfConst = ch lDurFac `S.is` S "assumed to be constant" +:+. fromSource assumpLDFC

lrCap :: Sentence
lrCap = ch lRe +:+. S "is also called capacity"

pbTolUsr :: Sentence
pbTolUsr = ch pbTol `S.is` S "entered by the" +:+. phrase user

qRef :: Sentence
qRef = ch demand `S.isThe` (demandq ^. defn) `sC` S "as given in" +:+. refS calofDemand

arRef, gtfRef, hRef, jRef, jtolRef, ldfRef, nonFLRef, qHtRef, qHtTlTolRef, riskRef :: Sentence
arRef       = definedIn  aspRat
gtfRef      = definedIn  glaTyFac
hRef        = definedIn' hFromt (S "and is based on the nominal thicknesses")
jRef        = definedIn  strDisFac
jtolRef     = definedIn  tolStrDisFac
ldfRef      = definedIn  loadDF
nonFLRef    = definedIn  nonFL
qHtRef      = definedIn  dimLL
qHtTlTolRef = definedIn  tolPre
riskRef     = definedIn  risk

-- List of Configuration Files necessary for DataDefs.hs
configFp :: [String]
configFp = ["SDF.txt", "TSD.txt"]

--- Helpers
interpolating :: (HasUID s, HasSymbol s, Referable f, HasShortName f) => s -> f -> Sentence
interpolating s f = foldlSent [ch s `S.is` S "obtained by interpolating from",
  plural datum, S "shown" `S.in_` refS f]

stdVals :: (HasSymbol s, HasUID s) => [s] -> Sentence
stdVals s = foldlList Comma List (map ch s) +:+ sent +:+. refS assumpSV
  where sent = case s of [ ]   -> error "stdVals needs quantities"
                         [_]   -> S "comes from"
                         (_:_) -> S "come from"

-- References --
dataDefRefs :: [Reference]
dataDefRefs = ref assumpSV : map ref [dimlessloadVsARFig, demandVsSDFig]
  ++ map ref dataDefs
