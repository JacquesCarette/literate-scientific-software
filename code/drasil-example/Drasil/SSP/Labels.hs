module Drasil.SSP.Labels where

import Language.Drasil


-- Assumptions
slipSurfaceL                = mkLabelRAAssump' "Slip-Surface-Concave"
geoSlopeMatL                = mkLabelRAAssump' "Geo-Slope-Mat-Props-of-Soil-Inputs"
soilLayerHomoL              = mkLabelRAAssump' "Soil-Layer-Homogeneous"
soilLayerIsoL               = mkLabelRAAssump' "Soil-Layers-Isotropic"
intersliceNormL             = mkLabelRAAssump' "Interslice-Norm-Shear-Forces-Linear"
baseNormShearForL           = mkLabelRAAssump' "Base-Norm-Shear-Forces-Linear-on-FS"
stressStrainCurveL          = mkLabelRAAssump' "Stress-Strain-Curve-interslice-Linear"
planeStrainL                = mkLabelRAAssump' "Plane-Strain-Conditions"
effectiveNormL              = mkLabelRAAssump' "Effective-Norm-Stress-Large"
surfaceBaseSliceL           = mkLabelRAAssump' "Surface-Base-Slice-between-Interslice-Straight-Lines"


-- General Definations
genDef1Label, genDef2Label, genDef3Label, genDef4Label, genDef5Label, genDef6Label, 
    genDef7Label, genDef8Label, genDef9Label, genDef10Label :: Label

genDef1Label  = mkLabelSame "normForcEq"  (Def General)
genDef2Label  = mkLabelSame "bsShrFEq"    (Def General)
genDef3Label  = mkLabelSame "resShr"      (Def General)
genDef4Label  = mkLabelSame "mobShr"      (Def General)
genDef5Label  = mkLabelSame "normShrR"    (Def General)
genDef6Label  = mkLabelSame "momentEql"   (Def General)
genDef7Label  = mkLabelSame "netForcex"   (Def General)
genDef8Label  = mkLabelSame "netForcey"   (Def General)
genDef9Label  = mkLabelSame "hookesLaw2d" (Def General)
genDef10Label = mkLabelSame "displVect"   (Def General)