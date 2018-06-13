module Data.Drasil.Quantities.PhysicalProperties where

import Language.Drasil
import Data.Drasil.Concepts.PhysicalProperties as CPP (density, len, mass, vol)
import Data.Drasil.SI_Units (kilogram, metre, m_3)
import Data.Drasil.Units.PhysicalProperties (densityU)

density, mass, len, vol :: UnitalChunk
density = uc CPP.density lRho densityU
mass    = uc CPP.mass    lM            kilogram
len     = uc CPP.len     cL            metre
vol     = uc CPP.vol     cV            m_3
