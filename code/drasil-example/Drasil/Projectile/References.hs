module Drasil.Projectile.References where

import Language.Drasil
import Data.Drasil.Citations (cartesianWiki, accelerationWiki, velocityWiki, hibbeler2004)

citations :: BibRef
citations = [accelerationWiki, velocityWiki, hibbeler2004, cartesianWiki]

-- References --
citeRefs :: [Reference]
citeRefs = map ref citations
