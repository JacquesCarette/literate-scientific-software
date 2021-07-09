module Drasil.DblPendulum.References where

import Language.Drasil
import Data.Drasil.Citations (cartesianWiki, accelerationWiki, velocityWiki, parnasClements1986, hibbeler2004)

citations :: BibRef
citations = [accelerationWiki, velocityWiki, hibbeler2004, cartesianWiki, parnasClements1986]

-- References --
citeRefs :: [Reference]
citeRefs = map ref citations
