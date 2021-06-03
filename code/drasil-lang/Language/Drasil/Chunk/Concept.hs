{-# Language TypeFamilies #-}
module Language.Drasil.Chunk.Concept 
  ( ConceptChunk, dcc, dcc', dccWDS, dccWDS', cc, cc', ccs, cic, cw
  , CommonConcept, ConceptInstance
  ) where

import Language.Drasil.Classes.Core (HasUID(uid))
import Language.Drasil.Classes (Idea, Definition(defn), ConceptDomain(cdom), Concept)
import Language.Drasil.Chunk.CommonIdea (commonIdea)
import Language.Drasil.Chunk.Concept.Core (ConceptChunk(ConDict),
  ConceptInstance(ConInst), CommonConcept(ComConDict))
import Language.Drasil.Sentence (Sentence(S), shortname')
import Language.Drasil.Chunk.NamedIdea(mkIdea,nw, nc)
import Language.Drasil.NounPhrase (NP, pn)

import Control.Lens ((^.))

--FIXME: Temporary ConceptDomain tag hacking to not break everything. 
 
dcc :: String -> NP -> String -> ConceptChunk 
-- | Smart constructor for creating concept chunks given a 'UID', 
-- 'NounPhrase' ('NP') and definition (as a 'String').
dcc i ter des = ConDict (mkIdea i ter Nothing) (S des) []
-- ^ Concept domain tagging is not yet implemented in this constructor.

-- | Identical to 'dcc', but takes an abbreviation ('String').
dcc' :: String -> NP -> String -> String -> CommonConcept
dcc' i t d a = ComConDict (commonIdea i t a []) (S d)

-- | Similar to 'dcc', except the definition takes a 'Sentence'.
dccWDS :: String -> NP -> Sentence -> ConceptChunk
dccWDS i t d = ConDict (mkIdea i t Nothing) d []

-- | Similar to 'dcc', except the definition is a 'Sentence' and takes
-- an abbreviation ('String').
dccWDS' :: String -> NP -> Sentence -> String -> CommonConcept
dccWDS' i t d a = ComConDict (commonIdea i t a []) d

-- | Constructor for 'ConceptChunk'. Takes the definition of the 
-- 'ConceptChunk' as a 'String'. Does not allow concept domain tagging.
cc :: Idea c => c -> String -> ConceptChunk
cc n d = ConDict (nw n) (S d) []

-- | Same as cc, except definition is a 'Sentence'.
cc' :: Idea c => c -> Sentence -> ConceptChunk
cc' n d = ConDict (nw n) d []

-- | Similar to 'cc'', but allows explicit tagging.
ccs :: (Idea c, Concept d) => c -> Sentence -> [d] -> ConceptChunk --Explicit tagging
ccs n d l = ConDict (nw n) d $ map (^. uid) l

-- | For projecting out to the 'ConceptChunk' data-type.
cw :: Concept c => c -> ConceptChunk
cw c = ConDict (nw c) (c ^. defn) (cdom c)

-- | Constructor for a 'ConceptInstance'. Takes in the 
-- Reference Address ('String'), a definition ('Sentence'), 
-- a short name ('String'), and a domain (or explicit tagging).
cic :: Concept c => String -> Sentence -> String -> c -> ConceptInstance
cic u d sn dom = ConInst (ccs (nc u $ pn sn) d [dom]) u $ shortname' sn
