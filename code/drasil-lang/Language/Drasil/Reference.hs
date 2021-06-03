module Language.Drasil.Reference where

import Control.Lens ((^.))

import Language.Drasil.Chunk.Citation (Citation)
import Language.Drasil.Classes.Core (HasUID(uid))
import Language.Drasil.Classes (Referable(renderRef))
import Language.Drasil.Sentence (Reference(..), RefInfo(..), Sentence(Ref), HasShortName(shortname))

-- | Projector function that creates a 'Reference' from something 'Referable'.
makeRef2 :: (Referable l, HasShortName l) => l -> Reference
makeRef2 l = Reference (l ^. uid) (renderRef l) (shortname l) None

-- | Makes a reference from the given argument and then wraps it into a 'Sentence'.
makeRef2S :: (Referable l, HasShortName l) => l -> Sentence
makeRef2S = Ref . makeRef2

-- Here we don't use the Lenses as constraints, we really do want a Citation.
-- | Similar to `makeRef2`, but only turns a citation into a reference.
makeCite :: Citation -> Reference
makeCite l = Reference (l ^. uid) (renderRef l) (shortname l) None

-- | Similar to `makeRef2S`, but only takes a citation.
makeCiteS :: Citation -> Sentence
makeCiteS = Ref . makeCite

-- | Makes a 'Reference' from a 'Citation' with additional information.
makeCiteInfo :: Citation -> RefInfo -> Reference
makeCiteInfo l = Reference (l ^. uid) (renderRef l) (shortname l)

-- | Makes a 'Reference' from a 'Citation' with additional information
-- and then wraps into 'Sentence' form.
makeCiteInfoS :: Citation -> RefInfo -> Sentence
makeCiteInfoS c ri = Ref $ makeCiteInfo c ri
