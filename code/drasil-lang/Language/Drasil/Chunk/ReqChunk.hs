{-# Language TemplateHaskell #-}
module Language.Drasil.Chunk.ReqChunk 
  ( ReqChunk(..), ReqType(..)
  , frc, nfrc,
  ) where

import Language.Drasil.UID (UID)
import Language.Drasil.Classes (HasUID(uid))
import Language.Drasil.Chunk.ShortName (HasShortName(shortname))
import Language.Drasil.Spec (Sentence)
import Language.Drasil.Label.Core (Label)
import Language.Drasil.Classes (HasLabel(getLabel))

import Control.Lens ((^.), makeLenses)

-- We will likely need to differentiate functional/non-functional reqs
-- (or whatever we want to call them) for the future when we parse our 
-- recipes and build the lists/dbs of chunks.

-- FIXME: We need a better way to capture requirement information. Sentences
-- are dead information, and larger structures (like Contents) are display-specific.
-- For now, using sentences to test.

-- | What type of requirement are we dealing with?
data ReqType = FR  -- ^ Functional Requirement
             | NFR -- ^ Non-Functional Requirement
  deriving Eq
  
instance Show ReqType where
  show FR  = "FR"
  show NFR = "NFR"

-- | Requirement chunk type. Has an id, the type of requirement
-- (Functional/Non-Functional) from 'ReqType', a sentence describing what is
-- required (TODO: Change this), and a short name.
data ReqChunk = RC 
  { _id        :: UID
  , reqType    :: ReqType 
  , requires   :: Sentence
  , _lbl       :: Label
  }
makeLenses ''ReqChunk
  
instance HasUID        ReqChunk where uid f (RC a b c d) = fmap (\x -> RC x b c d) (f a)
instance Eq            ReqChunk where a == b = a ^. uid == b ^. uid
instance HasLabel      ReqChunk where getLabel = lbl
instance HasShortName  ReqChunk where shortname = lbl . shortname

-- | Smart constructor for requirement chunks (should not be exported)
rc :: String -> ReqType -> Sentence -> Label -> ReqChunk
rc = RC

frc, nfrc :: String -> Sentence -> Label -> ReqChunk
-- | Smart constructor for functional requirement chunks.
frc i = rc i FR

-- | Smart constructor for non-functional requirement chunks.
nfrc i = rc i NFR
