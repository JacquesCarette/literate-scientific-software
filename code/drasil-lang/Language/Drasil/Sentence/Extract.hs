module Language.Drasil.Sentence.Extract(sdep, shortdep, lnames, lnames') where

import Data.List (nub)
import Language.Drasil.UID (UID)
import Language.Drasil.Sentence(Sentence(..), SentenceStyle(..))
import Language.Drasil.Display.Extract (deDep)


-- | Generic traverse of all positions that could lead to /symbolic/ 'UID's from 'Sentence's.
getUIDs :: Sentence -> [UID]
getUIDs (Ch ShortStyle _ _) = []
getUIDs (Ch TermStyle _ _)  = []
getUIDs (Ch PluralTerm _ _) = []
getUIDs (SyCh a)            = [a]
getUIDs (Sy _)              = []
getUIDs (S _)               = []
getUIDs (P _)               = []
getUIDs (Ref {})            = []
getUIDs Percent             = []
getUIDs ((:+:) a b)         = getUIDs a ++ getUIDs b
getUIDs (Quote a)           = getUIDs a
getUIDs (E a)               = deDep a
getUIDs EmptyS              = []

-- | Generic traverse of all positions that could lead to /symbolic/ and /abbreviated/ 'UID's from 'Sentence's
-- but doesn't go into expressions.
getUIDshort :: Sentence -> [UID]
getUIDshort (Ch ShortStyle _ a) = [a]
getUIDshort (Ch TermStyle _ _)  = []
getUIDshort (Ch PluralTerm _ _) = []
getUIDshort (SyCh _)            = []
getUIDshort (Sy _)              = []
getUIDshort (S _)               = []
getUIDshort Percent             = []
getUIDshort (P _)               = []
getUIDshort (Ref {})            = []
getUIDshort ((:+:) a b)         = getUIDshort a ++ getUIDshort b
getUIDshort (Quote a)           = getUIDshort a
getUIDshort (E _)               = []
getUIDshort EmptyS              = []

-- TODO: Should this be exported by the Language.Drasil file or the Language.Drasil.Display file?
-----------------------------------------------------------------------------
-- And now implement the exported traversals all in terms of the above
-- | This is to collect /symbolic/ 'UID's that are printed out as a 'Symbol'.
sdep :: Sentence -> [UID]
sdep = nub . getUIDs

-- This is to collect symbolic 'UID's that are printed out as an /abbreviation/.
shortdep :: Sentence -> [UID]
shortdep = nub . getUIDshort

-- | Generic traverse of all positions that could lead to /reference/ 'UID's from 'Sentence's.
lnames :: Sentence -> [UID]
lnames (Ch _ _ _)     = []
lnames (SyCh _)       = []
lnames (Sy _)         = []
lnames (S _)          = []
lnames Percent        = []
lnames (P _)          = []
lnames (Ref a _ _)    = [a]
lnames ((:+:) a b)    = lnames a ++ lnames b
lnames (Quote _)      = []
lnames (E _)          = []
lnames EmptyS         = []

-- | Get /reference/ 'UID's from 'Sentence's.
lnames'  :: [Sentence] -> [UID]
lnames' = concatMap lnames 
