module Language.Drasil.Sentence.Extract(sdep, shortdep, lnames, lnames') where

import Data.List (nub)
import Language.Drasil.UID (UID)
import Language.Drasil.Sentence(Sentence(..), SentenceStyle(..), Reference(Reference))
import Language.Drasil.Expr.Extract(names)


-- | Generic traverse of all positions that could lead to UIDs from 'Sentence's.
getUIDs :: Sentence -> [UID]
getUIDs (Ch SymbolStyle a) = [a]
getUIDs (Ch ShortStyle _)  = []
getUIDs (Ch TermStyle _)   = []
getUIDs (Ch PluralTerm _)  = []
getUIDs (Sy _)             = []
getUIDs (S _)              = []
getUIDs (P _)              = []
getUIDs (Ref _)            = []
getUIDs Percent            = []
getUIDs ((:+:) a b)        = getUIDs a ++ getUIDs b
getUIDs (Quote a)          = getUIDs a
getUIDs (E a)              = names a
getUIDs EmptyS             = []

-- | Generic traverse of all positions that could lead to 'UID's from 'Sentence's
-- but doesn't go into expressions.
getUIDshort :: Sentence -> [UID]
getUIDshort (Ch ShortStyle a)  = [a]
getUIDshort (Ch SymbolStyle _) = []
getUIDshort (Ch TermStyle _)   = []
getUIDshort (Ch PluralTerm _)  = []
getUIDshort (Sy _)             = []
getUIDshort (S _)              = []
getUIDshort Percent            = []
getUIDshort (P _)              = []
getUIDshort (Ref _)            = []
getUIDshort ((:+:) a b)        = getUIDshort a ++ getUIDshort b
getUIDshort (Quote a)          = getUIDshort a
getUIDshort (E _)              = []
getUIDshort EmptyS             = []

-----------------------------------------------------------------------------
-- And now implement the exported traversals all in terms of the above
-- | This is to collect 'UID's that are printed out as a 'Symbol'.
sdep :: Sentence -> [UID]
sdep = nub . getUIDs

-- This is to collect 'UID's that are printed out as an abbreviation.
shortdep :: Sentence -> [UID]
shortdep = nub . getUIDshort

-- | Generic traverse of all positions that could lead to reference 'UID's from 'Sentence's.
lnames :: Sentence -> [UID]
lnames (Ch _ _)       = []
lnames (Sy _)         = []
lnames (S _)          = []
lnames Percent        = []
lnames (P _)          = []
lnames (Ref (Reference u _ _ _)) = [u] -- This should be fixed.
lnames ((:+:) a b)    = lnames a ++ lnames b
lnames (Quote _)      = []
lnames (E _)          = []
lnames EmptyS         = []

-- | Get reference 'UID's from 'Sentence's.
lnames'  :: [Sentence] -> [UID]
lnames' = concatMap lnames 
