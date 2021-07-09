module Utils.Drasil.Concepts (and_, and_PS, and_PP, and_TGen, and_Gen, andIts, andThe, with, of_, of_NINP, of_PSNPNI, of_PS, ofA,
ofAPS, ofThe, ofThePS, the_ofThe, the_ofThePS, onThe, onThePS, onThePP, inThe, inThePS, inThePP, isThe, toThe, for, forTGen, in_, in_PS, inA, is, the, theGen, a_, a_Gen,
compoundNC, compoundNCPP, compoundNCGen, compoundNCPS, compoundNCPSPP, compoundNCGenP, combineNINP, combineNPNI, combineNINI) where

import Language.Drasil
import qualified Language.Drasil.Development as D
import Control.Lens ((^.))

import qualified Utils.Drasil.Sentence as S (and_, andIts, andThe, of_, ofA, ofThe, the_ofThe, onThe, for, inThe, in_, is, toThe, isThe) 


-- | Creates a 'NP' by combining two 'NamedIdea's with the word "and" between
-- their terms. Plural case is @(phrase t1) "and" (plural t2)@.
and_ :: (NamedIdea c, NamedIdea d) => c -> d -> NP
and_ t1 t2 = nounPhrase''
  (phrase t1 `S.and_` phrase t2)
  (phrase t1 `S.and_` plural t2)
  CapFirst
  CapWords

-- | Creates a 'NP' by combining two 'NamedIdea's with the word "and" between
-- their terms. Plural case is @(plural t1) "and" (phrase t2)@.
and_PS :: (NamedIdea c, NamedIdea d) => c -> d -> NP
and_PS t1 t2 = nounPhrase''
  (phrase t1 `S.and_` phrase t2)
  (plural t1 `S.and_` phrase t2)
  CapFirst
  CapWords

-- | Creates a 'NP' by combining two 'NamedIdea's with the word "and" between
-- their terms. Plural case is @(plural t1) "and" (plural t2)@.
and_PP :: (NamedIdea c, NamedIdea d) => c -> d -> NP
and_PP t1 t2 = nounPhrase''
  (phrase t1 `S.and_` phrase t2)
  (plural t1 `S.and_` plural t2)
  CapFirst
  CapWords

-- | Customizable `and_` combinator. Both plural and singular cases are dermined by the two given functions
and_Gen :: (c -> Sentence) -> (d -> Sentence) -> c -> d -> NP
and_Gen f1 f2 t1 t2 = nounPhrase''
  (f1 t1 `S.and_` f2 t2)
  (f1 t1 `S.and_` f2 t2)
  CapFirst
  CapWords

-- | Customizable `and_` combinator (takes two title case capitalization rules and two 'NamedIdeas').
and_TGen :: (NamedIdea c, NamedIdea d) => 
  (c -> Sentence) -> (d -> Sentence) -> c -> d -> NP
and_TGen f1 f2 t1 t2 = nounPhrase''
  (phrase t1 `S.and_` phrase t2)
  (phrase t1 `S.and_` plural t2)
  CapFirst
  (Replace (f1 t1 `S.and_` f2 t2))

-- | Creates a 'NP' by combining two 'NamedIdea's with the words "and its" between
-- their terms. Plural case is @(phrase t1) "and its" (plural t2)@.
andIts :: (NamedIdea c, NamedIdea d) => c -> d -> NP
andIts t1 t2 = nounPhrase''
  (phrase t1 `S.andIts` phrase t2)
  (phrase t1 `S.andIts` plural t2)
  CapFirst
  CapWords

-- | Creates a 'NP' by combining two 'NamedIdea's with the words "and the" between
-- their terms. Plural case is @(phrase t1) "and the" (plural t2)@.
andThe :: (NamedIdea c, NamedIdea d) => c -> d -> NP
andThe t1 t2 = nounPhrase''
  (phrase t1 `S.andThe` phrase t2)
  (phrase t1 `S.andThe` plural t2)
  CapFirst
  CapWords

-- | Case with "T1s with T2", as opposed to "T1 with T2", i.e.
-- singular case is @(plural t1) "with" (phrase t2)@ while the plural case pluralizes the first.
with :: (NamedIdea c, NamedIdea d) => c -> d -> NP
with t1 t2 = nounPhrase''
  (plural t1 +:+ S "with" +:+ phrase t2)
  (plural t1 +:+ S "with" +:+ phrase t2)
  CapFirst
  CapWords

-- | Creates a 'NP' by combining two 'NamedIdea's with the word "of" between
-- their terms. Plural case is @(phrase t1) "of" (plural t2)@.
of_ :: (NamedIdea c, NamedIdea d) => c -> d -> NP
of_ t1 t2 = nounPhrase'' 
  (phrase t1 `S.of_` phrase t2)
  (phrase t1 `S.of_` plural t2)
  CapFirst
  CapWords

-- | Same as 'of_' but second argument is a `NounPhrase`.
of_NINP :: (NamedIdea c, NounPhrase d) => c -> d -> NP
of_NINP t1 t2 = nounPhrase'' 
  (phrase t1 `S.of_` phraseNP t2)
  (phrase t1 `S.of_` pluralNP t2)
  CapFirst
  CapWords

-- | Same as 'of_' but first argument is a `NounPhrase` 
-- and plural case is @(plural t1) "of" (phrase t2)@.
of_PSNPNI :: (NounPhrase c, NamedIdea d) => c -> d -> NP
of_PSNPNI t1 t2 = nounPhrase'' 
  (phraseNP t1 `S.of_` phrase t2)
  (pluralNP t1 `S.of_` phrase t2)
  CapFirst
  CapWords

-- | Same as 'of_', except plural case is @(plural t1) "of" (phrase t2)@.
of_PS :: (NamedIdea c, NamedIdea d) => c -> d -> NP
of_PS t1 t2 = nounPhrase'' 
  (phrase t1 `S.of_` phrase t2)
  (plural t1 `S.of_` phrase t2)
  CapFirst
  CapWords

-- | Same as 'of_PS', except combining 'Sentence' piece is "of a".
ofA :: (NamedIdea c, NamedIdea d) => c -> d -> NP
ofA t1 t2 = nounPhrase'' 
  (phrase t1 `S.ofA` phrase t2)
  (plural t1 `S.ofA` phrase t2)
  CapFirst
  CapWords

-- | Same as 'ofA', except phrase case is @(plural t1) "of a" (phrase t2)@.
ofAPS :: (NamedIdea c, NamedIdea d) => c -> d -> NP
ofAPS t1 t2 = nounPhrase'' 
  (phrase t1 `S.ofA` phrase t2)
  (plural t1 `S.ofA` phrase t2)
  CapFirst
  CapWords

-- | Same as 'of_', except combining 'Sentence' piece is "of the". Plural case is @(phrase t1) `S.ofThe` (plural t2)@.
ofThe :: (NamedIdea c, NamedIdea d) => c -> d -> NP
ofThe t1 t2 = nounPhrase'' 
  (phrase t1 `S.ofThe` phrase t2)
  (phrase t1 `S.ofThe` plural t2)
  CapFirst
  CapWords

-- | Same as 'ofThe', except plural case is @(plural t1) `S.ofThe` (phrase t2)@.
ofThePS :: (NamedIdea c, NamedIdea d) => c -> d -> NP
ofThePS t1 t2 = nounPhrase'' 
  (phrase t1 `S.ofThe` phrase t2)
  (plural t1 `S.ofThe` phrase t2)
  CapFirst
  CapWords

-- | Same as 'ofThe', except prepends "the".
the_ofThe :: (NamedIdea c, NamedIdea d) => c -> d -> NP
the_ofThe t1 t2 = nounPhrase'' 
  (phrase t1 `S.the_ofThe` phrase t2)
  (phrase t1 `S.the_ofThe` plural t2)
  CapFirst
  CapWords

-- | Same as 'the_ofThe', except plural case is @(plural t1) `S.the_ofThe` (phrase t2)@
the_ofThePS :: (NamedIdea c, NamedIdea d) => c -> d -> NP
the_ofThePS t1 t2 = nounPhrase'' 
  (phrase t1 `S.the_ofThe` phrase t2)
  (plural t1 `S.the_ofThe` phrase t2)
  CapFirst
  CapWords

-- | Same as 'of_', except combining Sentence piece is "on the".
onThe :: (NamedIdea c, NamedIdea d) => c -> d -> NP
onThe t1 t2 = nounPhrase'' 
  (phrase t1 `S.onThe` phrase t2)
  (phrase t1 `S.onThe` plural t2)
  CapFirst
  CapWords

-- | Same as 'onThe', except plural case is (plural t1) S.onThe (phrase t2)
onThePS :: (NamedIdea c, NamedIdea d) => c -> d -> NP
onThePS t1 t2 = nounPhrase'' 
  (phrase t1 `S.onThe` phrase t2)
  (plural t1 `S.onThe` phrase t2)
  CapFirst
  CapWords

-- | Same as 'onThe', except plural case is (plural t1) S.onThe (plural t2)
onThePP :: (NamedIdea c, NamedIdea d) => c -> d -> NP
onThePP t1 t2 = nounPhrase'' 
  (phrase t1 `S.onThe` phrase t2)
  (plural t1 `S.onThe` plural t2)
  CapFirst
  CapWords

-- | Creates a 'NP' by combining two 'NamedIdea's with the words "in the" between
-- their terms. Plural case is @(phrase t1) "in the" (plural t2)@.
inThe :: (NamedIdea c, NamedIdea d) => c -> d -> NP
inThe t1 t2 = nounPhrase'' 
  (phrase t1 `S.inThe` phrase t2) 
  (phrase t1 `S.inThe` plural t2)
  CapFirst
  CapWords

-- | Creates a 'NP' by combining two 'NamedIdea's with the words "in the" between
-- their terms. Plural case is @(plural t1) "in the" (phrase t2)@.
inThePS :: (NamedIdea c, NamedIdea d) => c -> d -> NP
inThePS t1 t2 = nounPhrase'' 
  (phrase t1 `S.inThe` phrase t2) 
  (plural t1 `S.inThe` phrase t2)
  CapFirst
  CapWords

-- | Creates a 'NP' by combining two 'NamedIdea's with the words "in the" between
-- their terms. Plural case is @(plural t1) "in the" (plural t2)@.
inThePP :: (NamedIdea c, NamedIdea d) => c -> d -> NP
inThePP t1 t2 = nounPhrase'' 
  (phrase t1 `S.inThe` phrase t2)
  (plural t1 `S.inThe` plural t2)
  CapFirst
  CapWords

-- | Creates a 'NP' by combining two 'NamedIdea's with the words "is the" between
-- their terms. Plural case is @(phrase t1) "is the" (plural t2)@.
isThe :: (NamedIdea c, NamedIdea d) => c -> d -> NP
isThe t1 t2 = nounPhrase'' 
  (phrase t1 `S.isThe` phrase t2) 
  (phrase t1 `S.isThe` plural t2)
  CapFirst
  CapWords

-- | Creates a 'NP' by combining two 'NamedIdea's with the words "to the" between
-- their terms. Plural case is @(phrase t1) "to the" (plural t2)@.
toThe :: (NamedIdea c, NamedIdea d) => c -> d -> NP
toThe t1 t2 = nounPhrase'' 
  (phrase t1 `S.toThe` phrase t2) 
  (phrase t1 `S.toThe` plural t2)
  CapFirst
  CapWords

--FIXME: As mentioned in issue #487, the following should be re-examined later,
--       as they may embody a deeper idea in some cases.

-- we might want to eventually restrict the use of these via
-- some kind of type system, which asserts that:
-- 1. t1 `for` t2 means that t1 is a view of part of the reason behind t2
-- 2. t1 `of_` t2 means that t1 is a view of part of the structure of t2

-- | Creates a 'NP' by combining two 'NamedIdea's with the word "for" between
-- their terms. Plural case is @(phrase t1) "for" (plural t2)@.
for :: (NamedIdea c, NamedIdea d) => c -> d -> NP
for t1 t2 = nounPhrase'' 
  (phrase t1 `S.for` phrase t2)
  (phrase t1 `S.for` plural t2)
  CapFirst
  CapWords

-- | Similar to 'for', but takes two functions that determine the 'titleCase'.
forTGen :: (NamedIdea c, Idea d) => (c -> Sentence) -> (d -> Sentence) -> c -> d -> NP
forTGen f1 f2 t1 t2 = nounPhrase'' 
  (phrase t1 `S.for` phrase t2)
  (plural t1 `S.for` phrase t2)
  CapFirst
  (Replace (f1 t1 `S.for` f2 t2))

-- | Creates a 'NP' by combining two 'NamedIdea's with the word "in" between
-- their terms. Plural case is @(phrase t1) "in" (plural t2)@.
in_ :: (NamedIdea c, NamedIdea d) => c -> d -> NP
in_ t1 t2 = nounPhrase'' 
  (phrase t1 `S.in_` phrase t2)
  (phrase t1 `S.in_` plural t2)
  CapFirst
  CapWords

-- | Same as 'in_', except plural case is @(plural t1) "in" (phrase t2)@.
in_PS :: (NamedIdea c, NamedIdea d) => c -> d -> NP
in_PS t1 t2 = nounPhrase'' 
  (phrase t1 `S.in_` phrase t2)
  (plural t1 `S.in_` phrase t2)
  CapFirst
  CapWords

-- | Creates a 'NP' by combining two 'NamedIdea's with the words "in a" between
-- their terms. Plural case is @(phrase t1) "in a" (plural t2)@.
inA :: (NamedIdea c, NamedIdea d) => c -> d -> NP
inA t1 t2 = nounPhrase'' 
  (phrase t1 +:+ S "in a" +:+ phrase t2) 
  (phrase t1 +:+ S "in a" +:+ plural t2)
  CapFirst
  CapWords

-- | Creates a 'NP' by combining two 'NamedIdea's with the word "is" between
-- their terms. Plural case is @(phrase t1) "is" (plural t2)@.
is :: (NamedIdea c, NamedIdea d) => c -> d -> NP
is t1 t2 = nounPhrase'' 
  (phrase t1 `S.is` phrase t2)
  (phrase t1 `S.is` plural t2)
  CapFirst
  CapWords

-- | Prepends "the" to a 'NamedIdea'.
the :: (NamedIdea t) => t -> NP
the t = nounPhrase'' (S "the" +:+ phrase t) (S "the" +:+ plural t) CapFirst CapWords

-- | A customizable version of 'the'. The given function is applied to both singular and plural cases.
theGen :: (t -> Sentence) -> t -> NP
theGen f t = nounPhrase'' (S "the" +:+ f t) (S "the" +:+ f t) CapFirst CapWords

-- | Prepends "a" to a 'NamedIdea' (similar to 'the').
a_ :: (NamedIdea c) => c -> NP
a_ t = nounPhrase'' (S "a" +:+ phrase t) (S "a" +:+ plural t) CapFirst CapWords

-- | Customizable version of 'a'.
a_Gen :: (c -> Sentence) -> c -> NP
a_Gen f t = nounPhrase'' (S "a" +:+ f t) (S "a" +:+ f t) CapFirst CapWords



-- | Combinator for combining two 'NamedIdeas's into a 'NamedChunk'. 
-- Plural case only makes second term plural. 
-- See 'compoundPhrase' for more on plural behaviour.
-- /Does not preserve abbreviations/.
compoundNC :: (NamedIdea a, NamedIdea b) => a -> b -> NamedChunk
compoundNC t1 t2 = nc
  (t1 ^. uid ++ t2^.uid) (compoundPhrase (t1 ^. term) (t2 ^. term))

-- | Similar to 'compoundNC' but both terms are pluralized for plural case.
compoundNCPP :: (NamedIdea a, NamedIdea b) => a -> b -> NamedChunk
compoundNCPP t1 t2 = nc
  (t1 ^. uid ++ t2 ^. uid) (compoundPhrase'' D.pluralNP D.pluralNP (t1 ^. term) (t2 ^. term))

-- | Similar to 'compoundNC', except plural cases are customizable.
compoundNCGen :: (NamedIdea a, NamedIdea b) => 
  (NP -> Sentence) -> (NP -> Sentence) -> a -> b -> NamedChunk
compoundNCGen f1 f2 t1 t2 = nc
  (t1 ^. uid ++ t2 ^. uid) (compoundPhrase'' f1 f2 (t1 ^. term) (t2 ^. term))

-- | Similar to 'compoundNC', except for plural case, where first parameter gets pluralized while second one stays singular.
compoundNCPS :: NamedChunk -> NamedChunk -> NamedChunk
compoundNCPS = compoundNCGen D.pluralNP D.phraseNP

-- hack for Solution Characteristics Specification, calling upon plural will pluralize
-- Characteristics as it is the end of the first term (solutionCharacteristic)
-- | Similar to 'compoundNC', but takes a function that is applied to the first term (eg. 'short' or 'plural').
compoundNCGenP :: (NamedIdea a, NamedIdea b) => (NP -> Sentence) -> a -> b -> NamedChunk
compoundNCGenP f1 t1 t2 = nc
  (t1 ^. uid ++ t2 ^. uid) (compoundPhrase''' f1 (t1 ^. term) (t2 ^. term))

-- FIXME: Same as above function
-- | Similar to 'compoundNCGenP' but sets first parameter function to plural.
compoundNCPSPP :: NamedChunk -> NamedChunk -> NamedChunk
compoundNCPSPP = compoundNCGenP D.pluralNP

-- | Helper function that combines a 'NamedIdea' and a 'NP' without any words in between.
-- Plural case is @(phrase t1) +:+ (pluralNP t2)@.
combineNINP :: (NamedIdea c) => c -> NP -> NP
combineNINP t1 t2 = nounPhrase'' (phrase t1 +:+ phraseNP t2) (phrase t1 +:+ pluralNP t2) CapFirst CapWords
-- | Similar to 'combineNINP' but takes in a 'NP' first and a 'NamedIdea' second.
combineNPNI :: (NamedIdea c) => NP -> c -> NP
combineNPNI t1 t2 = nounPhrase'' (phraseNP t1 +:+ phrase t2) (phraseNP t1 +:+ plural t2) CapFirst CapWords
-- | Similar to 'combineNINP' but takes two 'NamedIdea's.
combineNINI :: (NamedIdea c, NamedIdea d) => c -> d -> NP
combineNINI t1 t2 = nounPhrase'' (phrase t1 +:+ phrase t2) (phrase t1 +:+ plural t2) CapFirst CapWords
