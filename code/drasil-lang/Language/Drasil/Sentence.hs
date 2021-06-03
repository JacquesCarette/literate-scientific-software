{-# LANGUAGE GADTs, PostfixOperators, TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
-- | Contains Sentences and helpers
module Language.Drasil.Sentence (Sentence(..), SentenceStyle(..), (+:+),
  (+:+.), (+:), (!.), capSent, ch, sC, sDash, sentencePlural, sentenceShort,
  sentenceSymb, sentenceTerm, sParen,
  Reference(Reference, refInfo), RefInfo(..), HasShortName(shortname),
  ShortName, getStringSN, shortname') where

import Language.Drasil.Classes.Core (HasUID(uid), HasSymbol, HasRefAddress(getRefAdd))
import Language.Drasil.Expr (Expr)
--import Language.Drasil.RefProg (Reference)
import Language.Drasil.Symbol (Symbol)
import Language.Drasil.UnitLang (USymb)
import Language.Drasil.Label.Type (LblType, getAdd)
import Language.Drasil.UID (UID)

import Control.Lens ((^.), makeLenses)

import Data.Char (toUpper)

-- | Used in 'Ch' constructor to determine the state of a 'Sentence'
-- (can record whether something is in plural form, a symbol, a singular term, or in short form).
data SentenceStyle = PluralTerm
                   | SymbolStyle
                   | TermStyle
                   | ShortStyle

-- | For writing 'Sentence's via combining smaller elements.
-- 'Sentence's are made up of some known vocabulary of things:
--
--     * units (their visual representation)
--     * words (via 'String's)
--     * special characters
--     * accented letters
--     * references to specific layout objects
infixr 5 :+:
data Sentence where
  -- | Among other things, Ch allows Sentences to hold plural forms of 'NounPhrase's and 'NamedIdea's.
  Ch    :: SentenceStyle -> UID -> Sentence
  -- | Converts a unit symbol into a usable Sentence form.
  Sy    :: USymb -> Sentence
  -- | Constructor for 'String's, used often for descriptions in Chunks.
  S     :: String -> Sentence
  -- | Converts the graphical representation of a symbol into a usable Sentence form.
  P     :: Symbol -> Sentence       -- should not be used in examples?
  -- | Lifts an expression into a Sentence.
  E     :: Expr -> Sentence
  -- | Lifts a Reference into a Sentence.
  Ref   :: Reference -> Sentence
  -- | Adds quotation marks around a Sentence.
  Quote :: Sentence -> Sentence
  -- | Used for a % symbol.
  Percent :: Sentence
  -- | Direct concatenation of two Sentences (no implicit spaces!).
  (:+:) :: Sentence -> Sentence -> Sentence
  -- | Empty Sentence.
  EmptyS :: Sentence

-- The HasSymbol is redundant, but on purpose
-- | Gets a symbol and places it in a 'Sentence'.
ch :: (HasUID c, HasSymbol c) => c -> Sentence
ch x = Ch SymbolStyle (x ^. uid)

-- | Sentences can be concatenated.
instance Semigroup Sentence where
  (<>) = (:+:)

-- | Sentences can be empty or directly concatenated.
instance Monoid Sentence where
  mempty = EmptyS
  mappend = (:+:)

-- | Smart constructors for turning a 'UID' into a 'Sentence'.
sentencePlural, sentenceShort, sentenceSymb, sentenceTerm :: UID -> Sentence
-- | Gets plural term of 'UID'.
sentencePlural = Ch PluralTerm
-- | Gets short form of 'UID'.
sentenceShort  = Ch ShortStyle
-- | Gets symbol form of 'UID'.
sentenceSymb   = Ch SymbolStyle
-- | Gets singular form of 'UID'.
sentenceTerm   = Ch TermStyle

-- | Helper for wrapping 'Sentence's in parentheses.
sParen :: Sentence -> Sentence
sParen x = S "(" :+: x :+: S ")"

-- | Helper for concatenating two 'Sentence's with a space-surrounded dash between them.
sDash :: Sentence -> Sentence -> Sentence
sDash a b = a +:+ S "-" +:+ b

-- | Helper for concatenating two 'Sentence's with a space between them.
(+:+) :: Sentence -> Sentence -> Sentence
EmptyS +:+ b = b
a +:+ EmptyS = a
a +:+ b = a :+: S " " :+: b

-- | Helper for concatenating two 'Sentence's with a comma and space between them.
sC :: Sentence -> Sentence -> Sentence
a `sC` b = a :+: S "," +:+ b

-- | Helper which concatenates two 'Sentence's using '+:+' and appends a period.
(+:+.) :: Sentence -> Sentence -> Sentence
a +:+. b = a +:+ b :+: S "."

-- | Helper which appends a period to the end of a 'Sentence' (used often as a post-fix operator).
(!.) :: Sentence -> Sentence
(!.) a = a :+: S "."

-- | Helper which concatenates two sentences using '+:+' and appends a colon.
(+:) :: Sentence -> Sentence -> Sentence
a +: b = a +:+ b :+: S ":"

-- | Capitalizes a Sentence.
capSent :: Sentence -> Sentence
capSent (S (s:ss)) = S (toUpper s : ss)
--capSent (phrase x) = atStart x
--capSent (plural x) = atStart' x
capSent (a :+: b)  = capSent a :+: b
capSent x          = x

-----------
-- ShortName and Reference modules here to temporarily solve import cycles
-----------

-- | Used for holding the short form of a name (as a 'Sentence' with a wrapper).
newtype ShortName = ShortNm String

-- | Pulls the short form (as a 'Sentence') out of a 'ShortName'.
getStringSN :: ShortName -> String
getStringSN (ShortNm s) = s

-- | Smart constructor for making a 'String' into a 'ShortName'.
shortname' :: String -> ShortName
shortname' = ShortNm

-- | Holds any extra information needed for a 'Reference', be it an equation, pages, a note, or nothing.
data RefInfo = None
             | Equation [Int]
             | Page [Int]
             | RefNote String

-- | A Reference contains the identifier ('UID'), a reference address ('LblType'),
-- a human-readable shortname ('ShortName'), and any extra information about the reference ('RefInfo').
data Reference = Reference
  { _ui :: UID
  ,  ra :: LblType
  ,  sn :: ShortName
  ,  refInfo :: RefInfo }
makeLenses ''Reference

-- | Finds the 'UID' of a 'Reference'.
instance HasUID        Reference where uid = ui
-- | Finds the reference address contained in a 'Reference' (through a 'LblType').
instance HasRefAddress Reference where getRefAdd = getAdd . ra
-- | Finds the shortname of the reference address used for the 'Reference'.
instance HasShortName  Reference where shortname = sn

------------------------------------------------------
-- | A 'ShortName' is the text to be displayed for a link.
-- Used for referencing within a document that can include symbols and whatnot if required.
-- Visible in the typeset documents (pdf).
class HasShortName  s where
  shortname :: s -> ShortName
