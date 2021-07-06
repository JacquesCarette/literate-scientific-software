{-# Language TypeFamilies #-}
module Utils.Drasil.Fold (EnumType(..), WrapType(..), SepType(..), FoldType(..),
  foldConstraints, foldlEnumList, foldlList, foldlSP, foldlSP_, foldlSPCol,
  foldlSent, foldlSent_, foldlSentCol, foldlsC, foldNums, numList) where

import Language.Drasil
import qualified Language.Drasil.DisplayExpr as DE
import qualified Utils.Drasil.Sentence as S (and_, or_)

-- | Fold helper function that applies f to all but the last element, applies g to
-- last element and the accumulator.
foldle :: (a -> a -> a) -> (a -> a -> a) -> a -> [a] -> a
foldle _ _ z []     = z
foldle _ g z [x]    = g z x
foldle f g z [x,y]  = g (f z x) y
foldle f g z (x:xs) = foldle f g (f z x) xs

-- | Fold helper function that applies f to all but last element, applies g to last
-- element and accumulator without starting value, does not work for empty list.
foldle1 :: (a -> a -> a) -> (a -> a -> a) -> [a] -> a
foldle1 _ _ []       = error "foldle1 cannot be used with empty list"
foldle1 _ _ [x]      = x
foldle1 _ g [x,y]    = g x y
foldle1 f g (x:y:xs) = foldle f g (f x y) xs

-- | Helper for formatting constraints.
foldConstraints :: Quantity c => c -> [ConstraintE] -> Sentence
foldConstraints _ [] = EmptyS
foldConstraints c e  = E $ DE.and $ map constraintToExpr e
  where
    constraintToExpr (Range _ ri)         = toDispExpr $ realInterval c ri

{--** Sentence Folding **--}
-- | Partial function application of foldle for sentences specifically. folds with spaces and adds "." at the end.
foldlSent :: [Sentence] -> Sentence
foldlSent = foldle (+:+) (+:+.) EmptyS

-- | 'foldlSent' but does not end with period.
foldlSent_ :: [Sentence] -> Sentence
foldlSent_ = foldl (+:+) EmptyS

-- | 'foldlSent' but ends with colon.
foldlSentCol :: [Sentence] -> Sentence
foldlSentCol = foldle (+:+) (+:) EmptyS

-- | Fold sentences then turns into content using 'foldlSent'.
foldlSP :: [Sentence] -> Contents
foldlSP = mkParagraph . foldlSent

-- | Same as 'foldlSP' but uses 'foldlSent_'.
foldlSP_ :: [Sentence] -> Contents
foldlSP_ = mkParagraph . foldlSent_

-- | Same as 'foldlSP' but uses 'foldlSentCol'.
foldlSPCol :: [Sentence] -> Contents
foldlSPCol = mkParagraph . foldlSentCol

-- | Folds a list of elements separated by commas, including the last element.
foldlsC :: [Sentence] -> Sentence 
foldlsC [] = EmptyS
foldlsC xs = foldl1 sC xs

-- | Type that helps determine enumeration method. Can use either numbers, uppercase letters, or lowercase letters.
data EnumType = Numb   | Upper   | Lower 
-- | Type to help wrap a sentence with parenthesis or to add a period at the end.
data WrapType = Parens | Period
-- | Type to help separate words with commas or semicolons.
data SepType  = Comma  | SemiCol
-- | Type to help fold differently between listed items, or if there are options (ex. using "and" or "or" at the end of a list of words).
data FoldType = List   | Options

-- | Creates a list of elements separated by a "separator", ending with "and" or "or".
foldlList :: SepType -> FoldType -> [Sentence] -> Sentence
foldlList _ _ []     = EmptyS
foldlList _ f [a, b] = end f a b
foldlList s f lst    = foldle1 (sep s) (\a b -> end f (sep s a EmptyS) b) lst

-- | Creates a list of elements with "enumerators" in "wrappers" using foldlList.
foldlEnumList :: EnumType -> WrapType -> SepType -> FoldType -> [Sentence] -> Sentence
foldlEnumList e w s l lst = foldlList s l $ zipWith (+:+) (enumList e w $ length lst) lst
  where
    enumList enum wt len = map (wrap wt . S) (take len (chList enum))
    chList Numb  = map show ([1..] :: [Integer])
    chList Upper = map show ['A'..'Z']
    chList Lower = map show ['a'..'z']
    wrap Parens x = sParen x
    wrap Period x = x :+: S "."

-- | Ending type helper functions to foldlList - not exported.
end :: FoldType -> (Sentence -> Sentence -> Sentence)
end List    = S.and_
end Options = S.or_

-- | Separator type helper function to foldlList - not exported.
sep :: SepType -> (Sentence -> Sentence -> Sentence)
sep Comma   = sC
sep SemiCol = \a b -> a :+: S ";" +:+ b

-- | Parses a list of integers into a nice sentence (ie. S "1, 4-7, and 13").
foldNums :: String -> [Int] -> Sentence
foldNums s x = foldlList Comma List $ map S (numList s x)

-- | Parses a list of integers into a list of strings (ie. ["1", "4-7", "13"]).
numList :: String -> [Int] -> [String]
numList _ []  = error "Empty list used with foldNums"
numList _ [y] = [show y]
numList s [y, z]
  | z == y + 1 = [rangeSep y z s]
  | otherwise  = map show [y, z]
numList s (y:z:xs)
  | z == y + 1 = range y z xs
  | otherwise  = show y : numList s (z:xs)
  where
    range a b []   = [rangeSep a b s]
    range a b [n]
      | n == b + 1 = [rangeSep a n s]
      | otherwise  = [rangeSep a b s, show n]
    range a b l@(n:ns)
      | n == b + 1 = range a n ns
      | otherwise  = rangeSep a b s : numList s l

-- | Helper for numList that concatenates integers to strings.
rangeSep :: Int -> Int -> String -> String
rangeSep p q s = show p ++ s ++ show q
