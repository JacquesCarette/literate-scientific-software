-- | A 'Symbol' is actually going to be a graphical description of what
-- gets rendered as a (unique) symbol.  This is actually NOT based on
-- semantics at all, but just a description of how things look.

module Language.Drasil.Symbol (Decoration(..), Symbol(..), compsy) where

import Language.Drasil.Unicode(Special)

import Data.Char (toLower)

-- | Decorations on symbols/characters such as hats or Vector representations
-- (bolding/etc)
data Decoration = Hat | Vector | Prime deriving (Eq, Ord)

-- | Symbols can be:
-- - variable (string such as "x" that represent a value that can vary)
-- - label (strings such as "max" or "target" that represent a single idea)
-- - special characters (ex. unicode)
-- - Decorated symbols
-- - Concatenations of symbols, including subscripts and superscripts
-- - empty! (this is to give this a monoid-like flavour)
data Symbol =
    Variable String
  | Label    String
  | Integ    Int
  | Special  Special
  | Atop     Decoration Symbol
  | Corners  [Symbol] [Symbol] [Symbol] [Symbol] Symbol
          -- upleft   lowleft  upright  lowright base
          -- [1]      [2]      [3]      [4]      [5]
          --  Visually:  [1]   [3]
          --    (out)       [5]
          --             [2]   [4]
  | Concat   [Symbol]
          -- [s1, s2] -> s1s2
  | Empty
  deriving Eq

instance Semigroup Symbol where
 a <> b = Concat [a , b]

instance Monoid Symbol where
  mempty = Empty
  mappend a b = Concat [a , b]

complsy :: [Symbol] -> [Symbol] -> Ordering
complsy [] [] = EQ
complsy [] _  = LT
complsy _  [] = GT
complsy (x : xs) (y : ys) = compsy x y `mappend` complsy xs ys

-- |The default compare function sorts all the lower case after the upper case.
-- Comparation is used twice for each `Atomic` case,
-- once for making sure they are the same letter, once for case sensitive.
-- As far as this comparison is considered, `Δ` is a "decoration" and ignored
-- unless the compared symbols are the exact same, in which case it is ordered
-- after the undecorated symbol.
compsy :: Symbol -> Symbol -> Ordering
compsy (Concat (Variable "Δ" : x)) y =
  case compsy (Concat x) y of
    EQ -> GT
    other -> other
compsy a (Concat (Variable "Δ" : y)) =
  case compsy a (Concat y) of
    EQ -> LT
    other -> other
compsy (Concat x) (Concat y) = complsy x y
compsy (Concat a) b = complsy a [b]
compsy b (Concat a) = complsy [b] a
-- The next two cases are very specific (but common) patterns where a superscript is added
-- to some "conceptual" base symbol to add further context. For example: `v_f^{AB}` (expressed in LaTeX
-- notation for clarity), where `v_f` is a final velocity, and the `^{AB}` adds context that it is the
-- final velocity between points `A` and `B`. In these cases, the sorting of `v_f^{AB}` should be
-- following `v_f` as it is logical to place it with its parent concept.
compsy (Corners [] [] ur [] (Corners [] [] [] lr b)) a = compsy (Corners [] [] ur lr b) a
compsy a (Corners [] [] ur [] (Corners [] [] [] lr b)) = compsy a (Corners [] [] ur lr b)
compsy (Corners _ _ u l b) (Corners _ _ u' l' b')  =
  case compsy b b' of
    EQ -> case complsy l l' of
      EQ -> complsy u u'
      other -> other
    other -> other
compsy a (Corners _ _ _ _ b) =
  case compsy a b of
    EQ -> LT
    other -> other
compsy (Corners _ _ _ _ b) a =
  case compsy b a of
    EQ -> GT
    other -> other
compsy (Atop d1 a) (Atop d2 a') = 
  case compsy a a' of
    EQ -> compare d1 d2
    other -> other
compsy a (Atop _ b) =
  case compsy a b of
    EQ -> LT
    other -> other
compsy (Atop _ b) a =
 case compsy b a of
    EQ -> GT
    other -> other
compsy (Special a)  (Special b)  = compare a b
compsy (Integ    x) (Integ    y) = compare x y
compsy (Variable x) (Variable y) = compsyLower x y
compsy (Variable x) (Label y)    = compsyLower x y
compsy (Label x)    (Variable y) = compsyLower x y
compsy (Label x)    (Label y)    = compsyLower x y
compsy (Special _)  _ = LT
compsy _ (Special _)  = GT
compsy (Integ _) _    = LT
compsy _ (Integ _)    = GT
compsy (Variable _) _ = LT
compsy _ (Variable _) = GT
compsy (Label _) _    = LT
compsy _ (Label _)    = GT
compsy Empty Empty    = EQ

compsyLower :: String -> String -> Ordering
compsyLower x y = case compare (map toLower x) (map toLower y) of
  EQ    -> compare x y 
  other -> other
