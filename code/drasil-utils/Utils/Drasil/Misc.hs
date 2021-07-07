{-# Language TypeFamilies, PostfixOperators #-}
module Utils.Drasil.Misc (addPercent, bulletFlat, bulletNested, checkValidStr,
  chgsStart, definedIn, definedIn', definedIn'', definedIn''', displayStrConstrntsAsSet, displayDblConstrntsAsSet,
  eqN, eqnWSource, fromReplace, fromSource, fromSources, fmtU, follows, getTandS,
  itemRefToSent, makeListRef, makeTMatrix, maybeChanged, maybeExpanded,
  maybeWOVerb, mkEnumAbbrevList, mkTableFromColumns, noRefs, refineChain,
  showingCxnBw, sortBySymbol, sortBySymbolTuple, substitute, tAndDOnly,
  tAndDWAcc, tAndDWSym, typUncr, underConsidertn, unwrap, weave, zipSentList, fterms) where

import Language.Drasil
import Language.Drasil.Display
import qualified Language.Drasil.DisplayExpr as DE
import Utils.Drasil.Fold (FoldType(List), SepType(Comma), foldlList, foldlSent)
import qualified Utils.Drasil.Sentence as S (are, in_, is, toThe)

import Control.Lens ((^.))

import Data.Decimal (DecimalRaw, realFracToDecimal)
import Data.Function (on)
import Data.List (sortBy, transpose)

-- | Sorts a list of 'HasSymbols' by Symbol.
sortBySymbol :: HasSymbol a => [a] -> [a]
sortBySymbol = sortBy compareBySymbol

-- | Sorts a tuple list of 'HasSymbols' by first Symbol in the tuple.
sortBySymbolTuple :: HasSymbol a => [(a, b)] -> [(a, b)]
sortBySymbolTuple = sortBy (compareBySymbol `on` fst)

-- | Compares two 'Symbol's and returns their order. See 'compsy' for more information.
compareBySymbol :: HasSymbol a => a -> a -> Ordering
compareBySymbol a b = compsy (eqSymb a) (eqSymb b)

--Ideally this would create a reference to the equation too
--Doesn't use equation concept so utils doesn't depend on data
-- | Prepends the word "Equation" to an 'Int'.
eqN :: Int -> Sentence
eqN n = S "Equation" +:+ sParen (S $ show n)

-- | Takes an expression and a 'Referable' and outputs as a Sentence "expression (source)".
eqnWSource :: (Display e, Referable r, HasShortName r) => e -> r -> Sentence
eqnWSource a b = eS a +:+ sParen (refS b)

-- | Takes a 'Referable' source and a 'UnitalChunk' and outputs as a 'Sentence': "From @source@ we can replace @symbol@:".
fromReplace :: (Referable r, HasShortName r) => r -> UnitalChunk -> Sentence
fromReplace src c = S "From" +:+ refS src `sC` S "we can replace" +: ch c

-- | Takes a list of 'Referable's and 'Symbol's and outputs as a Sentence "By substituting @symbols@, this can be written as:".
substitute :: (Referable r, HasShortName r, HasSymbol r) => [r] -> Sentence
substitute s = S "By substituting" +: (foldlList Comma List l `sC` S "this can be written as")
  where l = map (\x -> ch x +:+ fromSource x) s

-- | Takes a 'HasSymbol' that is also 'Referable' and outputs as a 'Sentence': "@symbol@ is defined in @reference@."
definedIn :: (Referable r, HasShortName r, HasSymbol r) => r -> Sentence
definedIn q = ch q `S.is` S "defined in" +:+. refS q

-- | Same as 'definedIn', but allows for additional information to be appended to the 'Sentence'.
definedIn' :: (Referable r, HasShortName r, HasSymbol r) => r -> Sentence -> Sentence
definedIn' q info = ch q `S.is` S "defined" `S.in_` refS q +:+. info 

-- | Takes a 'Referable' and outputs as a 'Sentence' "defined in @reference@" (no 'HasSymbol').
definedIn'' :: (Referable r, HasShortName r) => r -> Sentence
definedIn'' q =  S "defined" `S.in_` refS q

-- | Takes a 'Symbol' and its 'Reference' (does not append a period at the end!). Outputs as "@symbol@ is defined in @source@".
definedIn''' :: (HasSymbol q, HasUID q, Referable r, HasShortName r) => q -> r -> Sentence
definedIn''' q src = ch q `S.is` S "defined in" +:+ refS src

-- | Zip helper function enumerates abbreviations and zips it with list of 'ItemType':
--
--     * s - the number from which the enumeration should start from ('Integer'),
--     * t - the title of the list ('Sentence'),
--     * l - the list to be enumerated (['Sentence']).
mkEnumAbbrevList :: Integer -> Sentence -> [Sentence] -> [(Sentence, ItemType)]
mkEnumAbbrevList s t l = zip [t :+: S (show x) | x <- [s..]] $ map Flat l

-- | Takes an amount as a 'Sentence' and appends a unit to it.
fmtU :: (MayHaveUnit a) => Sentence -> a -> Sentence
fmtU n u  = n +:+ unwrap (getUnit u)

-- | Extracts the typical uncertainty to be displayed from something that has an uncertainty.
typUncr :: HasUncertainty c => c -> Sentence
typUncr x = found (uncVal x) (uncPrec x)
  where
    found u Nothing  = addPercent $ u * 100
    found u (Just p) = addPercent (realFracToDecimal (fromIntegral p) (u * 100) :: DecimalRaw Integer)

-- | Converts input to a 'Sentence' and appends %.
addPercent :: Show a => a -> Sentence
addPercent num = S (show num) :+: Percent

-- | Distributes a list of Sentences by prepending individual Sentences once to an existing list of Sentences. 
-- 
-- For example: 
--
-- >>> zipSentList [S "Hi", S "Hey", S "Hi"] [[S"Hello"], [S"World"], [S"Hello", S"World]]
-- [[S "Hi", S"Hello"], [S "Hey", S"World"], [S "Hi", S"Hello", S"World]]
zipSentList :: [[Sentence]] -> [Sentence] -> [[Sentence]] -> [[Sentence]] 
zipSentList acc _ []           = acc
zipSentList acc [] r           = acc ++ map (EmptyS:) r
zipSentList acc (x:xs) (y:ys)  = zipSentList (acc ++ [x:y]) xs ys

-- | Makes a traceability matrix from a list of row titles, a list of rows
--   of "checked" columns, and a list of columns.
makeTMatrix :: Eq a => [Sentence] -> [[a]] -> [a] -> [[Sentence]]
makeTMatrix rowName rows cols = zipSentList [] rowName [zipFTable' x cols | x <- rows]
  where
    zipFTable' content = concatMap (\x -> if x `elem` content then [S "X"] else [EmptyS])

-- | Helper for making a table from a columns.
mkTableFromColumns :: [(Sentence, [Sentence])] -> ([Sentence], [[Sentence]])
mkTableFromColumns l = 
  let l' = filter (not . all isEmpty . snd) l in 
  (map fst l', transpose $ map (map replaceEmptyS . snd) l')
  where
    isEmpty       EmptyS = True
    isEmpty       _      = False
    replaceEmptyS EmptyS = S "--"
    replaceEmptyS s      = s

-- | Makes 'Sentence's from an item and its reference. 
-- Takes the title of reference as a 'String' and a 
-- 'Sentence' containing the full reference. Wraps the full reference in parenthesis.
itemRefToSent :: String -> Sentence -> Sentence
itemRefToSent a b = S a +:+ sParen b

-- | Takes a list and a reference, then generates references to 
-- match the length of the list.
makeListRef :: [a] -> Section -> [Sentence]
makeListRef l = replicate (length l) . refS

-- | Applies 'Bullet' and 'Flat' to a list.
bulletFlat :: [Sentence] -> ListType
bulletFlat = Bullet . noRefs . map Flat

-- | Applies 'Bullet's and headers to a 'Nested' 'ListType'. 
-- The first argument is the headers of the 'Nested' lists.
bulletNested :: [Sentence] -> [ListType] -> ListType
bulletNested t l = Bullet (zipWith (\h c -> (Nested h c, Nothing)) t l)

-- | Interweaves two lists together @[[a,b,c],[d,e,f]] -> [a,d,b,e,c,f]@.
weave :: [[a]] -> [a]
weave = concat . transpose

-- | Get a unit symbol if there is one.
unwrap :: Maybe UnitDefn -> Sentence
unwrap (Just a) = Sy $ usymb a
unwrap Nothing  = EmptyS

-- | Converts lists of simple 'ItemType's into a list which may be used
-- in 'Contents' but is not directly referable.
noRefs :: [ItemType] -> [(ItemType, Maybe String)]
noRefs a = zip a $ repeat Nothing

--Doesn't use connection phrase so utils doesn't depend on data
-- | Returns the 'Sentence' "@('titleize' aNamedIdea)@ Showing the Connections Between @contents@".
showingCxnBw :: NamedIdea c => c -> Sentence -> Sentence
showingCxnBw traceyVar contents = titleize traceyVar +:+
  S "Showing the Connections Between" +:+ contents

-- | Returns the 'Sentence' "The @chunk@ under consideration is @chunkDefinition@".
underConsidertn :: ConceptChunk -> Sentence
underConsidertn chunk = S "The" +:+ phrase chunk +:+ 
  S "under consideration is" +:+. (chunk ^. defn)

-- | Create a list in the pattern of "The \_\_ are refined to the \_\_".
-- Note: Order matters!
refineChain :: NamedIdea c => [(c, Section)] -> Sentence
refineChain [x,y] = S "The" +:+ namedRef (snd x) (plural $ fst x) `S.are` S "refined" `S.toThe` plural (fst y)
refineChain (x:y:xs) = (foldlList Comma List (refineChain [x,y] : rc (y : xs)) !.)
  where
    rc [a, b]   = [rcSent a b]
    rc (a:b:as) =  rcSent a b : rc (b : as)
    rc _        = error "refineChain helper encountered an unexpected empty list"
    rcSent a b  = S "the" +:+ namedRef (snd a) (plural $ fst a) `S.toThe` namedRef (snd b) (plural (fst b))
refineChain _ = error "refineChain encountered an unexpected empty list"

-- | Helper functions for making likely change statements. Outputs "The @firstSentence@ may be @someVerb@ @thirdSentence@".
likelyFrame :: Sentence -> Sentence -> Sentence -> Sentence
likelyFrame a verb x = foldlSent [S "The", a, S "may be", verb, x]

-- | Helper functions for making likely change statements. Uses form @'likelyFrame' parameter1 _ parameter2@.
maybeWOVerb, maybeChanged, maybeExpanded :: Sentence -> Sentence -> Sentence
maybeWOVerb a   = likelyFrame a EmptyS
maybeChanged a  = likelyFrame a (S "changed")
maybeExpanded a = likelyFrame a (S "expanded")

-- | Helpful combinators for making 'Sentence's into Terminologies with Definitions.
-- Returns of the form: "@term (abbreviation) - termDefinition@".
tAndDWAcc :: Concept s => s -> ItemType
tAndDWAcc temp = Flat $ atStart temp +:+. (sParen (short temp) `sDash` capSent (temp ^. defn))
-- | Helpful combinators for making 'Sentence's into Terminologies with Definitions.
-- Returns of the form: "@term (symbol) - termDefinition@".
tAndDWSym :: (Concept s, Quantity a) => s -> a -> ItemType
tAndDWSym tD sym = Flat $ atStart tD +:+. (sParen (ch sym) `sDash` capSent (tD ^. defn))
-- | Helpful combinators for making 'Sentence's into Terminologies with Definitions.
-- Returns of the form: "@term - termDefinition@".
tAndDOnly :: Concept s => s -> ItemType
tAndDOnly chunk  = Flat $ atStart chunk `sDash` EmptyS +:+. capSent (chunk ^. defn)

-- | Appends "following @reference@" to the end of a 'Sentence'.
follows :: (Referable r, HasShortName r) => Sentence -> r -> Sentence
preceding `follows` r = preceding +:+ S "following" +:+ refS r

-- | Wraps "from @reference@" in parentheses.
fromSource :: (Referable r, HasShortName r) => r -> Sentence
fromSource r = sParen (S "from" +:+ refS r)

-- | Similar to `fromSource` but takes a list of references instead of one.
fromSources :: (Referable r, HasShortName r) => [r] -> Sentence
fromSources rs = sParen (S "from" +:+ foldlList Comma List (map refS rs))

-- | Used when you want to say a term followed by its symbol. ex. "...using the Force F in...".
getTandS :: (Quantity a) => a -> Sentence
getTandS a = phrase a +:+ ch a

-- TODO: The below 2 functions should eventually creating Sets instead of discrete spaces
-- | Produces a 'Sentence' that displays the constraints in a set {}.
displayStrConstrntsAsSet :: Quantity a => a -> [String] -> Sentence
displayStrConstrntsAsSet sym listOfVals = eS $ 
  DE.isIn (DE.sy sym) (DE.space $ DiscreteS listOfVals)

-- | Produces a 'Sentence' that displays the constraints in a set {}.
displayDblConstrntsAsSet :: Quantity a => a -> [Double] -> Sentence
displayDblConstrntsAsSet sym listOfVals = eS $ 
  DE.isIn (DE.sy sym) (DE.space $ DiscreteD listOfVals)

-- | Output is of the form "@reference - sentence@".
chgsStart :: (HasShortName x, Referable x) => x -> Sentence -> Sentence
chgsStart = sDash . refS

-- | Uses an 'Either' type to check if a 'String' is valid - 
-- 'Left' with error message if there is an invalid 'Char' in 'String', else 'Right' with 'String'.
checkValidStr :: String -> String -> Either String String
checkValidStr s [] = Right s
checkValidStr s (x:xs)
  | x `elem` s = Left $ "Invalid character: \'" ++ [x] ++ "\' in string \"" ++ s ++ ['\"']
  | otherwise  = checkValidStr s xs

-- | Apply a binary function to the terms of two named ideas, instead of to the named
-- ideas themselves. Ex. @fterms compoundPhrase t1 t2@ instead of
-- @compoundPhrase (t1 ^. term) (t2 ^. term)@.
fterms :: (NamedIdea c, NamedIdea d) => (NP -> NP -> t) -> c -> d -> t
fterms f a b = f (a ^. term) (b ^. term)
