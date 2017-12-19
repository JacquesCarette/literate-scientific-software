module Data.Drasil.SentenceStructures
  ( foldlSent, foldlSent_, foldlSentCol, foldlsC, foldlList
  , sAnd, andIts, andThe, sAre, sIn, sVersus
  , sIs, isThe, sOf, sOr, ofThe, ofThe'
  , ofGiv, ofGiv'
  , toThe, tableShows, figureLabel
  , isExpctdToHv, underConsidertn, showingCxnBw, refineChain
  , foldlSP, foldlSP_, foldlSPCol,foldlOptions
  , maybeChanged, maybeExpanded, maybeWOVerb
  , tAndDWAcc, tAndDWSym, tAndDOnly
  , followA
  , getTandS, getTDS
  , eqN
  , displayConstrntsAsSet
  , fmtInputConstr, fmtOutputConstr, physC, sfwrC, typUncr, rval
  , extrctStrng
  , acroA, acroDD, acroGD, acroGS, acroIM, acroLC, acroPS, acroR, acroT
  ) where

import Language.Drasil
import Data.Drasil.Utils (foldle, foldle1, getES, fmtU, getRVal)
import Data.Drasil.Concepts.Documentation
import Data.Drasil.Concepts.Math (equation)
import Control.Lens ((^.))

{--** Sentence Folding **--}
-- | partial function application of foldle for sentences specifically
foldlSent :: [Sentence] -> Sentence
foldlSent = foldle (+:+) (+:+.) EmptyS

-- | foldlSent but does not end with period
foldlSent_ :: [Sentence] -> Sentence
foldlSent_ = foldle (+:+) (+:+) EmptyS

-- | foldlSent but ends with colon
foldlSentCol :: [Sentence] -> Sentence
foldlSentCol = foldle (+:+) (+:) EmptyS

-- | fold sentences then turns into content
foldlSP :: [Sentence] -> Contents
foldlSP = Paragraph . foldlSent

foldlSP_ :: [Sentence] -> Contents
foldlSP_ = Paragraph . foldlSent_

foldlSPCol :: [Sentence] -> Contents
foldlSPCol = Paragraph . foldlSentCol

-- | creates a list of elements seperated by commas, including the last element
foldlsC :: [Sentence] -> Sentence
foldlsC []       = EmptyS
foldlsC [x]      = x
foldlsC [x,y]    = x `sC` y
foldlsC (x:y:xs) = foldle sC sC (x `sC` y) xs

-- | creates a list of elements seperated by commas, ending in a "_, and _"
foldlList :: [Sentence] -> Sentence
foldlList []    = EmptyS
foldlList [a,b] = a `sAnd` b
foldlList lst   = foldle1 sC (\a b -> a `sC` S "and" +:+ b) lst

-- | creates a list of elements seperated by commas, ending in a "_, or _"
foldlOptions :: [Sentence] -> Sentence
foldlOptions []    = EmptyS
foldlOptions [a,b] = a `sOr` b
foldlOptions lst   = foldle1 sC (\a b -> a `sC` S "or" +:+ b) lst

{--** Combinators **--}
sAnd, andIts :: Sentence -> Sentence -> Sentence
sAnd p1 p2 = p1 +:+ S "and" +:+ p2

andIts p1 p2 = p1 +:+ S "and its" +:+ p2

andThe :: Sentence -> Sentence -> Sentence
andThe p1 p2 = p1 +:+ S "and the" +:+ p2

sAre :: Sentence -> Sentence -> Sentence
sAre p1 p2 = p1 +:+ S "are" +:+ p2

sIn :: Sentence -> Sentence -> Sentence
sIn p1 p2 = p1 +:+ S "in" +:+ p2

sIs :: Sentence -> Sentence -> Sentence
sIs p1 p2 = p1 +:+ S "is" +:+ p2

isThe :: Sentence -> Sentence -> Sentence
isThe p1 p2 = p1 +:+ S "is the" +:+ p2

sOf :: Sentence -> Sentence -> Sentence
sOf p1 p2 = p1 +:+ S "of" +:+ p2

sOr :: Sentence -> Sentence -> Sentence
sOr p1 p2 = p1 +:+ S "or" +:+ p2

sVersus :: Sentence -> Sentence -> Sentence
sVersus p1 p2 = p1 +:+ S "versus" +:+ p2

ofThe, ofThe' :: Sentence -> Sentence -> Sentence
ofThe  p1 p2 = S "the" +:+ p1 +:+ S "of the" +:+ p2
ofThe' p1 p2 = S "The" +:+ p1 +:+ S "of the" +:+ p2

ofGiv, ofGiv' :: Sentence -> Sentence -> Sentence
ofGiv  p1 p2 = S "the" +:+ p1 +:+ S "of a given" +:+ p2
ofGiv' p1 p2 = S "The" +:+ p1 +:+ S "of a given" +:+ p2

toThe :: Sentence -> Sentence -> Sentence
toThe p1 p2 = p1 +:+ S "to the" +:+ p2

{--Acronyms to be used throughout--}
-- ex. S "as seen in (A1)" -> S "as seen in" +:+ sParen (acroA "1")
acroA, acroDD, acroGD, acroGS, acroIM, acroLC, acroPS, acroR, 
  acroT :: Int -> Sentence

acroA  numVar = short assumption  :+: S (show numVar)
acroDD numVar = short dataDefn    :+: S (show numVar)
acroGD numVar = short genDefn     :+: S (show numVar)
acroGS numVar = short goalStmt    :+: S (show numVar)
acroIM numVar = short inModel     :+: S (show numVar)
acroLC numVar = short likelyChg   :+: S (show numVar)
acroPS numVar = short physSyst    :+: S (show numVar)
acroR  numVar = short requirement :+: S (show numVar)
acroT  numVar = short thModel     :+: S (show numVar)


{--** Miscellaneous **--}
tableShows :: Contents -> Sentence -> Sentence
tableShows ref trailing = (makeRef ref) +:+ S "shows the" +:+ 
  plural dependency +:+ S "of" +:+ trailing

-- | Function that creates (a label for) a figure
--FIXME: Is `figureLabel` defined in the correct file?
figureLabel :: NamedIdea c => Int -> c -> Sentence -> [Char]-> Contents
figureLabel num traceyMG contents filePath = Figure (titleize figure +: 
  (S (show num)) +:+ (showingCxnBw (traceyMG) (contents))) filePath 100

showingCxnBw :: NamedIdea c => c -> Sentence -> Sentence
showingCxnBw traceyVar contents = titleize traceyVar +:+ S "Showing the" +:+
  titleize' connection +:+ S "Between" +:+ contents

isExpctdToHv :: Sentence -> Sentence -> Sentence
a `isExpctdToHv` b = S "The" +:+ a +:+ S "is expected to have" +:+ b

underConsidertn :: ConceptChunk -> Sentence
underConsidertn chunk = S "The" +:+ (phrase chunk) +:+ 
  S "under consideration is" +:+. (chunk ^. defn)

-- | Create a list in the pattern of "The __ are refined to the __".
-- Note: Order matters!
refineChain :: NamedIdea c => [c] -> Sentence
refineChain (x:y:[]) = S "The" +:+ plural x +:+ S "are refined to the" +:+ plural y
refineChain (x:y:xs) = refineChain [x,y] `sC` rc ([y] ++ xs)
refineChain _ = error "refineChain encountered an unexpected empty list"

-- | Helper used by refineChain
rc :: NamedIdea c => [c] -> Sentence
rc (x:y:[]) = S "and the" +:+ (plural x) +:+ S "to the" +:+. 
  (plural y)
rc (x:y:xs) = S "the" +:+ plural x +:+ S "to the" +:+ plural y `sC` rc ([y] ++ xs)
rc _ = error "refineChain helper encountered an unexpected empty list"

-- | helper functions for making likely change statements
likelyFrame :: Sentence -> Sentence -> Sentence -> Sentence
likelyFrame a verb x = foldlSent [S "The", a, S "may be", verb, x]
maybeWOVerb, maybeChanged, maybeExpanded :: Sentence -> Sentence -> Sentence
maybeWOVerb a b = likelyFrame a EmptyS b
maybeChanged a b = likelyFrame a (S "changed") b
maybeExpanded a b = likelyFrame a (S "expanded") b

-- | helpful combinators for making Sentences for Terminologies with Definitions
-- term (acc) - definition
tAndDWAcc :: Concept s => s -> ItemType
tAndDWAcc temp = Flat $ ((at_start temp) :+: sParenDash (short temp) :+: (temp ^. defn)) 
-- term (symbol) - definition
tAndDWSym :: (Concept s, Quantity a) => s -> a -> ItemType
tAndDWSym tD sym = Flat $ ((at_start tD) :+: 
  sParenDash (getES sym)) :+: (tD ^. defn)
-- term - definition
tAndDOnly :: Concept s => s -> ItemType
tAndDOnly chunk  = Flat $ ((at_start chunk) +:+ S "- ") :+: (chunk ^. defn)

followA :: Sentence -> Int -> Sentence
preceding `followA` num = preceding +:+ S "following" +:+ acroA num

-- | Used when you want to say a term followed by its symbol. ex. "...using the Force F in..."
getTandS :: (Quantity a, NamedIdea a) => a -> Sentence
getTandS a = phrase a +:+ getES a

-- | get term, definition, and symbol
getTDS :: (Quantity a, Concept a) => a -> Sentence
getTDS a = phrase a +:+ (a ^. defn) +:+ getES a

--Ideally this would create a reference to the equation too
eqN :: Int -> Sentence
eqN n = phrase equation +:+ sParen (S $ show n)

--Produces a sentence that displays the constraints in a {}.
displayConstrntsAsSet :: Quantity a => a -> [String] -> Sentence
displayConstrntsAsSet sym listOfVals = E $ (C sym) `IsIn` (DiscreteS listOfVals)

extrctStrng :: Sentence -> String
extrctStrng (S strng) = strng
extrctStrng _ = error "Invalid type extraction"

{-BELOW IS TO BE MOVED TO EXAMPLE/DRASIL/SECTIONS-}

-- Start of attempt at intelligent formatter for input constraints
-- these are the helper functions for inDataConstTbl

fmtInputConstr :: (UncertainQuantity c, Constrained c, Quantity c) => c -> [c] -> [Sentence]
fmtInputConstr q qlst = [getES q] ++ physC q qlst ++ sfwrC q qlst ++ [fmtU (E $ getRVal q) q] ++ typUncr q qlst

fmtOutputConstr :: (Constrained c, Quantity c) => c -> [c] -> [Sentence]
fmtOutputConstr q qlst = [getES q] ++ physC q qlst ++ sfwrC q qlst

none :: Sentence
none = S "None"

--These check the entire list of UncertainQuantity and if they are all empty in that field,
-- return empty list, otherwise return the appropriate thing
physC :: (Constrained c, Quantity c) => c -> [c] -> [Sentence]
physC q qlst
  | noPhysC (foldlSent_ $ map fmtPhys qlst) = []
  | noPhysC (fmtPhys q) = [none]
  | otherwise = [fmtPhys q]
  where noPhysC EmptyS = True
        noPhysC _      = False
        
sfwrC :: (Constrained c, Quantity c) => c -> [c] -> [Sentence]
sfwrC q qlst
  | noSfwrC (foldlSent_ $ map fmtSfwr qlst) = []
  | noSfwrC (fmtSfwr q) = [none]
  | otherwise = [fmtSfwr q]
  where noSfwrC EmptyS = True
        noSfwrC _      = False

rval :: (Constrained c) => c -> [c] -> [Sentence]
rval q qlst
  | null (filter isRV $ map (^. reasVal) qlst) = []
  | isRV (q ^. reasVal) = [fmtU (E $ getRVal q) q]
  | otherwise = [none]
  where isRV (Just _) = True
        isRV Nothing  = False
        
typUncr :: (UncertainQuantity c) => c -> [c] -> [Sentence]
typUncr q qlst
  | null (filter isUn $ map (^. uncert) qlst) = []
  | isUn (q ^. uncert) = [S $ show $ unwU (q ^. uncert)]
  | otherwise = [none]
  where unwU (Just u) = u
        unwU Nothing  = error $ "Something when wrong with 'typUncr'." ++
                        "'typUncr' was possibly called by fmtInputConstr or inDataConstTbl."
        isUn (Just _) = True
        isUn Nothing  = False

--Formatters for the constraints
fmtPhys :: (Constrained s, Quantity s) => s -> Sentence
fmtPhys s = foldlList $ fmtCP $ filter filterP (s ^. constraints)
  where filterP (Phys _) = True
        filterP _ = False
        fmtCP = map (\(Phys f) -> E $ f (C s))

fmtSfwr :: (Constrained s, Quantity s) => s -> Sentence
fmtSfwr s = foldlList $ fmtCS $ filter filterS (s ^. constraints)
  where filterS (Sfwr _) = True
        filterS _ = False
        fmtCS = map (\(Sfwr f) -> E $ f (C s))
