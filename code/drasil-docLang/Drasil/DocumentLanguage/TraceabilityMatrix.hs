{-# LANGUAGE PostfixOperators #-}
module Drasil.DocumentLanguage.TraceabilityMatrix where

import Language.Drasil
import Database.Drasil(ChunkDB, SystemInformation, UMap, _sysinfodb, asOrderedList,
  conceptinsTable, defResolve, refbyTable, traceTable, traceLookup)
import Utils.Drasil
import qualified Utils.Drasil.Sentence as S

import Data.Drasil.Concepts.Documentation (purpose, component, dependency,
  item, reference, traceyMatrix)

import Drasil.DocumentLanguage.Definitions (helpToRefField)

import Control.Lens ((^.), Getting)
import Data.List (nub)
import qualified Data.Map as Map

-- | Helper type that takes two sets of 'UID's and a 'ChunkDB'.
type TraceViewCat = [UID] -> ChunkDB -> [UID]

-- | Generalized traceability matrix introduction: appends references to the traceability matrices in 'Sentence' form
-- and wraps in 'Contents'. Usually references the four tables generally found in this section (in order of being mentioned).
traceMIntro :: [LabelledContent] -> [Sentence] -> Contents
traceMIntro refs trailings = UlC $ ulcc $ Paragraph $ foldlSent [phrase purpose
        `S.the_ofTheC` plural traceyMatrix, S "is to provide easy", plural reference, 
        S "on what has to be additionally modified if a certain",
        phrase component, S "is changed. Every time a", phrase component, 
        S "is changed, the", plural item, S "in the column of that", 
        phrase component, S "that are marked with an", Quote (S "X"), 
        S "should be modified as well"] +:+ foldlSent_ (zipWith tableShows refs trailings)

-- | Helper that finds the traceability matrix references (things being referenced).
traceMReferees :: ([UID] -> [UID]) -> ChunkDB -> [UID]
traceMReferees f = f . nub . Map.keys . (^. refbyTable)

-- | Helper that finds the traceability matrix references (things that are referring to other things).
traceMReferrers :: ([UID] -> [UID]) -> ChunkDB -> [UID]
traceMReferrers f = f . nub . concat . Map.elems . (^. refbyTable)

-- | Helper that finds the header of a traceability matrix.
traceMHeader :: (ChunkDB -> [UID]) -> SystemInformation -> [Sentence]
traceMHeader f c = map (`helpToRefField` c) $ f $ _sysinfodb c

-- | Helper that finds the headers of the traceability matrix columns.
traceMColHeader :: ([UID] -> [UID]) -> SystemInformation -> [Sentence]
traceMColHeader f = traceMHeader (traceMReferees f)

-- | Helper that finds the headers of the traceability matrix rows.
traceMRowHeader :: ([UID] -> [UID]) -> SystemInformation -> [Sentence]
traceMRowHeader f = traceMHeader (traceMReferrers f)

-- | Helper that makes the columns of a traceability matrix.
traceMColumns :: ([UID] -> [UID]) -> ([UID] -> [UID]) -> ChunkDB -> [[UID]]
traceMColumns fc fr c = map ((\u -> filter (`elem` u) $ fc u) . flip traceLookup (c ^. traceTable)) $ traceMReferrers fr c

-- | Helper that makes references of the form "@reference@ shows the dependencies of @something@".
tableShows :: (Referable a, HasShortName a) => a -> Sentence -> Sentence
tableShows r end = refS r +:+ S "shows the" +:+ plural dependency `S.of_` (end !.)

-- | Generates a traceability table. Takes a 'UID' for the table, a description ('Sentence'), columns ('TraceViewCat'), rows ('TraceViewCat'), and 'SystemInformation'.
generateTraceTableView :: UID -> Sentence -> [TraceViewCat] -> [TraceViewCat] -> SystemInformation -> LabelledContent
generateTraceTableView u _ [] _ _ = error $ "Expected non-empty list of column-view categories for traceability matrix " ++ u
generateTraceTableView u _ _ [] _ = error $ "Expected non-empty list of row-view categories for traceability matrix " ++ u
generateTraceTableView u desc cols rows c = llcc (makeTabRef u) $ Table
  (EmptyS : ensureItems u (traceMColHeader colf c))
  (makeTMatrix (ensureItems u $ traceMRowHeader rowf c) (traceMColumns colf rowf cdb) $ traceMReferees colf cdb)
  (showingCxnBw traceyMatrix desc) True where
    cdb = _sysinfodb c
    colf = layoutUIDs cols cdb
    rowf = layoutUIDs rows cdb

-- | Helper that makes sure the rows and columns of a traceability matrix have substance.
ensureItems :: UID -> [a] -> [a]
ensureItems u [] = error $ "Expected non-empty matrix dimension for traceability matrix " ++ u
ensureItems _ l = l

-- | Helper that finds the layout 'UID's of a traceability matrix.
layoutUIDs :: [TraceViewCat] -> ChunkDB -> [UID] -> [UID]
layoutUIDs a c e = filter (`elem` (Map.keys $ c ^. traceTable)) $ concatMap (\x -> x e c) a

-- | Helper that filters a traceability matrix given a function.
traceViewFilt :: HasUID a => (a -> Bool) -> Getting (UMap a) ChunkDB (UMap a) -> TraceViewCat
traceViewFilt f table _ = map (^. uid) . filter f . asOrderedList . (^. table)

-- | Helper that is similar to 'traceViewFilt', but the filter is always 'True'.
traceView :: HasUID a => Getting (UMap a) ChunkDB (UMap a) -> TraceViewCat
traceView = traceViewFilt (const True)

-- | Turns a 'Concept' into a 'TraceViewCat' via its domain.
traceViewCC :: Concept c => c -> TraceViewCat
traceViewCC dom u c = traceViewFilt (isDomUnder (dom ^. uid) . sDom . cdom) conceptinsTable u c
  where
    isDomUnder :: UID -> UID -> Bool
    isDomUnder filtDom curr
      | filtDom == curr = True
      | not $ null $ getDom curr = isDomUnder filtDom (sDom $ getDom curr)
      | otherwise = False
    getDom :: UID -> [UID]
    getDom curr = cdom $ defResolve c curr
