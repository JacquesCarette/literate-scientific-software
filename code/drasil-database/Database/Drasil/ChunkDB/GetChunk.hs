-- | Utilities to get grab certain chunks (from Expr, Sentence, etc) by UID and
-- dereference the chunk it refers to.
module Database.Drasil.ChunkDB.GetChunk (ccss, ccss', combine, getIdeaDict, vars) where

import Language.Drasil
import Language.Drasil.Display.Development (deDep)

import Database.Drasil.ChunkDB (ChunkDB, defResolve, symbResolve, termResolve)

import Data.List (nub)

-- | Gets a list of quantities ('QuantityDict') from an equation in order to print.
vars :: DisplayExpr -> ChunkDB -> [QuantityDict]
vars e m = map (symbResolve m) $ deDep e

-- | Gets a list of quantities ('QuantityDict') from a 'Sentence' in order to print.
vars' :: Sentence -> ChunkDB -> [QuantityDict]
vars' a m = map (symbResolve m) $ sdep a

-- | Combines the functions of 'vars' and 'concpt' to create a list of 'DefinedQuantityDict's from a 'Sentence'.
combine :: Sentence -> ChunkDB -> [DefinedQuantityDict]
combine a m = zipWith dqdQd (vars' a m) (concpt a m)

-- | Combines the functions of 'vars' and 'concpt' to create a list of 'DefinedQuantityDict's from an equation.
combine' :: DisplayExpr -> ChunkDB -> [DefinedQuantityDict]
combine' a m = zipWith dqdQd (vars a m) (concpt' a m)

-- | Gets a list of defined quantities ('DefinedQuantityDict's) from 'Sentence's and expressions that are contained in the database ('ChunkDB').
ccss :: [Sentence] -> [DisplayExpr] -> ChunkDB -> [DefinedQuantityDict]
ccss s e c = nub $ concatMap (`combine` c) s ++ concatMap (`combine'` c) e

-- | Gets a list of quantities ('QuantityDict's) from 'Sentence's and expressions that are contained in the database ('ChunkDB').
ccss' :: [Sentence] -> [DisplayExpr] -> ChunkDB -> [QuantityDict]
ccss' s e c = nub $ concatMap (`vars'` c) s ++ concatMap (`vars` c) e

-- | Gets a list of concepts ('ConceptChunk') from a 'Sentence' in order to print.
concpt :: Sentence -> ChunkDB -> [ConceptChunk]
concpt a m = map (defResolve m) $ sdep a

-- | Gets a list of concepts ('ConceptChunk') from an expression in order to print.
concpt' :: DisplayExpr -> ChunkDB -> [ConceptChunk]
concpt' a m = map (defResolve m) $ deDep a

-- | Gets a list of ideas ('IdeaDict') from a 'Sentence' in order to print.
getIdeaDict :: Sentence -> ChunkDB -> [IdeaDict]
getIdeaDict a m = map (termResolve m) $ shortdep a
