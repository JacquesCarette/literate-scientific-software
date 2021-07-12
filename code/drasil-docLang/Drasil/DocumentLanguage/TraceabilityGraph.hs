module Drasil.DocumentLanguage.TraceabilityGraph (mkGraphInfo) where

import Language.Drasil
import Database.Drasil hiding (cdb)
import Control.Lens ((^.))
import qualified Data.Map as Map
import Drasil.DocumentLanguage.TraceabilityMatrix (TraceViewCat, traceMReferees, traceMReferrers,
  traceMColumns, ensureItems, layoutUIDs)
import Drasil.Sections.TraceabilityMandGs (tvAssumps,
  tvDataDefns, tvGenDefns, tvTheoryModels, tvInsModels, tvGoals, tvReqs,
  tvChanges)
import Language.Drasil.Printers (GraphInfo(..), NodeFamily(..))
import Data.Maybe (fromMaybe)

-- | Extracts traceability graph inforomation from filled-in 'SystemInformation'.
mkGraphInfo :: SystemInformation -> GraphInfo
mkGraphInfo si = GI {
    assumpNF = mkGraphNodes tvAssumps si "mistyrose"
    , ddNF     = mkGraphNodes tvDataDefns si "paleturquoise1"
    , gdNF     = mkGraphNodes tvGenDefns si "palegreen"
    , tmNF     = mkGraphNodes tvTheoryModels si "pink"
    , imNF     = mkGraphNodes tvInsModels si "khaki1"
    , reqNF    = mkGraphNodes tvReqs si "ivory"
    , gsNF     = mkGraphNodes tvGoals si "darkgoldenrod1"
    , chgNF    = mkGraphNodes tvChanges si "lavender"

    , edgesAvsA     = mkGraphEdges [tvAssumps] [tvAssumps] si
    , edgesAvsAll   = mkGraphEdges [tvAssumps] [tvDataDefns, tvTheoryModels, tvGenDefns, tvInsModels, tvReqs, tvChanges] si
    , edgesRefvsRef = mkGraphEdges [tvDataDefns, tvTheoryModels, tvGenDefns, tvInsModels] [tvDataDefns, tvTheoryModels, tvGenDefns, tvInsModels] si
    , edgesAllvsR   = mkGraphEdges [tvDataDefns, tvTheoryModels,tvGenDefns, tvInsModels, tvReqs] [tvGoals, tvReqs] si
    , edgesAllvsAll = mkGraphEdges [tvAssumps, tvDataDefns, tvTheoryModels, tvGenDefns, tvInsModels, tvReqs, tvGoals, tvChanges] [tvAssumps, tvDataDefns, tvTheoryModels, tvGenDefns, tvInsModels, tvReqs, tvGoals, tvChanges] si
}

-- | Gets the node family of a graph based on the given section
-- and system information. Also applies a given colour to the node family.
mkGraphNodes :: TraceViewCat -> SystemInformation -> String -> NodeFamily
mkGraphNodes entry si col = NF {nodeUIDs = nodeContents, nodeLabels = map (checkUIDRefAdd si) nodeContents, nfLabel = checkNodeContents nodeContents, nfColour = col}
    where
        checkNodeContents :: [String] -> String
        checkNodeContents [] = ""
        checkNodeContents (x:_) = checkUIDAbbrev si x
        nodeContents = traceMReferees entryF cdb
        cdb = _sysinfodb si
        entryF = layoutUIDs [entry] cdb

-- | Creates the graph edges based on the relation of the first list of sections to the second.
-- Also needs the system information. Return value is of the form (Section, [Dependencies]).
mkGraphEdges :: [TraceViewCat] -> [TraceViewCat] -> SystemInformation -> [(UID, [UID])]
mkGraphEdges cols rows si = makeTGraph (ensureItems "Traceability Graph" $ traceGRowHeader rowf si) (traceMColumns colf rowf cdb) $ traceMReferees colf cdb
    where
        cdb = _sysinfodb si
        colf = layoutUIDs cols cdb
        rowf = layoutUIDs rows cdb

-- | Helper for making graph edges. Taken from Utils.Drasil's traceability matrix relation finder.
-- But, instead of marking "X" on two related ideas, it makes them an edge.
makeTGraph :: [String] -> [[String]] -> [String] -> [(String, [String])]
makeTGraph rowName rows cols = zip rowName [zipFTable' x cols | x <- rows]
  where
    zipFTable' content = filter (`elem` content)

-- | Checker for uids by finding if the 'UID' is in one of the possible data sets contained in the 'SystemInformation' database.
checkUID :: UID -> SystemInformation -> UID
checkUID t si
  | Just _ <- Map.lookupIndex t (s ^. dataDefnTable)        = t
  | Just _ <- Map.lookupIndex t (s ^. insmodelTable)        = t
  | Just _ <- Map.lookupIndex t (s ^. gendefTable)          = t
  | Just _ <- Map.lookupIndex t (s ^. theoryModelTable)     = t
  | Just _ <- Map.lookupIndex t (s ^. conceptinsTable)      = t
  | Just _ <- Map.lookupIndex t (s ^. sectionTable)         = t
  | Just _ <- Map.lookupIndex t (s ^. labelledcontentTable) = t
  | t `elem` map  (^. uid) (citeDB si) = ""
  | otherwise = error $ t ++ "Caught."
  where s = _sysinfodb si

-- | Similar to 'checkUID' but prepends domain for labelling.
checkUIDAbbrev :: SystemInformation -> UID -> String
checkUIDAbbrev si t
  | Just (x, _) <- Map.lookup t (s ^. dataDefnTable)        = abrv x
  | Just (x, _) <- Map.lookup t (s ^. insmodelTable)        = abrv x
  | Just (x, _) <- Map.lookup t (s ^. gendefTable)          = abrv x
  | Just (x, _) <- Map.lookup t (s ^. theoryModelTable)     = abrv x
  | Just (x, _) <- Map.lookup t (s ^. conceptinsTable)      = fromMaybe "" $ getA $ defResolve s $ sDom $ cdom x
  | Just _ <- Map.lookup t (s ^. sectionTable)         = t -- shouldn't really reach these cases
  | Just _ <- Map.lookup t (s ^. labelledcontentTable) = t
  | t `elem` map  (^. uid) (citeDB si) = ""
  | otherwise = error $ t ++ "Caught."
  where s = _sysinfodb si

-- | Similar to 'checkUID' but gets reference addresses for display.
checkUIDRefAdd :: SystemInformation -> UID -> String
checkUIDRefAdd si t
  | Just (x, _) <- Map.lookup t (s ^. dataDefnTable)        = getAdd $ getRefAdd x
  | Just (x, _) <- Map.lookup t (s ^. insmodelTable)        = getAdd $ getRefAdd x
  | Just (x, _) <- Map.lookup t (s ^. gendefTable)          = getAdd $ getRefAdd x
  | Just (x, _) <- Map.lookup t (s ^. theoryModelTable)     = getAdd $ getRefAdd x
  -- Concept instances can range from likely changes to non-functional requirements, so use domain abbreviations for labelling in addition to the reference address.
  | Just (x, _) <- Map.lookup t (s ^. conceptinsTable)      = fromMaybe "" (getA $ defResolve s $ sDom $ cdom x) ++ ":" ++ getAdd (getRefAdd x)
  | Just _ <- Map.lookup t (s ^. sectionTable)         = t -- shouldn't really reach these cases
  | Just _ <- Map.lookup t (s ^. labelledcontentTable) = t
  | t `elem` map  (^. uid) (citeDB si) = ""
  | otherwise = error $ t ++ "Caught."
  where s = _sysinfodb si

-- | Helper that finds the header of a traceability matrix.
-- However, here we use this to get a list of 'UID's for a traceability graph instead.
traceGHeader :: (ChunkDB -> [UID]) -> SystemInformation -> [UID]
traceGHeader f c = map (`checkUID` c) $ f $ _sysinfodb c

-- | Helper that finds the headers of the traceability matrix rows.
-- However, here we use this to get a list of 'UID's for a traceability graph instead.
-- This is then used to create the graph edges.
traceGRowHeader :: ([UID] -> [UID]) -> SystemInformation -> [UID]
traceGRowHeader f = traceGHeader (traceMReferrers f)
