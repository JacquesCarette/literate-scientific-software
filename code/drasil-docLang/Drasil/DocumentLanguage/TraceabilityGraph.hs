{-# LANGUAGE PostfixOperators #-}
module Drasil.DocumentLanguage.TraceabilityGraph where

import Language.Drasil
import Database.Drasil hiding (cdb)
import Control.Lens ((^.))
import qualified Data.Map as Map
import Drasil.DocumentLanguage.TraceabilityMatrix (TraceViewCat, traceMReferees, traceMReferrers,
  traceMColumns, ensureItems, layoutUIDs, traceMIntro)
import Drasil.Sections.TraceabilityMandGs (tvAssumps,
  tvDataDefns, tvGenDefns, tvTheoryModels, tvInsModels, tvGoals, tvReqs,
  tvChanges)
import qualified Drasil.DocLang.SRS as SRS
import Language.Drasil.Printers (GraphInfo(..), NodeFamily(..))
import Data.Maybe (fromMaybe)
import Data.Drasil.Concepts.Math (graph)
import Data.Drasil.Concepts.Documentation (traceyGraph, component, dependency, reference, purpose)
import Utils.Drasil
import qualified Utils.Drasil.Sentence as S

-- | Wrapper for 'traceMIntro' and 'traceGIntro'. Turns references ('LabelledContent's), trailing notes ('Sentence's), and any other needed contents to create a 'Section'.
traceMGF :: [LabelledContent] -> [Sentence] -> [Contents] -> [Section] -> Section
traceMGF refs trailing otherContents = SRS.traceyMandG (traceMIntro refs trailing : otherContents 
  ++ map UlC (traceGIntro traceGUIDs (trailing ++ [allvsallDesc])) ++ [traceGCon])

-- | Generalized traceability graph introduction: appends references to the traceability graphs in 'Sentence' form
-- and wraps in 'Contents'. Usually references the five graphs as defined in 'GraphInfo'.
traceGIntro :: [UID] -> [Sentence] -> [UnlabelledContent]
traceGIntro refs trailings = map ulcc [Paragraph $ foldlSent
        [phrase purpose `S.the_ofTheC` plural traceyGraph,
        S "is also to provide easy", plural reference, S "on what has to be",
        S "additionally modified if a certain", phrase component +:+. S "is changed", 
        S "The arrows in the", plural graph, S "represent" +:+. plural dependency,
        S "The", phrase component, S "at the tail of an arrow is depended on by the",
        phrase component, S "at the head of that arrow. Therefore, if a", phrase component,
        S "is changed, the", plural component, S "that it points to should also be changed"] +:+
        foldlSent_ (zipWith graphShows refs trailings)]

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
  | Just (x, _) <- Map.lookup t (s ^. dataDefnTable)        = getRefAdd x
  | Just (x, _) <- Map.lookup t (s ^. insmodelTable)        = getRefAdd x
  | Just (x, _) <- Map.lookup t (s ^. gendefTable)          = getRefAdd x
  | Just (x, _) <- Map.lookup t (s ^. theoryModelTable)     = getRefAdd x
  -- Concept instances can range from likely changes to non-functional requirements, so use domain abbreviations for labelling in addition to the reference address.
  | Just (x, _) <- Map.lookup t (s ^. conceptinsTable)      = fromMaybe "" (getA $ defResolve s $ sDom $ cdom x) ++ ":" ++ getRefAdd x
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

-- FIXME: Should take a Reference instead of just a Reference UID
-- | Helper that makes references of the form "@reference@ shows the dependencies of @something@". Only takes a reference `UID` instead of a `Reference`.
graphShows :: UID -> Sentence -> Sentence
graphShows r end = Ref r EmptyS None +:+ S "shows the" +:+ plural dependency `S.of_` (end !.)

-------- Creating the Tracey Graph Contents to display ------------

-- | Description of the @AllvsAll@ traceability graph.
allvsallDesc :: Sentence
allvsallDesc = S "dependencies of assumptions, models, definitions, requirements, goals, and changes with each other"

-- | Create a list of traceability graph references.
traceGCon :: Contents
traceGCon = UlC $ ulcc $ Enumeration $ Bullet $ zip folderList' $ repeat Nothing

-- | Traceability graph file names.
traceGFiles :: [String]
-- | Traceabiliy graph reference 'UID's.
traceGUIDs :: [UID]
-- | Create reference paths to traceability graphs given an example name.
traceyGraphPaths :: String -> [String]
-- | Create references to traceability graphs given an example name.
traceyGraphGetRefs :: String -> [Reference]

traceGFiles = ["avsa", "avsall", "refvsref", "allvsr", "allvsall"]
traceGUIDs = ["TraceGraphAvsA", "TraceGraphAvsAll", "TraceGraphRefvsRef", "TraceGraphAllvsR", "TraceGraphAllvsAll"]
traceyGraphPaths ex = map (\x -> resourcePath ++ ex ++ "/" ++ x ++ ".pdf") traceGFiles
traceyGraphGetRefs ex = zipWith (\x y -> Reference x (URI y) (shortname' $ S x) None) traceGUIDs $ traceyGraphPaths $ concat $ words ex

-- | Traceability graphs reference path.
resourcePath :: String
resourcePath = "../../../traceygraphs/"

-- | Helper to create a list of traceability graph references.
folderList' :: [ItemType]
folderList' = map (Flat . (\x -> Ref x EmptyS None)) traceGUIDs
