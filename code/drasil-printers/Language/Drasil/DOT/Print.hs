module Language.Drasil.DOT.Print where

--import Drasil.DocLang
import Language.Drasil
import Database.Drasil hiding (cdb)
--import Data.Char (toLower)
import Data.List (nub)
import Control.Lens ((^.), Getting)
import qualified Data.Map as Map
import System.IO
import Data.Drasil.Concepts.Documentation (assumpDom, chgProbDom,
  goalStmtDom, reqDom)
import System.Directory

type Colour = String

-- Brainstorming ideas here
{-
Some (many) of these functions should be in DocLang instead, but for simplicity + debugging they are here.

Ideally, all the functions in doclang create a traceability table for us.
That means all the heavy lifting of collecting the actual information required to 
autogenerate the .dot files would already be done. The labels also appear on the reference names 
(for example, a data definition shows up as DD:dataDefUID), so we could sort the graph
in that manner. But then again, Drasil.Sections.TraceabilityMandGs already does most of this stuff.
However, I think that drasil-docLang isn't compiled until after drasil-printers, 
so that could pose a problem.

Each traceability matrix should be separate for easier viewability

Also, this has the ability to make the type dependency graph generator in the scripts folder way more generalized.
For now though, I think just focus on making them separate but similar.

Some of these functions/types should exist:-}

genDot :: SystemInformation -> IO ()
genDot si = do
    let gi = mkGraphInfo si
    output "TraceyGraph" gi
    return mempty

mkGraphNodes :: [TraceViewCat] -> SystemInformation -> [UID]
mkGraphNodes entries si = (traceGReferees entryF cdb)
    where
        cdb = _sysinfodb si
        entryF = layoutUIDs entries cdb

mkGraphEdges :: [TraceViewCat] -> [TraceViewCat] -> SystemInformation -> [(UID, [UID])]
mkGraphEdges cols rows si = makeTGraph (ensureItems $ traceMRowHeader rowf si) (traceMColumns colf rowf cdb) $ traceMReferees colf cdb --(traceGReferees entryF cdb) (traceGReferrers entryF cdb) -- first list always seems empty. Hmm
    where
        cdb = _sysinfodb si
        colf = layoutUIDs cols cdb
        rowf = layoutUIDs rows cdb

makeTGraph :: [String] -> [[String]] -> [String] -> [(String, [String])]
makeTGraph rowName rows cols = zip rowName [zipFTable' x cols | x <- rows]
  where
    zipFTable' content = concatMap (\x -> if x `elem` content then [x] else [""])

mkGraphInfo :: SystemInformation -> GraphInfo
mkGraphInfo si = GI {
      assumpColour = "red"
    , ddColour     = "blue"
    , gdColour     = "magenta"
    , tmColour     = "green"
    , imColour     = "yellow"
    --, frColour     = "grey"
    --, nfrColour    = "pink"
    , rColour      = "pink"
    , gsColour     = "orange"
    --, lcColour     = "brown"
    --, ucColour     = "teal"
    , cColour      = "cyan"

    {-}, assumpLabels = map ("A_" ++) $ getLabels tvAssumps si
    , ddLabels = map ("DD_" ++) $ getLabels tvDataDefns si
    , gdLabels = map ("GD_" ++) $ getLabels tvGenDefns si
    , tmLabels = map ("TM_" ++) $ getLabels tvTheoryModels si
    , imLabels = map ("IM_" ++) $ getLabels tvInsModels si
    , rLabels = map ("R_" ++) $ getLabels tvReqs si
    , gsLabels = map ("GS_" ++) $ getLabels tvGoals si
    , cLabels = map ("C_" ++) $ getLabels tvChanges si
    , everyLabel = map ("All_" ++) $ getLabels tvEverything si-}

    , assumpLabels = getLabels tvAssumps si
    , ddLabels     = getLabels tvDataDefns si
    , gdLabels     = getLabels tvGenDefns si
    , tmLabels     = getLabels tvTheoryModels si
    , imLabels     = getLabels tvInsModels si
    , rLabels      = getLabels tvReqs si
    , gsLabels     = getLabels tvGoals si
    , cLabels      = getLabels tvChanges si
    , everyLabel   = getLabels tvEverything si

    , directionsAvsA     = mkGraphEdges [tvAssumps] [tvAssumps] si
    , directionsAvsAll   = mkGraphEdges [tvAssumps] [tvDataDefns, tvTheoryModels, tvGenDefns, tvInsModels, tvReqs, tvChanges] si
    , directionsRefvsRef = mkGraphEdges [tvDataDefns, tvTheoryModels, tvGenDefns, tvInsModels] [tvDataDefns, tvTheoryModels, tvGenDefns, tvInsModels] si
    , directionsAllvsR   = mkGraphEdges [tvDataDefns, tvTheoryModels,tvGenDefns, tvInsModels, tvReqs] [tvGoals, tvReqs] si
    , directionsAllvsAll = mkGraphEdges [tvAssumps, tvDataDefns, tvTheoryModels, tvGenDefns, tvInsModels, tvReqs, tvGoals, tvChanges] [tvAssumps, tvDataDefns, tvTheoryModels, tvGenDefns, tvInsModels, tvReqs, tvGoals, tvChanges] si -- [tvEverything] si
    --, sections = ["A", "DD", "GD", "TM", "IM", "FR", "NFR", "GS", "LC", "UC"] -- unused for now
}

getLabels :: TraceViewCat -> SystemInformation -> [UID]
getLabels l si = mkGraphNodes [l] si

-- | Helper that finds the traceability matrix references (things being referenced).
traceGReferees :: ([UID] -> [UID]) -> ChunkDB -> [UID]
traceGReferees f = f . nub . Map.keys . (^. refbyTable)

-- | Helper that finds the traceability matrix references (things that are referring to other things).
traceGReferrers :: ([UID] -> [UID]) -> ChunkDB -> [[UID]]
traceGReferrers f = (map f) . nub . Map.elems . (^. refbyTable)

{-[] _ _ _ = error $ "Expected non-empty list of column-view categories for traceability matrix " ++ u
mkGraphInfo _ [] _ _ = error $ "Expected non-empty list of row-view categories for traceability matrix " ++ u
mkGraphInfo cols rows c gi =
  -----------------zip (ensureItems (traceMColHeader colf c)) --Header row. This is where all the arrows will be pointing from
  makeTGraph (ensureItems $ traceMRowHeader rowf c) (traceMColumns colf rowf cdb) $ traceMReferees colf cdb -- data (rows)
  where
    cdb = _sysinfodb c
    colf = layoutUIDs cols cdb
    rowf = layoutUIDs rows cdb

makeTGraph :: [[String]] -> [String] -> [(String,[String])]
makeTGraph sections possibleDependencies = map (uncurry mTGraphAux) (zip sections possibleDependencies)
  where
    mTGraphAux :: String -> [String] -> (String, [String])
    mtGraphAux s pDep = if s `elem` pDep then
  map (\x -> zipFTable' x dep) indep --[zipFTable' x cols | x <- rows]
  where
    zipFTable' independent = concatMap (\x -> if x `elem` independent then x else [])
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
    rowf = layoutUIDs rows cdb-}

data GraphInfo = GI {
    assumpLabels :: [String] -- Assumption
    , ddLabels   :: [String] -- Data definition
    , gdLabels   :: [String] -- General definition
    , tmLabels   :: [String] -- Theory model
    , imLabels   :: [String] -- Instance model
    --, frLabels   :: [String] -- Functional requirements
    --, nfrLabels  :: [String] -- Non-functional requirements
    , rLabels    :: [String] -- requirements
    , gsLabels   :: [String] -- Goal statements
    --, lcLabels   :: [String] -- Likely changes
    --, ucLabels   :: [String] -- Unlikely changes
    , cLabels    :: [String] -- changes
    , everyLabel :: [String] -- all labels, unused for now since each label has a separate tag

    , directionsAvsA     :: [(String, [String])] -- graph directions
    , directionsAvsAll   :: [(String, [String])]
    , directionsRefvsRef :: [(String, [String])]
    , directionsAllvsR   :: [(String, [String])]
    , directionsAllvsAll :: [(String, [String])]
    --, sections :: [String] -- can be assumptions, TMs, DDs, etc. currently unused

    , assumpColour :: Colour -- give the ability to change colours of bubbles within the graph
    , ddColour     :: Colour
    , gdColour     :: Colour
    , tmColour     :: Colour
    , imColour     :: Colour
    --, frColour     :: Colour
    --, nfrColour    :: Colour
    , rColour      :: Colour
    , gsColour     :: Colour
    --, lcColour     :: Colour
    --, ucColour     :: Colour
    , cColour      :: Colour
    
    -- may need more information regarding ranking & ordering, but for now I'm just keeping it simple
}

-- Output files -- mostly already done from the files in the scripts folder.

-- Does this need to know a filepath? How can we put it in the same place as the build files?
-- Is this a drasil-generator type of thing?
-- Output each graph individually
output :: FilePath -> GraphInfo -> IO ()
output outputFilePath gi = do
    createDirectoryIfMissing False outputFilePath
    setCurrentDirectory outputFilePath
    mkOutputAvsA gi
    mkOutputAvsAll gi
    mkOutputRefvsRef gi
    mkOutputAllvsR gi
    mkOutputAllvsAll gi

mkOutputAvsA :: GraphInfo -> IO ()
mkOutputAvsA gi = do
    handle <- openFile "avsa.dot" WriteMode
    hPutStrLn handle "digraph avsa {"
    outputSubAvsA gi handle
    hPutStrLn handle "}"
    hClose handle

-- since the 'dot' method of displaying graphs naturally groups subgraphs,
--all related ideas could be grouped by section type 
--(eg. assumptions all together, datadefs all together)
outputSubAvsA :: GraphInfo -> Handle -> IO ()
outputSubAvsA gi handle = do
    --hPutStrLn handle ("\tsubgraph " ++ s ++ " {")
    mapM_ (mkDirections handle) (directionsAvsA gi)
    let allLabels = zip3 [assumpLabels gi] ["A: "] [assumpColour gi]
    mapM_ (mkNodes handle) allLabels
    --hPutStrLn handle "\t}"

addLabel :: [String] -> String -> String -> ([String], String)
addLabel lbl col addlbl = (map (addlbl ++) lbl, col)

mkOutputAvsAll :: GraphInfo -> IO ()
mkOutputAvsAll gi = do
    handle <- openFile "avsall.dot" WriteMode
    hPutStrLn handle $ "digraph avsall {"
    outputSubAvsAll gi handle
    hPutStrLn handle "}"
    hClose handle

outputSubAvsAll :: GraphInfo -> Handle -> IO ()
outputSubAvsAll gi handle = do
    --hPutStrLn handle ("\tsubgraph " ++ s ++ " {")
    mapM_ (mkDirections handle) (directionsAvsAll gi)
    --let allLabels = zip [assumpLabels gi, ddLabels gi, tmLabels gi, gdLabels gi, imLabels gi, frLabels gi, nfrLabels gi, lcLabels gi, ucLabels gi] [assumpColour gi, ddColour gi, tmColour gi, gdColour gi, imColour gi, frColour gi, nfrColour gi, lcColour gi, ucColour gi]
    let allLabels = zip3 [assumpLabels gi, ddLabels gi, tmLabels gi, gdLabels gi, imLabels gi, rLabels gi, cLabels gi] ["A: ", "DD: ", "TM: ", "GD: ", "IM: ", "R: ", "C: "] [assumpColour gi, ddColour gi, tmColour gi, gdColour gi, imColour gi, rColour gi, cColour gi]
    mapM_ (mkNodes handle) allLabels
    --hPutStrLn handle "\t}"

mkOutputRefvsRef :: GraphInfo -> IO ()
mkOutputRefvsRef gi = do
    handle <- openFile "refvsref.dot" WriteMode
    hPutStrLn handle $ "digraph refvsref {"
    outputSubRefvsRef gi handle
    hPutStrLn handle "}"
    hClose handle

outputSubRefvsRef :: GraphInfo -> Handle -> IO ()
outputSubRefvsRef gi handle = do
    --hPutStrLn handle ("\tsubgraph " ++ s ++ " {")
    mapM_ (mkDirections handle) (directionsRefvsRef gi)
    let allLabels = zip3 [ddLabels gi, tmLabels gi, gdLabels gi, imLabels gi] ["DD: ", "TM: ", "GD: ", "IM: "] [ddColour gi, tmColour gi, gdColour gi, imColour gi]
    mapM_ (mkNodes handle) allLabels
    --hPutStrLn handle "\t}"


mkOutputAllvsR :: GraphInfo -> IO ()
mkOutputAllvsR gi = do
    handle <- openFile "allvsr.dot" WriteMode
    hPutStrLn handle $ "digraph allvsr {"
    outputSubAllvsR gi handle
    hPutStrLn handle "}"
    hClose handle

outputSubAllvsR :: GraphInfo -> Handle -> IO ()
outputSubAllvsR gi handle = do
    --hPutStrLn handle ("\tsubgraph " ++ s ++ " {")
    mapM_ (mkDirections handle) (directionsAllvsR gi) ----------------- map ($ gi) [assumpLabels ..]?
    --let allLabels = zip [assumpLabels gi, ddLabels gi, tmLabels gi, gdLabels gi, imLabels gi, frLabels gi, nfrLabels gi, gsLabels gi] [assumpColour gi, ddColour gi, tmColour gi, gdColour gi, imColour gi, frColour gi, nfrColour gi, gsColour gi]
    let allLabels = zip3 [assumpLabels gi, ddLabels gi, tmLabels gi, gdLabels gi, imLabels gi, rLabels gi, gsLabels gi] ["A: ", "DD: ", "TM: ", "GD: ", "IM: ", "R: ", "GS: "] [assumpColour gi, ddColour gi, tmColour gi, gdColour gi, imColour gi, rColour gi, gsColour gi]
    mapM_ (mkNodes handle) allLabels
    --hPutStrLn handle "\t}"

mkOutputAllvsAll :: GraphInfo -> IO ()
mkOutputAllvsAll gi = do
    handle <- openFile "allvsall.dot" WriteMode
    hPutStrLn handle $ "digraph allvsall {"
    outputSubAllvsAll gi handle
    hPutStrLn handle "}"
    hClose handle

outputSubAllvsAll :: GraphInfo -> Handle -> IO ()
outputSubAllvsAll gi handle = do
    --hPutStrLn handle ("\tsubgraph " ++ s ++ " {")
    mapM_ (mkDirections handle) (directionsAllvsAll gi) ----------------- map ($ gi) [assumpLabels ..]?
    --let allLabels = zip [assumpLabels gi, ddLabels gi, tmLabels gi, gdLabels gi, imLabels gi, frLabels gi, nfrLabels gi, gsLabels gi] [assumpColour gi, ddColour gi, tmColour gi, gdColour gi, imColour gi, frColour gi, nfrColour gi, gsColour gi]
    let allLabels = zip3 [assumpLabels gi, ddLabels gi, tmLabels gi, gdLabels gi, imLabels gi, rLabels gi, gsLabels gi, cLabels gi] ["A: ", "DD: ", "TM: ", "GD: ", "IM: ", "R: ", "GS: ", "C: "] [assumpColour gi, ddColour gi, tmColour gi, gdColour gi, imColour gi, rColour gi, gsColour gi, cColour gi]
    mapM_ (mkNodes handle) allLabels
    --hPutStrLn handle "\t}"

{-- since the 'dot' method of displaying graphs naturally groups subgraphs,
all related ideas could be grouped by s type 
(eg. assumptions all together, datadefs all together)
outputSub :: GraphInfo -> Handle -> String -> IO ()
outputSub gi handle s = do
    hPutStrLn handle ("\tsubgraph " ++ s ++ " {")
    mapM_ (mkDirections handle) (directions gi)
    let allLabels = zip [assumpLabels gi, ddLabels gi, tmLabels gi, ...] [assumpColour gi, ddColour gi, ...]
    mapM_ (uncurry (mkNodes handle)) allLabels
    hPutStrLn handle "\t}"-}

mkDirections :: Handle -> (String, [String]) -> IO ()
mkDirections handle ls = do
    mapM_ (hPutStrLn handle) $ makeEdgesSub (fst ls) (filter (not . null) $ snd ls)
    where
       -- Creates an edge between a type and its dependency (indented for subgraphs)
        makeEdgesSub :: String -> [String] -> [String]
        makeEdgesSub _ [] = []
        makeEdgesSub nm (c:cs) = ("\t" ++ nm ++ " -> " ++ c ++ ";"): makeEdgesSub nm cs

mkNodes :: Handle -> ([String], String, Colour) -> IO () -- uncurry3 doesn't really work, so just change it to take in a tuple instead
mkNodes handle (ls, lbl, col) = do
    mapM_ ((hPutStrLn handle) . (makeNodesSub col lbl)) ls
    where
        -- Creates a node based on the kind of datatype (indented for subgraphs)
        makeNodesSub :: Colour -> String -> String -> String
        makeNodesSub c l nm  = "\t" ++ nm ++ "\t[shape=oval, color=" ++ c ++ ", label=" ++ l ++ nm ++ "];"


----------- Helper functions taken from other parts of drasil. Modified versions could be useful here.-----------

-- | Checker for uids by finding if the 'UID' is in one of the possible data sets contained in the 'SystemInformation' database.
checkUID :: UID -> SystemInformation -> UID
checkUID t si
  | t `elem` Map.keys (s ^. dataDefnTable)        = datadefnLookup    t (s ^. dataDefnTable) ^. uid
  | t `elem` Map.keys (s ^. insmodelTable)        = insmodelLookup    t (s ^. insmodelTable) ^. uid
  | t `elem` Map.keys (s ^. gendefTable)          = gendefLookup      t (s ^. gendefTable) ^. uid
  | t `elem` Map.keys (s ^. theoryModelTable)     = theoryModelLookup t (s ^. theoryModelTable) ^. uid
  | t `elem` Map.keys (s ^. conceptinsTable)      = conceptinsLookup  t (s ^. conceptinsTable) ^. uid
  | t `elem` Map.keys (s ^. sectionTable)         = sectionLookup     t (s ^. sectionTable) ^. uid
  | t `elem` Map.keys (s ^. labelledcontentTable) = labelledconLookup t (s ^. labelledcontentTable)  ^. uid
  | t `elem` map  (^. uid) (citeDB si) = ""
  | otherwise = error $ t ++ "Caught."
  where s = _sysinfodb si

-- | Helper that finds the traceability matrix references (things being referenced).
traceMReferees :: ([UID] -> [UID]) -> ChunkDB -> [UID]
traceMReferees f = f . nub . Map.keys . (^. refbyTable)

-- | Helper that finds the traceability matrix references (things that are referring to other things).
traceMReferrers :: ([UID] -> [UID]) -> ChunkDB -> [UID]
traceMReferrers f = f . nub . concat . Map.elems . (^. refbyTable)

-- | Helper that finds the header of a traceability matrix.
traceMHeader :: (ChunkDB -> [UID]) -> SystemInformation -> [UID]
traceMHeader f c = map (`checkUID` c) $ f $ _sysinfodb c

-- | Helper that finds the headers of the traceability matrix columns.
traceMColHeader :: ([UID] -> [UID]) -> SystemInformation -> [UID]
traceMColHeader f = traceMHeader (traceMReferees f)

-- | Helper that finds the headers of the traceability matrix rows.
traceMRowHeader :: ([UID] -> [UID]) -> SystemInformation -> [UID]
traceMRowHeader f = traceMHeader (traceMReferrers f)

-- | Helper that makes the columns of a traceability matrix.
traceMColumns :: ([UID] -> [UID]) -> ([UID] -> [UID]) -> ChunkDB -> [[UID]]
traceMColumns fc fr c = map ((\u -> filter (`elem` u) $ fc u) . flip traceLookup (c ^. traceTable)) $ traceMReferrers fr c

{-- | Generates a traceability table. Takes a 'UID' for the table, a description ('Sentence'), columns ('TraceViewCat'), rows ('TraceViewCat'), and 'SystemInformation'.
generateTraceTableView :: UID -> Sentence -> [TraceViewCat] -> [TraceViewCat] -> SystemInformation -> LabelledContent
generateTraceTableView u _ [] _ _ = error $ "Expected non-empty list of column-view categories for traceability matrix " ++ u
generateTraceTableView u _ _ [] _ = error $ "Expected non-empty list of row-view categories for traceability matrix " ++ u
generateTraceTableView u desc cols rows c = llcc (makeTabRef u) $ Table
  (EmptyS : ensureItems u (traceMColHeader colf c))
  (makeTMatrix (ensureItems u $ traceMRowHeader rowf c) (traceMColumns colf rowf cdb) $ traceMReferees colf cdb)
  (showingCxnBw traceyMatrix desc) True where
    cdb = _sysinfodb c
    colf = layoutUIDs cols cdb
    rowf = layoutUIDs rows cdb-}

-- | Helper type that takes two sets of 'UID's and a 'ChunkDB'.
type TraceViewCat = [UID] -> ChunkDB -> [UID]

-- | Helper that makes sure the rows and columns of a traceability matrix have substance.
ensureItems :: [a] -> [a]
ensureItems [] = error $ "Expected non-empty matrix dimension for traceability matrix."
ensureItems l = l

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

-- | Traceabiliy viewing everything. Takes a 'UID' and a 'ChunkDB'. Returns a list of 'UID's.
tvEverything :: TraceViewCat
tvEverything = flip (const id)
-- | Traceabiliy viewing assumptions. Takes a 'UID' and a 'ChunkDB'. Returns a list of 'UID's.
tvAssumps :: TraceViewCat
tvAssumps = traceViewCC assumpDom
-- | Traceabiliy viewing data definitions. Takes a 'UID' and a 'ChunkDB'. Returns a list of 'UID's.
tvDataDefns :: TraceViewCat
tvDataDefns = traceView dataDefnTable
-- | Traceabiliy viewing general definitions. Takes a 'UID' and a 'ChunkDB'. Returns a list of 'UID's.
tvGenDefns :: TraceViewCat
tvGenDefns = traceView gendefTable
-- | Traceabiliy viewing theory models. Takes a 'UID' and a 'ChunkDB'. Returns a list of 'UID's.
tvTheoryModels :: TraceViewCat
tvTheoryModels = traceView theoryModelTable
-- | Traceabiliy viewing instance models. Takes a 'UID' and a 'ChunkDB'. Returns a list of 'UID's.
tvInsModels :: TraceViewCat
tvInsModels = traceView insmodelTable
-- | Traceabiliy viewing goals. Takes a 'UID' and a 'ChunkDB'. Returns a list of 'UID's.
tvGoals :: TraceViewCat
tvGoals = traceViewCC goalStmtDom
-- | Traceabiliy viewing requirements. Takes a 'UID' and a 'ChunkDB'. Returns a list of 'UID's.
tvReqs :: TraceViewCat
tvReqs = traceViewCC reqDom
-- | Traceabiliy viewing changes. Takes a 'UID' and a 'ChunkDB'. Returns a list of 'UID's.
tvChanges :: TraceViewCat
tvChanges = traceViewCC chgProbDom