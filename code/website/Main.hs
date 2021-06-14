{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Main where

import Control.Monad (filterM)
import Data.Char (toUpper)
import Data.List (isInfixOf, isSuffixOf, sort, zip5, groupBy)
import Data.Maybe (fromMaybe, fromJust, isJust)
import Hakyll
import StringUtils (rstrip)
import System.Directory (doesDirectoryExist, doesFileExist, findFiles, listDirectory)
import System.Environment (getEnv, lookupEnv)
import System.FilePath (takeBaseName, takeDirectory, takeExtension)

-- type FilePath = String -- from System.FilePath
type Name = String
data CodeSource = CS {codePath :: FilePath, 
                      doxPath :: FilePath, 
                      versionName :: String,
                      langName :: String}
type SRSVariants = [(Name, String)]
type Description = Maybe String
data Example = E Name [CodeSource] SRSVariants Description [String]
-- data DocsExist = CPP Bool | CSharp Bool | Python Bool | Java Bool | Swift Bool

-- Returns FilePath of the SRS
exDirPath :: FilePath -> FilePath -> FilePath -> FilePath
exDirPath baseDir ex dir = baseDir ++ ex ++ "/" ++ dir

-- Returns the Uppercase String of the given extension
getExtensionName :: String -> String
getExtensionName [] = error "Expected some file extension. Got none."
getExtensionName [_] = error "Expected file extension. Got a single character."
getExtensionName ('.':xs) = map toUpper xs
getExtensionName _ = error "Expected some extension."

-- returns a list of tuples, where each tuple is (Name, string) where string is "HTML" or "PDF"
getSRS :: [Name] -> SRSVariants
getSRS = map (\x -> (x, getExtensionName $ takeExtension x)) . filter (\x -> any (`isSuffixOf` x) [".pdf", ".html"])

-- returns a CodeSource with the path to the source code, doxygen page, version name if the corresponding example has multiple versions, and the name of the language
getSrc :: [String] -> String -> FilePath -> CodeSource
-- source comes in as a path, takeBaseName takes only rightmost folder name
getSrc names repoRoot source = CS (repoRoot ++ source)
  ((if null verName then "" else verName ++ "/") ++ takeBaseName source ++ 
    "/index.html")
  verName
  (lang $ takeBaseName source)
  where
    eDir = takeBaseName $ takeDirectory $ takeDirectory source
    verName = if any (`isInfixOf` eDir) names then eDir else ""
    {-doxygenCheck :: FilePath -> FilePath
    doxygenCheck p 
      | (takeBaseName (takeDirectory p)) == "swift" || (takeBaseName (takeDirectory p)) == "Swift" = "" 
      | otherwise = p-}

-- Some languages might be named differently than the folders they are stored in.
-- Also used below now.
lang :: String -> String
lang "cpp"    = "C++"
lang "csharp" = "C#"
lang "python" = "Python"
lang "java"   = "Java"
lang "swift"  = "Swift"
lang x        = error ("No given display name for language: " ++ x)

{--- Checks a CodeSource to make sure the documentation path exists.
filePathCheckH :: Bool -> String -> DocsExist
filePathCheckH exist name
  | name == "C++" = CPP exist
  | name == "C#" = CSharp exist
  | name == "Python" = Python exist
  | name == "Java" = Java exist
  | name == "Swift" = Swift exist
  | otherwise = error ("Not a valid language: " ++ name)
--filePathCheck dPath = if takeBaseName dPath == "index" then dPath else ""

-- Meant to help get the boolean value by matching the correct source.
-- However, projectile has more than 5 DocsExist so it doesn't really work at all.
-- I'll have to spend a little more time figuring this one out.
-- For now, I'm just trying to get it to compile so it will be really janky.
selectSrc :: [DocsExist] -> String -> DocsExist
selectSrc [a,b,c,d,e] name 
  | name == "C++" = a
  | name == "C#" = b
  | name == "Python" = c
  | name == "Java" = d
  | name == "Swift" = e
  | otherwise = error ("Not a valid language: " ++ name)
selectSrc x name
  | length x == 25 = selectSrc (take 5 x) name
  | otherwise      = error (show (length x) ++ " DocsExist found. Expected 5 DocsExist for " ++ name)

-- Meant to help get the boolean value
getBool :: DocsExist -> Bool
getBool (CPP x) = x
getBool (CSharp x) = x
getBool (Python x) = x
getBool (Java x) = x
getBool (Swift x) = x

filePathCheck :: [Bool] -> [String] -> [DocsExist]
filePathCheck bs ss = map (uncurry filePathCheckH) $ zip bs ss-}

---filePathH :: [([Bool], ([FilePath], [String]))] -> [([Bool], [FilePath], [String])]
--filePathH [] = []
--filePathH (bs, (fs, ns)):xs = filePathH2 (bs, fs, ns): xs
---filePathH fs = map (\(xs, (ys,zs))-> (xs,ys,zs)) fs
--filePathH fs = map (uncurry zip3 (\(xs, (ys,zs))-> (xs,ys,zs))) fs

--filePathH2 :: ([Bool], [FilePath], [String]) -> [(Bool, FilePath, String)]
--filePathH2 (b:bs, f:fs, n:ns) = (b,f,n): filePathH2 (bs,fs,ns)
--filePathH2 _ = []

-- Some examples dont generate in all languages so this is meant to fill in the missing ones with a False DocsExist
-- It doesn't really work either though.
{-fillMissingCheck :: [[DocsExist]] -> [[DocsExist]]
fillMissingCheck ds = map (\x -> missingCheck x langs) ds
  where missingCheck :: [DocsExist] -> [String]-> [DocsExist] 
        missingCheck [] []= []
        missingCheck x [] = x -- error ("Should not exceed number of available languages: " ++ show (length x))
        missingCheck [] ls = map (\x -> filePathCheckH False x) ls
        missingCheck (c:cs) (l:ls) = if l `elem` (map compareLang (c:cs)) then c : missingCheck cs ls else filePathCheckH False l : missingCheck cs ls
        langs :: [String]
        langs = ["C++", "CSharp", "Python", "Java", "Swift"]
        compareLang (CPP _) = "C++"
        compareLang (CSharp _) = "CSharp"
        compareLang (Python _) = "Python"
        compareLang (Java _) = "Java"
        compareLang (Swift _) = "Swift"

langCheck :: String -> String
langCheck l = if l == "Swift" then "" else l-}

-- helper to filter out "src" and "srs" folders.
filterDox :: [String] -> [String]
filterDox dirs 
  | "doxygen" `elem` dirs = ["doxygen"]
  | otherwise = []

{-docsGet :: [([String], String)] -> FilePath -> IO [[String]]
docsGet [] _ = return [[]]
docsGet (x:xs) fPath = listDirectory (fPath ++ snd x ++ "/" ++ fst.fst x) : docsGet xs fPath-}

mkExamples :: String -> FilePath -> FilePath -> IO [Example]
mkExamples repoRoot localPath srsDir = do

  -- names will be a sorted list of example names (GamePhysics, GlassBR, etc.) of type FilePath
  names <- sort <$> (listDirectory localPath >>= filterM (\x -> doesDirectoryExist $ localPath ++ x))

  -- a list of lists of sources based on existence and contents of src file for each example.
  -- if no src file, then the inner list will be empty
  sources <- mapM (\x -> listDirectory (localPath ++ x) >>= \dirs -> 
    findFiles ((localPath ++ x):dirs) "src" >>=
    fmap (concatMap (map (getSrc names repoRoot) . lines . rstrip)) .
    mapM readFile) names

  -- a list of Just descriptions or Nothing based on existence and contents of desc file.
  descriptions <- mapM (\x -> doesFileExist ("descriptions/" ++ x ++ ".txt") >>=
    \y -> if y then Just . rstrip <$> readFile ("descriptions/" ++ x ++ ".txt") else return Nothing) names

  -- let docSources = map (map (\(CS _ dPath _ name) -> (dPath, name))) sources

  -- Meant to take example names then lists directories within examples.
  -- If the example is projectile, use the first projectile version as a groundbase.
  -- Then make sure the file actually exists.
  -- I don't really know how to handle the projectile case, but this seems to work for the other examples.
  -- Projectile doesn't work at all. Something like the replicate function wouldn't work, would it?
  docsExist <- mapM (\x ->  (listDirectory (localPath ++ x)) >>= 
    (\y -> if "doxygen" `elem` y then 
      (if x == "Projectile" then 
        listDirectory (localPath ++ x ++ "/doxygen/Projectile_C_P_NoL_B_U_V_D")
        else listDirectory (localPath ++ x ++ "/doxygen")) 
      else return []) >>= (\z ->
    filterM (\a ->  doesFileExist (if x == "Projectile" then
      localPath ++ x ++ "/doxygen/Projectile_C_P_NoL_B_U_V_D" ++ a ++ "/index.html"
      -- this part may not be needed if there are no empty directories.
      else localPath ++ x ++ "/doxygen/" ++ a ++ "/index.html")) z)) names


  -- let docsExistSources' = filter (\x -> "doxygen" `elem` x) docsExistSources
  -- filterM (doesDirectoryExist . (++ "doxygen"))-- >>= filterM (doesDirectoryExist (localPath ++ x ++ "doxygen"))) names -- listDirectory (localPath ++ x ++ "doxygen")) names 
  -- print docsExist
  --let docsFilterExist = map filterDox docsExistSources
  -- let docsZipExist = map (zip docsFilterExist) names
  --docsExist <- -- docsGet (zip docsFilterExist names) localPath
  --print docsExist
  -- >>= \dirs -> 
    -- findFiles ((localPath ++ x): dirs) "doxygen") names -- >>= \y -> if y then Just <$> x else return Nothing) names
  -- fst $ map (fst . (map print)) docsExistSources
  --let docsExist = map (map (\x -> show x)) docsExistSources -- map (map (maybe "" id)) docsExistSources
  -- docsExistSources <- mapM (mapM doesFileExist) (map (map fst) docSources)
  -- let docsExist = fillMissingCheck (map (uncurry filePathCheck) $ zip docsExistSources (map (map snd) docSources))
  ---let docsExist = map (uncurry filePathCheck) $ filePathH $ zip docsExistSources (map (map snd) docSources)
  -- creates a list of SRSVariants, so a list of list of tuples.
  -- the outer list has an element for each example.
  srss <- mapM (\x -> sort . getSRS <$> listDirectory (exDirPath localPath x srsDir)) names

  -- returns the IO list of examples with constructor E containing
  -- the name of the example, sources, list of variants, and description
  return $ map (\(name, source, srs, desc, dExist) -> E name source srs desc dExist) $ zip5 names sources srss descriptions docsExist

maybeField :: String -> (Item a -> Compiler (Maybe String)) -> Context a
maybeField s f = Context $ \k _ i -> do
  -- val will be of type Maybe String
  val <- f i
  -- if val is Just and the input string matches the html file paramater,
  -- then we use the String contained in val
  if k == s && isJust val then
    return $ StringField $ fromJust val
  else
    -- Otherwise, fail acts to not use the $if$ parameter in the html file
    fail $ "maybeField " ++ s ++ " used when really Nothing. Wrap in `$if(" ++ s ++ ")$` block."

-- List if non-empty
maybeListFieldWith :: String -> Context a -> (Item b -> [c]) -> (Item b -> Compiler [Item a]) -> Context b
-- takes as input: the string to be replaced, the context to be used for each item in the list,
-- a function to be used on input to return a list to check for emptiness,
-- and the function to be used to return a list of items from the input item, for when the checked list is not empty
maybeListFieldWith s contxt checkNull f = listFieldWith s contxt
  -- If the list is empty, fail is used so as not to create a specific section
  (\x -> if null (checkNull x) then fail ("No instances of " ++ s) else f x)

mkExampleCtx :: FilePath -> FilePath -> FilePath -> Context Example
mkExampleCtx exampleDir srsDir doxDir =
  -- each function is applied to the item containing an Example of form:
  -- E Name [CodeSource] SRSVariants Description

  -- here, listFieldWith used with multiple versions of SRS per example
  listFieldWith "srs" (
    -- field will replace the string parameters in the html with the returned string
    -- <> acts as a semigroup concatenator

    -- returns filename from ((filename, TYPE), E _ _ _ _)
    field "filename" (return . fst . srsVar) <>
    -- returns TYPE from ((_, TYPE), E _ _ _ _)
    field "type" (return . snd . srsVar) <>
    -- returns name from ((_, _), E name [codeSource] srsVariants description)
    -- which is the same name as the outer scope example name
    field "name" (return . name . example) <>
    -- gets the URL by taking the path to the srs ++ the filename of the srs
    field "url" (\x -> return $ exDirPath exampleDir (name $ example x) srsDir ++ fst (srsVar x))
  -- creates a list of items where each item contains a tuple
  -- the tuple contains (specific srs variant, general Example)
  -- in this way, each srs variant is accounted for, and all information can be accessible 
  ) (\x -> mapM (\y -> makeItem (y, itemBody x)) $ srs $ itemBody x)  <>

  -- Fills in the overall name of each example
  -- by returning name from E name [codeSource] srsVariants description
  field "name" (return . name . itemBody) <>
  -- returns the description of an example if Just, otherwise fails and does not add one
  maybeField "desc" (return . desc . itemBody) <>

  -- Lists of source versions if they exist (may be none, one, or many)
  maybeListFieldWith "vers" (
    field "verName" (return . versionName . head . snd . itemBody) <>
    -- List of srcs for a given language. Should be one for each generated code language
    listFieldWith "src" (
      -- return filepath
      field "path" (return . codePath . snd . itemBody) <>
      -- return language
      field "lang" (return . langName . snd . itemBody) <>
      field "doxPath" (return . (\x -> exDirPath exampleDir (name $ fst x) doxDir ++ doxPath (snd x)) . itemBody) <>
      -- PSEUDO: boolField "doxExist" ((\x -> langName (snd x) `elem` dox (fst x)) . itemBody)
      -- needs lang to make sure language name styles match up.
      boolField "doxExist" ((\x -> langName (snd x) `elem` (map lang (dox (fst x)))) . itemBody)
      --boolField "doxExist" ((\x -> langName (snd x) == "Python") . itemBody)
      -- boolField "doxExist" (getBool . uncurry selectSrc . (\x -> (dox (fst x), langName (snd x))) . itemBody)
      -- Extract each source individually and rewrap as an item
      ) ((\(x,y) -> mapM (makeItem . (x,)) y) . itemBody)) 
    -- (src . itemBody) gets the list of sources to be checked for emptiness
    -- (mapM makeItem . src . itemBody) rewraps every item in src to be used internally
    (src . itemBody) 
    -- Srcs corresponding to the same version of an example should always be adjacent, so we use groupBy to group the srcs for each version in their own list, then rewrap as an Item
    ((\x -> mapM (makeItem . (x,)) . groupBy (\a b -> versionName a == versionName b) $ src x) . itemBody)
  where
    name (E nm _ _ _ _) = nm
    src (E _ s _ _ _) = s
    srs (E _ _ s _ _) = s
    desc (E _ _ _ d _) = d
    dox (E _ _ _ _ d) = d
    srsVar = fst . itemBody
    example = snd . itemBody

mkGraphs :: FilePath -> IO [FilePath]
-- returns an IO list of the paths to all graphs based on an input path
mkGraphs path = sort . filter (isSuffixOf ".pdf") <$> listDirectory path

mkGraphCtx :: FilePath -> Context FilePath
mkGraphCtx graphRoot =
  -- fills name with the base name of the file (ie. no paths or extensions)
  field "name" (return . takeBaseName . itemBody) <>
  -- fills url with the link to the file based on the link to the root directory concatenated with the filename
  field "url" (return . (++) graphRoot . itemBody)

main :: IO ()
main = do
  -- Require the Makefile (or deploy script as it will usually be) to feed locations for where to find certain
  -- files/groups of files in the staged deploy folder.
  deployLocation <- getEnv "DEPLOY_FOLDER"
  docsRoot <- getEnv "DOCS_FOLDER"
  exampleRoot <- getEnv "EXAMPLES_FOLDER"
  srsDir <- getEnv "SRS_FOLDER_FRAG"
  doxDir <- getEnv "DOX_FOLDER"
  graphRoot <- getEnv "GRAPH_FOLDER"
  analysisRoot <- getEnv "ANALYSIS_FOLDER"

  -- Env variables relating to variables exposed on CI.
  -- Because we want to be able to test site building locally, we fill in these stubs with
  -- (sometimes correct) assumptions.
  repoSlug <- fromMaybe "JacquesCarette/Drasil" <$> lookupEnv "GITHUB_REPOSITORY"
  commit <- fromMaybe "master" <$> lookupEnv "GITHUB_SHA"
  -- Next two are metadata used to produce the footer.
  buildNumber <- fromMaybe "0" <$> lookupEnv "GITHUB_RUN_NUMBER"
  buildId <- lookupEnv "GITHUB_RUN_ID"

  let repoCommitRoot = "https://github.com/" ++ repoSlug ++ "/tree/" ++ commit ++ "/"
  let docsPath = docsRoot ++ "index.html"
  let fullDocsPath = docsRoot ++ "full/index.html"
  let analysisPath = analysisRoot ++ "DataTable.csv"
  let analysisHTMLPath = analysisRoot ++ "DataTable.html"

  let buildPath = "https://github.com/" ++ repoSlug ++ "/actions" ++ maybe "" ("/runs/" ++) buildId

  doesDocsExist <- doesFileExist $ deployLocation ++ docsPath
  doesFullDocsExist <- doesFileExist $ deployLocation ++ fullDocsPath
  doesAnalysisExist <- doesFileExist $ deployLocation ++ analysisPath
  doesAnalysisHTMLExist <- doesFileExist $ deployLocation ++ analysisHTMLPath
  examples <- mkExamples repoCommitRoot (deployLocation ++ exampleRoot) srsDir
  graphs <- mkGraphs $ deployLocation ++ graphRoot

  hakyll $ do
    -- These first two matches we have nothing here, but they came with the site.hs when I created
    -- the default template. I'm sure at some point there will be CSS and images as well so these rules
    -- don't hurt.
    match "images/*" $ do
      route   idRoute
      compile copyFileCompiler

    match "css/*" $ do
      route   idRoute
      compile compressCssCompiler

    match "index.html" $ do
      route idRoute
      compile $ do
        let indexCtx = listField "examples" (mkExampleCtx exampleRoot srsDir doxDir) (mapM makeItem examples) <>
                       listField "graphs" (mkGraphCtx graphRoot) (mapM makeItem graphs) <>
                       (if doesDocsExist then field "docsUrl" (return . const docsPath) else mempty) <>
                       (if doesFullDocsExist then field "fullDocsUrl" (return . const fullDocsPath) else mempty) <>
                       (if doesAnalysisExist then field "analysisUrl" (return . const analysisPath) else mempty) <>
                       (if doesAnalysisHTMLExist then field "analysisHTMLUrl" (return . const analysisHTMLPath) else mempty) <>
                       field "buildNumber" (return . const buildNumber) <>
                       field "buildUrl" (return . const buildPath) <>
                       field "commit" (return . const commit) <>
                       field "commitUrl" (return . const repoCommitRoot) <>
                       defaultContext

        getResourceBody >>=
          applyAsTemplate indexCtx >>=
          loadAndApplyTemplate "templates/base.html" indexCtx >>=
          relativizeUrls

    match "templates/*" $ compile templateBodyCompiler
