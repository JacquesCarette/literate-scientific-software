module Drasil.Website.Example (exampleSec, exampleRefs)where

import Data.List (zipWith4, isPrefixOf)
import Language.Drasil hiding (C)
import Utils.Drasil
import Data.Char (toUpper, toLower)
import qualified Data.List.Split as L


--------------------------
-- Examples Section
--------------------------

exampleSec :: FilePath -> FilePath -> Section
exampleSec p1 p2 = section (S exampleTitle) [mkParagraph exampleIntro, UlC $ ulcc $ mkExampleList p1 p2] [] exampleSecRef

exampleTitle :: String
exampleTitle = "Generated Examples"

exampleIntro :: Sentence
exampleIntro = S "Each of the case studies contain their own generated PDF and HTML reports," +:+
  S "and in some cases, their own generated code."

-- Put the list into RawContent form.
mkExampleList :: FilePath -> FilePath -> RawContent
mkExampleList p1 p2 = Enumeration $ exampleList p1 p2

-- Put the examples into a list.
exampleList :: FilePath -> FilePath -> ListType
exampleList repoPth exPth = Bullet $ zip (zipWith4 (mkExampleListFunc exPth) exampleTitles exampleDescs (exampleCodeRefs repoPth) (exampleDoxRefs exPth)) $ repeat Nothing

-- Organize the examples into a bulleted list.
mkExampleListFunc :: FilePath -> String -> String -> [(String, [(Sentence, Reference)])] -> [(String, [(Sentence, Reference)])] -> ItemType
mkExampleListFunc path exmpl desc codePth doxPth
  | map (map snd . snd) codePth == [[]] && map (map snd . snd) doxPth == [[]] = Nested (S exmpl +:+ S desc) $ Bullet [(Flat (S (exmpl ++ "_SRS") +:+ namedRef (getHTMLRef path exmpl) (S "[HTML]") +:+ namedRef (getPDFRef path exmpl) (S "[PDF]")), Nothing)]
  | map (map snd . snd) doxPth == [[]]                            = Nested (S exmpl +:+ S desc) $ Bullet $ zip [Flat $ S (exmpl ++ "_SRS") +:+ namedRef (getHTMLRef path exmpl) (S "[HTML]") +:+ namedRef (getPDFRef path exmpl) (S "[PDF]"),
                                                                       Nested (S generatedCodeTitle) $ Bullet $ mkCodeList codePth] $ repeat Nothing
  | otherwise                                         = Nested (S exmpl +:+ S desc) $ Bullet $ zip [Flat $ S (exmpl ++ "_SRS") +:+ namedRef (getHTMLRef path exmpl) (S "[HTML]") +:+ namedRef (getPDFRef path exmpl) (S "[PDF]"),
                                                                       Nested (S generatedCodeTitle) $ Bullet $ mkCodeList codePth,
                                                                       Nested (S generatedCodeDocsTitle) $ Bullet $ mkCodeList doxPth] $ repeat Nothing

-- References come in the form of [(Project version), [(Display name, Reference for generated code or documents)]]
mkCodeList :: [(String, [(Sentence, Reference)])] -> [(ItemType, Maybe String)]
mkCodeList = map
      (\ r
         -> (Flat
               $ foldlSent_
                   $ S (fst r) : map (uncurry (flip namedRef)) (snd r),
             Nothing))

exampleTitles, exampleDescs :: [String]
-- Sorts the references for mkCodeList.
exampleCodeRefs, exampleDoxRefs :: FilePath -> [[(String, [(Sentence, Reference)])]]
-- example titles
exampleTitles = [pendulum, gamePhys, glassBR, hghc, noPCM, pdController, projectile, ssp, swhs, template]
-- example descriptions (used in the list of examples)
exampleDescs = [pendulumDesc, gamePhysDesc, glassBRDesc, hghcDesc, noPCMDesc, pdControllerDesc, projectileDesc, sspDesc, swhsDesc, templateDesc]
exampleCodeRefs path =[[(pendulum, [])],
                  [(gamePhys, [])],
                  [(glassBR, map (getCodeRef path $ map toLower glassBR) glassBRCode)],
                  [(hghc, [])],
                  [(noPCM, map (getCodeRef path $map toLower noPCM) noPCMCode)],
                  [(pdController, map (getCodeRef path $ map toLower pdController) pdControllerCode)],
                  [(projectileC1, map (getCodeRef path (map toLower projectile ++ "/" ++ projectileC1)) projectileCase1Code),
                  (projectileC2, map (getCodeRef path (map toLower projectile ++ "/" ++ projectileC2)) projectileCase2Code),
                  (projectileC3, map (getCodeRef path (map toLower projectile ++ "/" ++ projectileC3)) projectileCase3Code),
                  (projectileC4, map (getCodeRef path (map toLower projectile ++ "/" ++ projectileC4)) projectileCase4Code),
                  (projectileC5, map (getCodeRef path (map toLower projectile ++ "/" ++ projectileC5)) projectileCase5Code)],
                  [(ssp, [])],
                  [(swhs, [])],
                  [(template, [])]]
exampleDoxRefs path =[[(pendulum, [])],
                 [(gamePhys, [])],
                 [(glassBR, map (getDoxRef path glassBR) glassBRDox)],
                 [(hghc, [])],
                 [(noPCM, map (getDoxRef path noPCM) noPCMDox)],
                 [(pdController, map (getDoxRef path pdController) pdControllerDox)],
                 [(projectileC1, map (\x -> getDoxRef path projectile (projectileC1 ++ "/" ++ x)) projectileCase1Dox),
                 (projectileC2, map (\x -> getDoxRef path projectile (projectileC2 ++ "/" ++ x)) projectileCase2Dox),
                 (projectileC3, map (\x -> getDoxRef path projectile (projectileC3 ++ "/" ++ x)) projectileCase3Dox),
                 (projectileC4, map (\x -> getDoxRef path projectile (projectileC4 ++ "/" ++ x)) projectileCase4Dox),
                 (projectileC5, map (\x -> getDoxRef path projectile (projectileC5 ++ "/" ++ x)) projectileCase5Dox)],
                 [(ssp, [])],
                 [(swhs, [])],
                 [(template, [])]]

--example names, maybe make a unique type to accept fields of documents, gen code, and doxygen?
pendulum, gamePhys, glassBR, hghc, noPCM, pdController, projectile, projectileC1,
  projectileC2, projectileC3, projectileC4, projectileC5, ssp, swhs, template :: String

pendulum = "DblPendulum"
gamePhys = "GamePhysics"
glassBR = "GlassBR"
hghc = "HGHC"
noPCM = "NoPCM"
pdController = "PDController"
projectile = "Projectile"
projectileC1 = "Projectile_C_P_NoL_B_U_V_D"
projectileC2 = "Projectile_S_L_NoL_U_U_V_F"
projectileC3 = "Projectile_U_P_L_B_B_C_D"
projectileC4 = "Projectile_U_P_NoL_U_WI_V_D"
projectileC5 = "Projectile_U_P_L_B_WI_V_F"
ssp = "SSP"
swhs = "SWHS"
template = "Template"

-- list that states what languages the generated code/doxygen docs exist in.
glassBRCode, glassBRDox, noPCMCode, noPCMDox, pdControllerCode, pdControllerDox, projectileCase1Code, projectileCase2Code,
  projectileCase3Code, projectileCase4Code, projectileCase5Code, projectileCase1Dox, projectileCase2Dox,
  projectileCase3Dox, projectileCase4Dox, projectileCase5Dox :: [String]

glassBRCode          = ["cpp", "csharp", "java", "python", "swift"]
glassBRDox           = ["cpp", "csharp", "java", "python"]
noPCMCode            = ["cpp", "csharp", "java", "python"]
noPCMDox             = ["cpp", "csharp", "java", "python"]
pdControllerCode     = ["python"]
pdControllerDox      = []
projectileCase1Code  = ["cpp", "csharp", "java", "python", "swift"]
projectileCase2Code  = ["cpp", "csharp", "java", "python", "swift"]
projectileCase3Code  = ["cpp", "csharp", "java", "python", "swift"]
projectileCase4Code  = ["cpp", "csharp", "java", "python", "swift"]
projectileCase5Code  = ["cpp", "csharp", "java", "python", "swift"]
projectileCase1Dox   = ["cpp", "csharp", "java", "python"]
projectileCase2Dox   = ["cpp", "csharp", "java", "python"]
projectileCase3Dox   = ["cpp", "csharp", "java", "python"]
projectileCase4Dox   = ["cpp", "csharp", "java", "python"]
projectileCase5Dox   = ["cpp", "csharp", "java", "python"]

-- Make references for each of the generated SRS files
getHTMLRef, getPDFRef :: FilePath -> String -> Reference
getHTMLRef path ex = Reference ("htmlRef" ++ ex) (URI (getHTMLPath path ex)) (shortname' $ S ("htmlRef" ++ ex)) None
getPDFRef path ex = Reference ("pdfRef" ++ ex) (URI (getPDFPath path ex)) (shortname' $ S ("pdfRef" ++ ex)) None
getHTMLPath, getPDFPath :: FilePath -> String -> FilePath
getHTMLPath path ex = path ++ ex ++ "/srs/" ++ ex ++ "_SRS.html"
getPDFPath path ex = path ++ ex ++ "/srs/" ++ ex ++ "_SRS.pdf"

-- Make display names and references for generated code and docs
getCodeRef, getDoxRef :: FilePath -> String -> String -> (Sentence, Reference)
getCodeRef path ex lang = (S ("[" ++ convertlang ++ "]"), Reference ("codeRef" ++ ex ++ lang) (URI (getCodePath path ex lang)) (shortname' $ S ("codeRef" ++ ex ++ lang)) None)
  where
    convertlang
      | lang == "cpp" = "C++"
      | lang == "csharp" = "C Sharp" -- Drasil printers dont like the # symbol, so we use the full word.
      | otherwise = (toUpper.head) lang : tail lang
getDoxRef path ex lang = (S ("[" ++ convertlang lang ++ "]"), Reference ("doxRef" ++ ex ++ lang) (URI (getDoxPath path ex lang)) (shortname' $ S ("doxRef" ++ ex ++ lang)) None)
  where
    convertlang l
      | l == "cpp" = "C++"
      | l == "csharp" = "C Sharp"
      | "Projectile" `isPrefixOf` l = convertlang $ concat $ tail $ L.splitOn "/" l
      | otherwise = (toUpper.head) l : tail l
getCodePath :: FilePath -> String -> String -> FilePath
getDoxPath :: FilePath -> String -> String -> FilePath
getCodePath path ex lang = path ++ "code/stable/" ++ ex ++ "/src/" ++ lang -- need repoCommit path
getDoxPath path ex lang = path ++ ex ++ "/doxygen/" ++ lang ++ "/index.html" -- need example path

-- Project descriptions
pendulumDesc, gamePhysDesc, glassBRDesc, hghcDesc, noPCMDesc, pdControllerDesc,
  projectileDesc, sspDesc, swhsDesc, templateDesc :: String

pendulumDesc = ""
gamePhysDesc = "describes the modeling of an open source 2D rigid body physics library used for games."
glassBRDesc = "predicts whether a given glass slab is likely to resist a specified blast."
hghcDesc = "describes heat transfer coefficients related to clad."
noPCMDesc = "describes the modelling of a solar water heating system without phase change material."
pdControllerDesc = ""
projectileDesc = "describes the motion of a projectile object in free space."
sspDesc = "describes the requirements of a slope stability analysis program."
swhsDesc = "describes the modelling of a solar water heating system with phase change material."
templateDesc = "is an empty template document."

generatedCodeTitle, generatedCodeDocsTitle :: String
generatedCodeTitle = "Generated Code:"
generatedCodeDocsTitle = "Generated Code Documentation:"

exampleRefs :: FilePath -> FilePath -> [Reference]
exampleRefs p1 p2 = [exampleSecRef, ref $ exampleSec p1 p2] ++
  concatMap (concatMap (map snd. snd)) (exampleCodeRefs p1) ++
  concatMap (concatMap (map snd. snd)) (exampleDoxRefs p2) ++
  map (getHTMLRef p2) exampleTitles ++ map (getPDFRef p2) exampleTitles

exampleSecRef :: Reference
exampleSecRef = makeSecRef "Examples" $ S "Examples"