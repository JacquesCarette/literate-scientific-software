module Drasil.DocLang.Notebook where

import Language.Drasil
import Utils.Drasil.Sentence

import qualified Data.Drasil.Concepts.Documentation as Doc (notebook, introduction, 
  prpsOfDoc, review, body, mainIdea, procForAnls, summary, methAndAnls,method_, 
  coordinateSystem, example, result, appendix, reference)
import qualified Data.Drasil.Concepts.Physics as P (motion, horizontalMotion, verticalMotion, kinematics)

-- | Notebook constructor. 
-- Create the notebook from given system name, authors, and sections
doc :: NamedIdea c => c -> Sentence -> [Section] -> Document
doc  sys = Document (Doc.notebook `forTT` sys)

intro, prpsOfDoc, body, review, procForAnls, summary, method, example, result, 
  appendix, reference :: [Contents] -> [Section] -> Section
  
intro       cs ss = section' (titleize Doc.introduction)      cs ss "Intro"
prpsOfDoc   cs ss = section' (titleize Doc.prpsOfDoc)         cs ss "DocPurpose"

body        cs ss = section' (titleize Doc.body)              cs ss "Body"
review      cs ss = section' (titleize Doc.review)            cs ss "Review"

mainIdea    cs ss = section' (titleize Doc.mainIdea)          cs ss "MainIdea"
motion      cs ss = section' (titleize P.motion)              cs ss "Motion"
hormotion   cs ss = section' (titleize P.horizontalMotion)    cs ss "HorizontalMotion"
vermotion   cs ss = section' (titleize P.verticalMotion)      cs ss "VerticalMotion"

methAndAnls cs ss = section  (titleize' Doc.methAndAnls)      cs ss methsAndanlsLabel
method      cs ss = section' (titleize Doc.method_)           cs ss "Method"

summary     cs ss = section' (titleize Doc.summary)           cs ss "Summary"

procForAnls cs ss = section'  (titleize Doc.procForAnls)      cs ss "AnlsProc"
coorSyst    cs ss = section'  (titleize Doc.coordinateSystem) cs ss "CoordinateSystem"
kinematic   cs ss = section'  (titleize P.kinematics)          cs ss "Kinematic"

example     cs ss = section' (titleize Doc.example)           cs ss "Example"

result      cs ss = section' (titleize Doc.result)            cs ss "Result"

appendix    cs ss = section' (titleize Doc.appendix)          cs ss "Appendix"

reference   cs ss = section  (titleize' Doc.reference)        cs ss referenceLabel

--function that sets the shortname of each section to be the reference address
section' :: Sentence -> [Contents] -> [Section] -> String -> Section
section' a b c d = section a b c (makeSecRef d (toString a))
  where
    toString :: Sentence -> String --FIXME: same as sentenceDoc, import instead? 
    toString (S x) = x
    toString ((:+:) s1 s2) = toString s1 ++ toString s2
    toString _ = error "Term is not a string"

--Labels--
methsAndanlsLabel, referenceLabel :: Reference
methsAndanlsLabel = makeSecRef "MethsAndAnls"  "Methods and Analysis"
referenceLabel    = makeSecRef "References"    "References"