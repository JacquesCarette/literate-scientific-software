module Data.Drasil.Domains where

import Language.Drasil (IdeaDict, mkIdea, cn')

compScience, softEng, mathematics, progLanguage, physics, civilEng
  , materialEng, documentc, knowledgemng :: IdeaDict
-------------------------------------------------------------------------------
--  IdeaDict     |   |      id       |       term                    |  abbreviation
-------------------------------------------------------------------------------
compScience  = mkIdea  "compScience"    (cn' "Computer Science")      (Just "CS")
softEng      = mkIdea  "softEng"        (cn' "Software Engineering")  (Just "SE")
mathematics  = mkIdea  "mathematics"    (cn' "Mathematics")           Nothing
progLanguage = mkIdea  "progLanguage"   (cn' "Programming Language")  Nothing
physics      = mkIdea  "physics"        (cn' "Physics")               Nothing
civilEng     = mkIdea  "civilEng"       (cn' "Civil Engineering")     Nothing
materialEng  = mkIdea  "materialEng"    (cn' "Material Engineering")  Nothing
documentc    = mkIdea  "documentc"      (cn' "Document")              (Just "Doc")
knowledgemng = mkIdea  "knowledgemng"   (cn' "Knowledge Management")  Nothing
