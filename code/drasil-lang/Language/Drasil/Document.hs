{-# Language TemplateHaskell #-}
-- | Document Description Language
module Language.Drasil.Document where

import Language.Drasil.Classes.Core (HasUID(uid), getRefAdd)
import Language.Drasil.Classes (Referable(refAdd, renderRef))
import Language.Drasil.Document.Core
import Language.Drasil.Label.Type (prepend, LblType(RP, URI),raw, (+::+), name)
import Language.Drasil.Misc (repUnd)
import Language.Drasil.RefProg ()
import Language.Drasil.Sentence (Sentence(..), ShortName, shortname',
  HasShortName(shortname), Reference(Reference), RefInfo(None))
import Language.Drasil.UID (UID)

import Control.Lens (makeLenses, view)

-- | Section Contents are split into subsections or contents, where contents
-- are standard layout objects (see 'Contents').
data SecCons = Sub   Section
             | Con   Contents

-- | Sections have a title ('Sentence'), a list of contents ('SecCons')
-- and a shortname ('Reference').
data Section = Section 
             { tle  :: Title 
             , cons :: [SecCons]
             , _lab :: Reference
             }
makeLenses ''Section

-- | Finds the 'UID' of a 'Section'.
instance HasUID        Section where uid = lab . uid
-- | Finds the short name of a 'Section'.
instance HasShortName  Section where shortname = shortname . view lab
-- | Finds the reference address of a 'Section'.
instance Referable Section where
  refAdd    (Section _ _ lb ) = getRefAdd lb
  renderRef (Section _ _ lb)  = RP (raw "Section: " +::+ name) (getRefAdd lb)

-- | A Document has a Title ('Sentence'), Author(s) ('Sentence'), and 'Section's
-- which hold the contents of the document.
data Document = Document Title Author [Section]

-- | Smart constructor for labelled content chunks.
llcc :: Reference -> RawContent -> LabelledContent
llcc = LblC

-- | Smart constructor for unlabelled content chunks (no 'Reference').
ulcc :: RawContent -> UnlabelledContent
ulcc = UnlblC

---------------------------------------------------------------------------
-- | Smart constructor that wraps 'UnlabelledContent' into 'Contents'.
mkParagraph :: Sentence -> Contents
mkParagraph x = UlC $ ulcc $ Paragraph x
-- | Smart constructor that wraps 'LabelledContent' into 'Contents'.
mkFig :: Reference -> RawContent -> Contents
mkFig x y = LlC $ llcc x y

--Fixme: use mkRawLc or llcc?
-- | Smart constructor similar to 'llcc', but takes in 'RawContent' first.
mkRawLC :: RawContent -> Reference -> LabelledContent
mkRawLC x lb = llcc lb x

---------------------------------------------------------------------------
-- smart constructors and combinators for making instances of the above
-- data types.  Over time, the types should no longer be exported, and
-- only these used

-- | Smart constructor for creating 'Section's with a title ('Sentence'), introductory contents
-- (ie. paragraphs, tables, etc.), a list of subsections, and a shortname ('Reference').
section :: Sentence -> [Contents] -> [Section] -> Reference -> Section
section title intro secs = Section title (map Con intro ++ map Sub secs)

-- | Smart constructor for retrieving the contents ('Section's) from a 'Document'.
extractSection :: Document -> [Section]
extractSection (Document _ _ sec) = concatMap getSec sec

-- | Smart constructor for retrieving the subsections ('Section's) within a 'Section'.
getSec :: Section -> [Section]
getSec t@(Section _ sc _) = t : concatMap getSecCons sc

-- | Helper to retrieve subsections ('Section's) from section contents ('SecCons').
getSecCons :: SecCons -> [Section]
getSecCons (Sub sec) = getSec sec
getSecCons (Con _)   = []

-- | 'Figure' smart constructor with a 'Lbl' and a 'Filepath'. Assumes 100% of page width as max width.
fig :: Lbl -> Filepath -> RawContent
fig l f = Figure l f 100

-- | 'Figure' smart constructor that allows for customized max widths.
figWithWidth :: Lbl -> Filepath -> MaxWidthPercent -> RawContent
figWithWidth = Figure

---------------------------------------------------------------------------
-- FIXME: horrible hacks.
-- These should eventually either disappear, or at least move out to docLang
-- | Create a reference for a table. Takes in the name of a table (which will also be used for its shortname).
makeTabRef :: String -> Reference
makeTabRef rs = Reference rs (RP (prepend "Tab") ("Table:" ++ repUnd rs)) (shortname' rs) None

-- | Create a reference for a figure. Takes in the name of a figure (which will also be used for its shortname).
makeFigRef :: String -> Reference
makeFigRef rs = Reference rs (RP (prepend "Fig") ("Figure:" ++ repUnd rs)) (shortname' rs) None

-- | Create a reference for a section. Takes in the name of a section and a shortname for the section.
makeSecRef :: String -> String -> Reference
makeSecRef r s = Reference (r ++ "Label") (RP (prepend "Section") ("Sec:" ++ repUnd r))
  (shortname' s) None

-- | Create a reference for a list. Takes in the name of the list and a shortname for the list.
makeLstRef :: String -> String -> Reference
makeLstRef r s = Reference (r ++ "Label") (RP (prepend "Lst") ("Lst:" ++ repUnd r))
  (shortname' s) None

-- | Create a reference for a 'URI'.
makeURI :: UID -> String -> ShortName -> Reference
makeURI u r s = Reference u (URI r) s None
