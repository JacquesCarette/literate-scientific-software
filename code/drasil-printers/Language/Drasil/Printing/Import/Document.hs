module Language.Drasil.Printing.Import.Document where

import Language.Drasil hiding (neg, sec, symbol, isIn)

import qualified Language.Drasil.Printing.AST as P
import qualified Language.Drasil.Printing.Citation as P
import qualified Language.Drasil.Printing.LayoutObj as T
import Language.Drasil.Printing.PrintingInformation
  (PrintingInformation)

import Language.Drasil.Printing.Import.DisplayExpr (dispExpr)
import Language.Drasil.Printing.Import.Sentence (spec)

import Control.Lens ((^.))
import Data.Bifunctor (bimap, second)

-- | Translates from 'Document' to a printable representation of 'T.Document'.
makeDocument :: PrintingInformation -> Document -> T.Document
makeDocument sm (Document titleLb authorName sections) =
  T.Document (spec sm titleLb) (spec sm authorName) (createLayout sm sections)

-- | Helper for translating sections into a printable representation of layout objects ('T.LayoutObj').
layout :: PrintingInformation -> Int -> SecCons -> T.LayoutObj
layout sm currDepth (Sub s) = sec sm (currDepth+1) s
layout sm _         (Con c) = lay sm c

-- | Helper function for creating sections as layout objects.
createLayout :: PrintingInformation -> [Section] -> [T.LayoutObj]
createLayout sm = map (sec sm 0)

-- | Helper function for creating sections at the appropriate depth.
sec :: PrintingInformation -> Int -> Section -> T.LayoutObj
sec sm depth x@(Section titleLb contents _) = --FIXME: should ShortName be used somewhere?
  let refr = P.S (refAdd x) in
  T.HDiv [concat (replicate depth "sub") ++ "section"]
  (T.Header depth (spec sm titleLb) refr :
   map (layout sm depth) contents) refr

-- | Helper that translates 'Contents' to a printable representation of 'T.LayoutObj'.
-- Called internally by 'layout'.
lay :: PrintingInformation -> Contents -> T.LayoutObj
lay sm (LlC x) = layLabelled sm x
lay sm (UlC x) = layUnlabelled sm (x ^. accessContents)

-- | Helper that translates 'LabelledContent's to a printable representation of 'T.LayoutObj'.
-- Called internally by 'lay'.
layLabelled :: PrintingInformation -> LabelledContent -> T.LayoutObj
layLabelled sm x@(LblC _ (Table hdr lls t b)) = T.Table ["table"]
  (map (spec sm) hdr : map (map (spec sm)) lls)
  (P.S $ getAdd $ getRefAdd x)
  b (spec sm t)
layLabelled sm x@(LblC _ (EqnBlock c))          = T.HDiv ["equation"]
  [T.EqnBlock (P.E (dispExpr c sm))]
  (P.S $ getAdd $ getRefAdd x)
layLabelled sm x@(LblC _ (Figure c f wp))     = T.Figure
  (P.S $ getAdd $ getRefAdd x)
  (spec sm c) f wp
layLabelled sm x@(LblC _ (Graph ps w h t))    = T.Graph
  (map (bimap (spec sm) (spec sm)) ps) w h (spec sm t)
  (P.S $ getAdd $ getRefAdd x)
layLabelled sm x@(LblC _ (Defini dtyp pairs)) = T.Definition
  dtyp (layPairs pairs)
  (P.S $ getAdd $ getRefAdd x)
  where layPairs = map (second (map (lay sm)))
layLabelled sm (LblC _ (Paragraph c))    = T.Paragraph (spec sm c)
layLabelled sm x@(LblC _ (DerivBlock h d)) = T.HDiv ["subsubsubsection"]
  (T.Header 3 (spec sm h) refr : map (layUnlabelled sm) d) refr
  where refr = P.S $ refAdd x ++ "Deriv"
layLabelled sm (LblC _ (Enumeration cs)) = T.List $ makeL sm cs
layLabelled  _ (LblC _ (Bib bib))        = T.Bib $ map layCite bib

-- | Helper that translates 'RawContent's to a printable representation of 'T.LayoutObj'.
-- Called internally by 'lay'.
layUnlabelled :: PrintingInformation -> RawContent -> T.LayoutObj
layUnlabelled sm (Table hdr lls t b) = T.Table ["table"]
  (map (spec sm) hdr : map (map (spec sm)) lls) (P.S "nolabel0") b (spec sm t)
layUnlabelled sm (Paragraph c)    = T.Paragraph (spec sm c)
layUnlabelled sm (EqnBlock c)     = T.HDiv ["equation"] [T.EqnBlock (P.E (dispExpr c sm))] P.EmptyS
layUnlabelled sm (DerivBlock h d) = T.HDiv ["subsubsubsection"]
  (T.Header 3 (spec sm h) refr : map (layUnlabelled sm) d) refr
  where refr = P.S "nolabel1"
layUnlabelled sm (Enumeration cs) = T.List $ makeL sm cs
layUnlabelled sm (Figure c f wp)  = T.Figure (P.S "nolabel2") (spec sm c) f wp
layUnlabelled sm (Graph ps w h t) = T.Graph (map (bimap (spec sm) (spec sm)) ps)
                               w h (spec sm t) (P.S "nolabel6")
layUnlabelled sm (Defini dtyp pairs)  = T.Definition dtyp (layPairs pairs) (P.S "nolabel7")
  where layPairs = map (second (map temp))
        temp  y   = layUnlabelled sm (y ^. accessContents)
layUnlabelled  _ (Bib bib)              = T.Bib $ map layCite bib

-- | For importing a bibliography.
layCite :: Citation -> P.Citation
layCite c = P.Cite (c ^. citeID) (c ^. citeKind) (map layField (c ^. getFields))

-- | Helper for translating 'Citefield's into a printable representation of 'P.CiteField's
layField :: CiteField -> P.CiteField
layField (Address      s) = P.Address      $ P.S s
layField (Author       p) = P.Author       p
layField (BookTitle    b) = P.BookTitle    $ P.S b
layField (Chapter      c) = P.Chapter      c
layField (Edition      e) = P.Edition      e
layField (Editor       e) = P.Editor       e
layField (Institution  i) = P.Institution  $ P.S i
layField (Journal      j) = P.Journal      $ P.S j
layField (Month        m) = P.Month        m
layField (Note         n) = P.Note         $ P.S n
layField (Number       n) = P.Number       n
layField (Organization o) = P.Organization $ P.S o
layField (Pages        p) = P.Pages        p
layField (Publisher    p) = P.Publisher    $ P.S p
layField (School       s) = P.School       $ P.S s
layField (Series       s) = P.Series       $ P.S s
layField (Title        t) = P.Title        $ P.S t
layField (Type         t) = P.Type         $ P.S t
layField (Volume       v) = P.Volume       v
layField (Year         y) = P.Year         y
layField (HowPublished (URL  u)) = P.HowPublished (P.URL  $ P.S u)
layField (HowPublished (Verb v)) = P.HowPublished (P.Verb $ P.S v)

-- | Translates lists to be printable.
makeL :: PrintingInformation -> ListType -> P.ListType
makeL sm (Bullet bs)      = P.Unordered   $ map (bimap (item sm) (fmap P.S)) bs
makeL sm (Numeric ns)     = P.Ordered     $ map (bimap (item sm) (fmap P.S)) ns
makeL sm (Simple ps)      = P.Simple      $ map (\(x,y,z) -> (spec sm x, item sm y, fmap P.S z)) ps
makeL sm (Desc ps)        = P.Desc        $ map (\(x,y,z) -> (spec sm x, item sm y, fmap P.S z)) ps
makeL sm (Definitions ps) = P.Definitions $ map (\(x,y,z) -> (spec sm x, item sm y, fmap P.S z)) ps

-- | Helper for translating list items to be printable.
item :: PrintingInformation -> ItemType -> P.ItemType
item sm (Flat i)     = P.Flat $ spec sm i
item sm (Nested t s) = P.Nested (spec sm t) (makeL sm s)
