{-# Language TemplateHaskell #-}
module Language.Drasil.Document.Core where

import Language.Drasil.Chunk.Citation (BibRef)

import Language.Drasil.Classes.Core (HasUID(uid), HasRefAddress(getRefAdd),
  Referable(refAdd, renderRef))
import Language.Drasil.Classes.Core2 (HasShortName(shortname))
import Language.Drasil.Display.Expr (DisplayExpr)
import Language.Drasil.Label.Type (LblType(RP), IRefProg, prepend)
import Language.Drasil.Reference (Reference)
import Language.Drasil.Sentence (Sentence)

import Control.Lens ((^.), makeLenses, Lens', set, view)

-- | Denotes the different possible types that can be used as a list.
data ListType = Bullet      [(ItemType, Maybe String)] -- ^ Bulleted list.
              | Numeric     [(ItemType, Maybe String)] -- ^ Enumerated list.
              | Simple      [ListTuple] -- ^ Simple list with items denoted by @:@. Renders as "Title: Item"
              | Desc        [ListTuple] -- ^ Descriptive list, renders as "__Title: Item__" (see 'ListTuple').
              | Definitions [ListTuple] -- ^ Renders a list of "@'Title'@ is the @Item@".

-- | Denotes how something should behave in a list ('ListType').
data ItemType = Flat Sentence -- ^ Standard singular item.
              | Nested Header ListType -- ^ Nest a list ('ListType') as an item.

-- | MaxWidthPercent should be kept in the range 1-100.
-- Values outside this range may have unexpected results.
-- Used for specifying max figure width as
-- @pagewidth*MaxWidthPercent/100@.
type MaxWidthPercent = Float

type Title    = Sentence
type Author   = Sentence
type Header   = Sentence -- ^ Used when creating sublists.
type Depth    = Int
type Width    = Float
type Height   = Float
type ListTuple = (Title, ItemType, Maybe String) -- ^ Formats as Title: Item. For use in lists.
type Filepath = String
type Lbl      = Sentence  -- ^ Label.

-- | Contents may be labelled or unlabelled.
data Contents = UlC UnlabelledContent
              | LlC LabelledContent

-- For 'Defini' below.  From DocumentLanguage.Definitions
--   tmodel, TM, mkTMField [ Para, EqnBlock, Enumeration]
--   ddefn, DD, mkDDField [Para, EqnBlock, Enumeration]
--   gdefn, General, mkGDField [Para, EqnBlock, Enumeration]
--   instanceModel, Instance, mkIMField [Para, EqnBlock, Enumeration]

-- | Types of definitions (general, instance, theory, or data).
data DType = General
           | Instance
           | Theory
           | Data

-- | Types of layout objects we deal with explicitly.
data RawContent =
    Table [Sentence] [[Sentence]] Title Bool -- ^ table has: header-row, data(rows), label/caption, and a bool that determines whether or not to show label.
  | Paragraph Sentence                       -- ^ Paragraphs are just sentences.
  | EqnBlock DisplayExpr                     -- ^ Block of Equations holds an expression.
  | DerivBlock Sentence [RawContent]         -- ^ Grants the ability to label a group of 'RawContent'.
  | Enumeration ListType                     -- ^ For enumerated lists.
  | Defini DType [(Identifier, [Contents])]  -- ^ Defines something with a type, identifier, and 'Contents'.
  | Figure Lbl Filepath MaxWidthPercent      -- ^ For creating figures in a document. Should use relative file path.
  | Bib BibRef                               -- ^ Grants the ability to reference something.
  | Graph [(Sentence, Sentence)] (Maybe Width) (Maybe Height) Lbl -- ^ Contain a graph with coordinates ('Sentence's), maybe a width and height, and a label ('Sentence').
               -- TODO: Fill this one in.

-- | An identifier is just a 'String'.
type Identifier = String

-- | Contains a 'Reference' and 'RawContent'.
data LabelledContent = LblC { _ref :: Reference
                            , _ctype :: RawContent
                            }

-- | Only contains 'RawContent'.                         
newtype UnlabelledContent = UnlblC { _cntnts :: RawContent }

makeLenses ''LabelledContent
makeLenses ''UnlabelledContent

-- FIXME: this is here temporarily due to import cycles
-- | Members of this class must have 'RawContent'.
class HasContents c where
  -- | Provides a 'Lens' to the 'RawContent'.
  accessContents :: Lens' c RawContent

-- | Finds 'UID' of the 'LabelledContent'.
instance HasUID        LabelledContent where uid = ref . uid  
-- | Finds the reference address contained in the 'Reference' of 'LabelledContent'.
instance HasRefAddress LabelledContent where getRefAdd = getRefAdd . view ref
-- | Access the 'RawContent' within the 'LabelledContent'.
instance HasContents   LabelledContent where accessContents = ctype
-- | Find the shortname of the reference address used for the 'LabelledContent'.
instance HasShortName  LabelledContent where shortname = shortname . view ref

-- | Access the 'RawContent' within the 'UnlabelledContent'.
instance HasContents  UnlabelledContent where accessContents = cntnts

-- | Access the 'RawContent' within 'Contents'.
instance HasContents Contents where
  accessContents f (UlC c) = fmap (UlC . (\x -> set cntnts x c)) (f $ c ^. cntnts)
  accessContents f (LlC c) = fmap (LlC . (\x -> set ctype x c)) (f $ c ^. ctype)

-- | Finds the reference information of 'LabelledContent'.
instance Referable LabelledContent where
  refAdd     (LblC lb _) = getRefAdd lb
  renderRef  (LblC lb c) = RP (prependLabel c) (getRefAdd lb)

-- | Helper to prepend labels to 'LabelledContent' when referencing.
prependLabel :: RawContent -> IRefProg
prependLabel Table{}        = prepend "Tab"
prependLabel Figure{}       = prepend "Fig"
prependLabel Graph{}        = prepend "Fig"
prependLabel Defini{}       = prepend "Def"
prependLabel EqnBlock{}     = prepend "EqnB"
prependLabel DerivBlock{}   = prepend "Deriv"
prependLabel Enumeration{}  = prepend "Lst"
prependLabel Paragraph{}    = error "Shouldn't reference paragraphs"
prependLabel Bib{}          = error $ 
    "Bibliography list of references cannot be referenced. " ++
    "You must reference the Section or an individual citation."
