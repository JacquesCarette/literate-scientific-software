-- | Document Description Language
module Language.Drasil.Document where
import Prelude hiding (id)
import Language.Drasil.Chunk.AssumpChunk
import Language.Drasil.Chunk.Change
import Language.Drasil.Chunk.Eq
import Language.Drasil.Chunk.Relation
import Language.Drasil.Chunk.ReqChunk
import Language.Drasil.Spec (Sentence(..), RefName, RefAdd)
import Language.Drasil.Expr
import Language.Drasil.Citations

type Title    = Sentence
type Author   = Sentence
type Header   = Sentence -- Used when creating sublists
type Depth    = Int
type Width    = Float
type Height   = Float
type ListPair = (Title,ItemType) -- ^ Title: Item
type Filepath = String
type Label    = Sentence
type Sections = [Section]

-- | A Document has a Title ('Sentence'), Author(s) ('Sentence'), and Sections
-- which hold the contents of the document
data Document = Document Title Author Sections

-- | Section Contents are split into subsections or contents, where contents
-- are standard layout objects (see 'Contents')
data SecCons = Sub Section
             | Con Contents

-- | Sections have a title ('Sentence') and a list of contents ('SecCons')
data Section = Section Title [SecCons] RefAdd

-- | Types of layout objects we deal with explicitly
data Contents = Table [Sentence] [[Sentence]] Title Bool RefAdd
  -- ^ table has: header-row data(rows) label/caption showlabel?
               | Paragraph Sentence -- ^ Paragraphs are just sentences.
               | EqnBlock Expr RefAdd
     --        CodeBlock Code   -- GOOL complicates this.  Removed for now.
               | Definition DType
               | Enumeration ListType -- ^ Lists
               | Figure Label Filepath MaxWidthPercent RefAdd-- ^ Should use relative file path.
               | Requirement ReqChunk
               | Assumption AssumpChunk
               | LikelyChange Change    --TODO: Collapse likely and unlikely?
               | UnlikelyChange Change
               | Bib BibRef
     --        UsesHierarchy [(ModuleChunk,[ModuleChunk])]
               | Graph [(Sentence, Sentence)] (Maybe Width) (Maybe Height) Label RefAdd
               -- ^ TODO: Fill this one in.
               ------NEW TMOD/DDEF/IM/GD BEGINS HERE------
               ---- FIXME: The above Definition will need to be removed ----
               --------------------------------------------
               | Defnt DType [(Identifier, [Contents])] RefAdd
type Identifier = String

-- | MaxWidthPercent should be kept in the range 1-100. 
-- Values outside this range may have unexpected results.
-- Used for specifying max figure width as
-- pagewidth*MaxWidthPercent/100.
type MaxWidthPercent = Float

data ListType = Bullet [ItemType] -- ^ Bulleted list
              | Number [ItemType] -- ^ Enumerated List
              | Simple [ListPair] -- ^ Simple list with items denoted by @-@
              | Desc [ListPair] -- ^ Descriptive list, renders as "Title: Item" (see 'ListPair')
              | Definitions [ListPair] -- ^ Renders a list of "@Title@ is the @Item@"
         
data ItemType = Flat Sentence -- ^ Standard singular item
              | Nested Header ListType -- ^ Nest a list as an item

--FIXME: Remove Data and Theory from below.
-- | Types of definitions
data DType = Data QDefinition -- ^ QDefinition is the chunk with the defining 
                              -- equation used to generate the Data Definition
           | General
           | Theory RelationConcept -- ^ Theoretical models use a relation as
                                    -- their definition
           | Instance
           | TM
           | DD

---------------------------------------------------------------------------
-- smart constructors and combinators for making instances of the above
-- data types.  Over time, the types should no longer be exported, and 
-- only these used

-- | Smart constructor for creating Sections with introductory contents
-- (ie. paragraphs, tables, etc.) and a list of subsections.
section :: Sentence -> [Contents] -> [Section] -> RefAdd -> Section
section title intro secs = Section title (map Con intro ++ map Sub secs)

-- | Figure smart constructor. Assumes 100% of page width as max width.
fig :: Label -> Filepath -> RefAdd -> Contents
fig l f = Figure l f 100

-- | Figure smart constructor for customized max widths.
figWithWidth :: Label -> Filepath -> MaxWidthPercent -> RefAdd -> Contents
figWithWidth = Figure
