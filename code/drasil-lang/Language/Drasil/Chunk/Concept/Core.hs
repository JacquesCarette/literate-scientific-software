{-# Language TemplateHaskell #-}
module Language.Drasil.Chunk.Concept.Core(ConceptChunk(ConDict), CommonConcept(ComConDict)
  , ConceptInstance(ConInst)
  , sDom)
  where

import Language.Drasil.Classes.Core (HasUID(uid),
  HasRefAddress(getRefAdd), Referable(refAdd, renderRef))
import Language.Drasil.Classes.Core2 (HasShortName(shortname))
import Language.Drasil.Classes (NamedIdea(term), Idea(getA),
  Definition(defn), ConceptDomain(cdom), CommonIdea(abrv))
import Language.Drasil.Chunk.CommonIdea (CI)
import Language.Drasil.Chunk.NamedIdea (IdeaDict)
import Language.Drasil.Label.Type (LblType(RP), name, raw, (+::+), defer)
import Language.Drasil.Sentence (Sentence)
import Language.Drasil.ShortName (ShortName)
import Language.Drasil.UID (UID)

import Control.Lens (makeLenses, (^.), view)

-- | Check if something has one domain. Throws an error if there is more than one.
sDom :: [UID] -> UID
sDom [d] = d
sDom d = error $ "Expected ConceptDomain to have a single domain, found " ++
  show (length d) ++ " instead."

-- | The ConceptChunk datatype is a Concept that contains an idea ('IdeaDict'), a definition ('Sentence'), and a domain (['UID']).
data ConceptChunk = ConDict { _idea :: IdeaDict -- ^ Contains the idea of the concept.
                            , _defn' :: Sentence -- ^ The definition of the concept.
                            , cdom' :: [UID] -- ^ Domain of the concept.
                            }
makeLenses ''ConceptChunk

-- | Equal if 'UID's are equal.
instance Eq            ConceptChunk where c1 == c2 = (c1 ^. uid) == (c2 ^. uid) 
-- | Finds 'UID' of the 'IdeaDict' used to make the 'ConceptChunk'.
instance HasUID        ConceptChunk where uid = idea . uid 
-- | Finds term ('NP') of the 'IdeaDict' used to make the 'ConceptChunk'.
instance NamedIdea     ConceptChunk where term = idea . term 
-- | Finds the idea contained in the 'IdeaDict' used to make the 'ConceptChunk'.
instance Idea          ConceptChunk where getA = getA . view idea 
-- | Finds definition of a 'ConceptChunk'.
instance Definition    ConceptChunk where defn = defn' 
-- | Finds the domain of 'UID's of a 'ConceptChunk'.
instance ConceptDomain ConceptChunk where cdom = cdom' 

-- | Contains a common idea ('CI') with a definition ('Sentence').
data CommonConcept = ComConDict { _comm :: CI, _def :: Sentence}
makeLenses ''CommonConcept

-- | Equal if 'UID's are equal.
instance Eq            CommonConcept where c1 == c2 = (c1 ^. uid) == (c2 ^. uid) 
-- | Finds 'UID' of the 'CI' used to make the 'CommonConcept'.
instance HasUID        CommonConcept where uid = comm . uid 
-- | Finds term ('NP') of the 'CI' used to make the 'CommonConcept'.
instance NamedIdea     CommonConcept where term = comm . term 
-- | Finds the idea contained in the 'CI' used to make the 'CommonConcept'.
instance Idea          CommonConcept where getA = getA . view comm 
-- | Finds definition of a 'CommonConcept'.
instance Definition    CommonConcept where defn = def 
-- | Finds the abbreviation contained in the 'CI' used to make the 'CommonConcept'.
instance CommonIdea    CommonConcept where abrv = abrv . view comm 
-- | Finds the domain contained in the 'CI' used to make the 'CommonConcept'.
instance ConceptDomain CommonConcept where cdom = cdom . view comm 

-- | Contains a 'ConceptChunk', reference address, and a 'ShortName'.
data ConceptInstance = ConInst { _cc :: ConceptChunk , ra :: String, shnm :: ShortName}
makeLenses ''ConceptInstance

-- | Equal if 'UID's are equal.
instance Eq            ConceptInstance where c1 == c2 = (c1 ^. uid) == (c2 ^. uid) 
-- | Finds 'UID' of the 'ConceptChunk' used to make the 'ConceptInstance'.
instance HasUID        ConceptInstance where uid = cc . idea . uid 
-- | Finds term ('NP') of the 'ConceptChunk' used to make the 'ConceptInstance'.
instance NamedIdea     ConceptInstance where term = cc . idea . term 
-- | Finds the idea contained in the 'ConceptChunk' used to make the 'ConceptInstance'.
instance Idea          ConceptInstance where getA = getA . view (cc . idea) 
-- | Finds the definition contained in the 'ConceptChunk' used to make the 'ConceptInstance'.
instance Definition    ConceptInstance where defn = cc . defn' 
-- | Finds the domain contained in the 'ConceptChunk' used to make the 'ConceptInstance'.
instance ConceptDomain ConceptInstance where cdom = cdom' . view cc 
-- | Finds the 'ShortName' contained in a 'ConceptInstance'.
instance HasShortName  ConceptInstance where shortname = shnm 
-- | Finds the reference address contained in a 'ConceptInstance'.
instance HasRefAddress ConceptInstance where getRefAdd l = RP (defer (sDom $ cdom l) +::+ raw ":" +::+ name) (ra l)
-- | Finds the reference information contained in a 'ConceptInstance'.
instance Referable     ConceptInstance where
  refAdd      = ra        -- Finds the reference address contained in a ConceptInstance.
  renderRef   = getRefAdd -- Finds the reference address but in a diferent form.

