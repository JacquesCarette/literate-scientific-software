{-# LANGUAGE TemplateHaskell, Rank2Types, ScopedTypeVariables  #-}
module Theory.Drasil.GenDefn (GenDefn,
  gd, gdNoRefs, getEqModQdsFromGd) where

import Language.Drasil
import Data.Drasil.TheoryConcepts (genDefn)
import Theory.Drasil.ModelKinds (ModelKind, getEqModQds)

import Control.Lens ((^.), view, makeLenses)

-- | A general definition is a 'ModelKind' that may have units, a derivation,
-- references, a shortname, a reference address, and notes.
data GenDefn = GD { _mk    :: ModelKind
                  , gdUnit :: Maybe UnitDefn -- TODO: Should be derived from the ModelKinds
                  , _deri  :: Maybe Derivation
                  , _rf   :: [Reference]
                  , _sn    :: ShortName
                  , _ra    :: String -- RefAddr
                  , _notes :: [Sentence]
                  }
makeLenses ''GenDefn

-- | Finds the 'UID' of a 'GenDefn'.
instance HasUID             GenDefn where uid           = mk . uid
-- | Finds the term ('NP') of the 'GenDefn'.
instance NamedIdea          GenDefn where term          = mk . term
-- | Finds the idea contained in the 'GenDefn'.
instance Idea               GenDefn where getA          = getA . (^. mk)
-- | Finds the definition of the 'GenDefn'.
instance Definition         GenDefn where defn          = mk . defn
-- | Finds the domain of the 'GenDefn'.
instance ConceptDomain      GenDefn where cdom          = cdom . (^. mk)
-- | Converts the 'GenDefn's related expression into the display language.
instance Display            GenDefn where toDispExpr    = toDispExpr . (^. mk)
-- | Finds the derivation of the 'GenDefn'. May contain Nothing.
instance HasDerivation      GenDefn where derivations   = deri
-- | Finds 'Reference's contained in the 'GenDefn'.
instance HasReference       GenDefn where getReferences = rf
-- | Finds the 'ShortName' of the 'GenDefn'.
instance HasShortName       GenDefn where shortname     = view sn
-- | Finds the reference address of the 'GenDefn'.
instance HasRefAddress      GenDefn where getRefAdd     = view ra
-- | Finds the units of the 'GenDefn'.
instance HasAdditionalNotes GenDefn where getNotes      = notes
-- | Finds the units of the 'GenDefn'.
instance MayHaveUnit        GenDefn where getUnit       = gdUnit
-- | Finds the idea of a 'GenDefn' (abbreviation).
instance CommonIdea         GenDefn where abrv _        = abrv genDefn
-- | Finds the reference address of a 'GenDefn'.
instance Referable          GenDefn where
  refAdd      = getRefAdd
  renderRef l = RP (prepend $ abrv l) (getRefAdd l)

-- | Smart constructor for general definitions.
gd :: IsUnit u => ModelKind -> Maybe u ->
  Maybe Derivation -> [Reference] -> String -> [Sentence] -> GenDefn
gd mkind _   _     []   _  = error $ "Source field of " ++ (mkind ^. uid) ++ " is empty"
gd mkind u derivs refs sn_ = 
  GD mkind (fmap unitWrapper u) derivs refs (shortname' $ S sn_) (prependAbrv genDefn sn_)

-- | Smart constructor for general definitions with no references.
gdNoRefs :: IsUnit u => ModelKind -> Maybe u ->
  Maybe Derivation -> String -> [Sentence] -> GenDefn
gdNoRefs mkind u derivs sn_ = 
  GD mkind (fmap unitWrapper u) derivs [] (shortname' $ S sn_) (prependAbrv genDefn sn_)

-- | Grab all related 'QDefinitions' from a list of general definitions.
getEqModQdsFromGd :: [GenDefn] -> [QDefinition]
getEqModQdsFromGd = getEqModQds . map _mk
