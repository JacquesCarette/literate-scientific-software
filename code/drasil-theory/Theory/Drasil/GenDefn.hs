{-# LANGUAGE TemplateHaskell, Rank2Types, ScopedTypeVariables  #-}
module Theory.Drasil.GenDefn (GenDefn,
  gd, gdNoRefs, gd', gdNoRefs', getEqModQdsFromGd) where

import Language.Drasil
import Data.Drasil.TheoryConcepts (genDefn)
import Theory.Drasil.ModelKinds (ModelKinds(..), getEqModQds)

import Control.Lens ((^.), view, makeLenses)

-- | A general definition is a 'ModelKind' that may have units, a derivation,
-- references (as 'DecRef's), a shortname, a reference address, and notes.
data GenDefn = GD { _gUid  :: UID
                  , _mk    :: ModelKinds
                  , gdUnit :: Maybe UnitDefn -- TODO: Should be derived from the ModelKinds
                  , _deri  :: Maybe Derivation
                  , _rf    :: [DecRef]
                  , _sn    :: ShortName
                  , _ra    :: String -- RefAddr
                  , _notes :: [Sentence]
                  }
makeLenses ''GenDefn

-- | Finds the 'UID' of a 'GenDefn'.
instance HasUID             GenDefn where uid           = gUid
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
{-- | Finds 'Reference's contained in the 'GenDefn'.
instance HasReference       GenDefn where getReferences = rf-}
-- | Finds 'DecRef's contained in the 'GenDefn'.
instance HasDecRef          GenDefn where getDecRefs = rf
-- | Finds the 'ShortName' of the 'GenDefn'.
instance HasShortName       GenDefn where shortname     = view sn
-- | Finds the reference address of the 'GenDefn'.
instance HasRefAddress      GenDefn where getRefAdd   l = RP (prepend $ abrv l) (view ra l)
-- | Finds the units of the 'GenDefn'.
instance HasAdditionalNotes GenDefn where getNotes      = notes
-- | Finds the units of the 'GenDefn'.
instance MayHaveUnit        GenDefn where getUnit       = gdUnit
-- | Finds the idea of a 'GenDefn' (abbreviation).
instance CommonIdea         GenDefn where abrv _        = abrv genDefn
-- | Finds the reference address of a 'GenDefn'.
instance Referable          GenDefn where
  refAdd      = view ra
  renderRef l = RP (prepend $ abrv l) (refAdd l)

-- | Smart constructor for general definitions derived from ModelKinds.
gd :: IsUnit u => ModelKinds -> Maybe u ->
  Maybe Derivation -> [DecRef] -> String -> [Sentence] -> GenDefn
gd mkind = gd' (mkind ^. uid) mkind

-- | Smart constructor for general definitions with no references, derived from ModelKinds.
gdNoRefs :: (IsUnit u) => ModelKinds -> Maybe u ->
  Maybe Derivation -> String -> [Sentence] -> GenDefn
gdNoRefs mkind = gdNoRefs' (mkind ^. uid) mkind

-- | Smart constructor for general definitions.
gd' :: IsUnit u => UID -> ModelKinds -> Maybe u ->
  Maybe Derivation -> [DecRef] -> String -> [Sentence] -> GenDefn
gd' gid _     _   _     []   _  = error $ "Source field of " ++ gid ++ " is empty"
gd' gid mkind u derivs refs sn_ = 
  GD gid mkind (fmap unitWrapper u) derivs refs (shortname' $ S sn_) (prependAbrv genDefn sn_)

-- | Smart constructor for general definitions with no references.
gdNoRefs' :: IsUnit u => UID -> ModelKinds -> Maybe u ->
  Maybe Derivation -> String -> [Sentence] -> GenDefn
gdNoRefs' gid mkind u derivs sn_ = 
  GD gid mkind (fmap unitWrapper u) derivs [] (shortname' $ S sn_) (prependAbrv genDefn sn_)

-- | Grab all related 'QDefinitions' from a list of general definitions.
getEqModQdsFromGd :: [GenDefn] -> [QDefinition]
getEqModQdsFromGd = getEqModQds . map _mk
