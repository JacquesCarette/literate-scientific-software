{-# LANGUAGE TemplateHaskell #-}
module Theory.Drasil.DataDefinition where

import Control.Lens (makeLenses, (^.), view)
import Language.Drasil
import qualified Language.Drasil.DisplayExpr as DE
import Data.Drasil.TheoryConcepts (dataDefn)

-- | A scope is an indirect reference to a 'UID'.
newtype Scope = Scp { _spec :: UID }

-- | Determines the scope of data.
data ScopeType =
    Local Scope -- ^ Only visible within a limited scope.
  | Global      -- ^ Visible everywhere.

-- | A data definition is a 'QDefinition' that may have additional notes: 
-- the scope, any references, maybe a derivation, a label ('ShortName'), a reference address, and other notes ('Sentence's).
data DataDefinition = DatDef { _qd    :: QDefinition
                             , _scp   :: ScopeType
                             , _rf   :: [Reference]
                             , _deri  :: Maybe Derivation
                             , lbl    :: ShortName
                             , ra     :: String
                             , _notes :: [Sentence]
                             }
makeLenses ''DataDefinition

-- | Finds the 'UID' of a 'DataDefinition'.
instance HasUID             DataDefinition where uid = qd . uid
-- | Finds the term ('NP') of the 'QDefinition' used to make the 'DataDefinition'.
instance NamedIdea          DataDefinition where term = qd . term
-- | Finds the idea contained in the 'QDefinition' used to make the 'DataDefinition'.
instance Idea               DataDefinition where getA c = getA $ c ^. qd
-- | Finds the Space of the 'QDefinition' used to make the 'DataDefinition'.
instance HasSpace           DataDefinition where typ = qd . typ
-- | Finds the Symbol of the 'QDefinition' used to make the 'DataDefinition'.
instance HasSymbol          DataDefinition where symbol e = symbol (e ^. qd)
-- | 'DataDefinition's have a 'Quantity'.
instance Quantity           DataDefinition where 
-- | Finds the defining expression of the 'QDefinition' used to make the 'DataDefinition'.
instance DefiningExpr       DataDefinition where defnExpr = qd . defnExpr
-- | Converts the defining expression of a 'DataDefinition' into the display language.
instance Display            DataDefinition where toDispExpr d = DE.defines (sy d) (d ^. defnExpr)
-- | Finds 'Reference's contained in the 'DataDefinition'.
instance HasReference       DataDefinition where getReferences = rf
-- | Equal if 'UID's are equal.
instance Eq                 DataDefinition where a == b = (a ^. uid) == (b ^. uid)
-- | Finds the derivation of the 'DataDefinition'. May contain Nothing.
instance HasDerivation      DataDefinition where derivations = deri
-- | Finds any additional notes for the 'DataDefinition'.
instance HasAdditionalNotes DataDefinition where getNotes = notes
-- | Finds the units of the 'QDefinition' used to make the 'DataDefinition'.
instance MayHaveUnit        DataDefinition where getUnit = getUnit . view qd 
-- | Finds the 'ShortName' of the 'DataDefinition'.
instance HasShortName       DataDefinition where shortname = lbl
-- | Finds the reference address of a 'DataDefinition'.
instance HasRefAddress      DataDefinition where getRefAdd = ra
-- | Finds the domain of the 'QDefinition' used to make the 'DataDefinition'.
instance ConceptDomain      DataDefinition where cdom _ = cdom dataDefn
-- | Finds the idea of a 'DataDefinition' (abbreviation).
instance CommonIdea         DataDefinition where abrv _ = abrv dataDefn
-- | Finds the reference address of a 'DataDefinition'.
instance Referable          DataDefinition where
  refAdd      = getRefAdd
  renderRef l = RP (prepend $ abrv l) (getRefAdd l)

-- | Smart constructor for data definitions.
dd :: QDefinition -> [Reference] -> Maybe Derivation -> String -> [Sentence] -> DataDefinition
dd q []   _   _  = error $ "Source field of " ++ q ^. uid ++ " is empty"
dd q refs der sn = DatDef q Global refs der (shortname' $ S sn) (prependAbrv dataDefn sn)

-- | Smart constructor for data definitions with no references.
ddNoRefs :: QDefinition -> Maybe Derivation -> String -> [Sentence] -> DataDefinition
ddNoRefs q der sn = DatDef q Global [] der (shortname' $ S sn) (prependAbrv dataDefn sn)

-- | Extracts the 'QDefinition' from a 'DataDefinition'.
qdFromDD :: DataDefinition -> QDefinition
qdFromDD = (^. qd)
