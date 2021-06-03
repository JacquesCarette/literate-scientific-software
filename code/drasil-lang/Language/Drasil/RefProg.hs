{-# Language TemplateHaskell #-}
module Language.Drasil.RefProg where --(Reference(Reference, refInfo), RefInfo(..)) where

{-import Language.Drasil.Sentence (ShortName, Reference(..))
import Language.Drasil.Classes.Core (HasUID(uid), HasRefAddress(getRefAdd))
import Language.Drasil.Classes (HasShortName(shortname))
import Language.Drasil.Label.Type (getAdd)
--import Language.Drasil.ShortName (ShortName)
--import Language.Drasil.UID (UID)

import Control.Lens (makeLenses)

{-- | Holds any extra information needed for a 'Reference', be it an equation, pages, a note, or nothing.
data RefInfo = None
             | Equation [Int]
             | Page [Int]
             | RefNote String

-- | A Reference contains the identifier ('UID'), a reference address ('LblType'),
-- a human-readable shortname ('ShortName'), and any extra information about the reference ('RefInfo').
data Reference = Reference
  { _ui :: UID
  ,  ra :: LblType
  ,  sn :: ShortName
  ,  refInfo :: RefInfo -}
makeLenses ''Reference

-- | Finds the 'UID' of a 'Reference'.
instance HasUID        Reference where uid = ui
-- | Finds the reference address contained in a 'Reference' (through a 'LblType').
instance HasRefAddress Reference where getRefAdd = getAdd . ra
-- | Finds the shortname of the reference address used for the 'Reference'.
instance HasShortName  Reference where shortname = sn-}
