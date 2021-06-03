{-# Language TypeFamilies #-}
-- | Defining the core classes which represent knowledge-about-knowledge
module Language.Drasil.Classes.Core (
    HasUID(uid)
  --, HasShortName(shortname)
  , HasRefAddress(getRefAdd)
  , HasSymbol(symbol)
  ) where

--import Language.Drasil.ShortName (ShortName)
import Language.Drasil.Stages (Stage)
import Language.Drasil.Symbol (Symbol)
import Language.Drasil.UID (UID)

import Control.Lens (Lens')

-- | The most basic item: having a unique identifier key, here a UID.
class HasUID c where
  -- | Provides a /unique/ id for internal Drasil use.
  uid :: Lens' c UID

-- | A HasSymbol is anything which has a 'Symbol'.
class HasSymbol c where
  -- | Provides the 'Symbol' for a particular stage of generation.
  symbol  :: c -> Stage -> Symbol
 
-- | Members must have a reference address.
class HasRefAddress b where
  -- | Provides the ability to hold a reference address.
  getRefAdd :: b -> String
