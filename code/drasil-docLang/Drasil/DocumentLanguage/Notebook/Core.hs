{-# LANGUAGE GADTs #-}
module Drasil.DocumentLanguage.Notebook.Core where

import Data.Generics.Multiplate (Multiplate(multiplate, mkPlate))
import Language.Drasil

type NBDesc = [DocSection]

data DocSection = IntroSec IntroSec
                | BodySec BodySec
                | SmmrySec SmmrySec
                | Bibliography
                | AppndxSec AppndxSec

-- **TODO: Work on detail structure of notebooks

{--}

-- | Introduction section. Contents are top level followed by a list of subsections.
data IntroSec = IntroProg [Contents] [IntroSub]

-- | Introduction subsections
data IntroSub where
  IPurpose :: [Sentence] -> IntroSub -- **maybe change to [Contents]
  IScope   :: Sentence -> IntroSub

{--}

newtype BodySec = BodyProg [BodySub]

data BodySub where
  Review       :: [Contents] -> BodySub
  MainIdea     :: [Contents] -> [Section] -> BodySub
  MethsAndAnls :: [Contents] -> [Section] -> BodySub

{--}

newtype SmmrySec = SmmryProg [Contents]

{--}

newtype AppndxSec = AppndxProg [Contents]

{--}

data DLPlate f = DLPlate {
  docSec :: DocSection -> f DocSection,
  introSec :: IntroSec -> f IntroSec,
  introSub :: IntroSub -> f IntroSub,
  bodySec :: BodySec -> f BodySec,
  bodySub :: BodySub -> f BodySub,
  smmrySec ::SmmrySec -> f SmmrySec,
  appendSec :: AppndxSec -> f AppndxSec
}

instance Multiplate DLPlate where
  multiplate p = DLPlate ds intro intro' body body' smry aps where
    ds (IntroSec x) = IntroSec <$> introSec p x
    ds (BodySec x) = BodySec <$> bodySec p x
    ds (SmmrySec x) = SmmrySec <$> smmrySec p x
    ds (AppndxSec x) = AppndxSec <$> appendSec p x
    ds Bibliography = pure Bibliography

    intro (IntroProg c progs) = IntroProg c <$>
      traverse (introSub p) progs
    intro' (IPurpose s) = pure $ IPurpose s
    intro' (IScope s) = pure $ IScope s
    body (BodyProg progs) = BodyProg <$> traverse (bodySub p) progs
    body' (Review c) = pure $ Review c
    body' (MainIdea c s) = pure $ MainIdea c s
    body' (MethsAndAnls c s) = pure $ MethsAndAnls c s
    smry (SmmryProg con) = pure $ SmmryProg con 
    aps (AppndxProg con) = pure $ AppndxProg con
  mkPlate b = DLPlate (b docSec) (b introSec) (b introSub) (b bodySec)
    (b bodySub) (b smmrySec) (b appendSec)