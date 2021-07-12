{-# LANGUAGE TemplateHaskell, Rank2Types, ScopedTypeVariables  #-}
module Theory.Drasil.InstanceModel
  ( InstanceModel
  , im, imNoDeriv, imNoRefs, imNoDerivNoRefs
  , im', imNoDeriv', imNoRefs', imNoDerivNoRefs'
  , getEqModQdsFromIm
  , qwUC, qwC
  ) where

import Language.Drasil
import Theory.Drasil.Classes (HasInputs(inputs), HasOutput(..))
import Data.Drasil.TheoryConcepts (inModel)

import Control.Lens ((^.), view, makeLenses, _1, _2) 
import Theory.Drasil.ModelKinds (ModelKinds(..), getEqModQds)

type Input = (QuantityDict, Maybe (RealInterval Expr Expr))
type Inputs = [Input]
type Output = QuantityDict
type OutputConstraints = [RealInterval Expr Expr]

-- | An instance model is a ModelKind that may have specific inputs, outputs, and output
-- constraints. It also has attributes like references (as 'DecRef's), derivation, labels ('ShortName'), reference address, and notes.
data InstanceModel = IM { _mk       :: ModelKinds
                        , _imTerm   :: NP
                        , _imInputs :: Inputs
                        , _imOutput :: (Output, OutputConstraints)
                        , _rf       :: [DecRef]
                        , _deri     :: Maybe Derivation
                        ,  lb       :: ShortName
                        ,  ra       :: String
                        , _notes    :: [Sentence]
                        }
makeLenses ''InstanceModel

-- | Finds the 'UID' of an 'InstanceModel'.
instance HasUID             InstanceModel where uid = mk . uid
-- | Finds the term ('NP') of the 'InstanceModel'.
instance NamedIdea          InstanceModel where term = imTerm
-- | Finds the idea contained in the 'InstanceModel'.
instance Idea               InstanceModel where getA = getA . (^. mk)
-- | Finds the definition of the 'InstanceModel'.
instance Definition         InstanceModel where defn = mk . defn
-- | Finds the domain of the 'InstanceModel'.
instance ConceptDomain      InstanceModel where cdom = cdom . (^. mk)
-- | Converts the 'InstanceModel's related expression into the display language.
instance Display            InstanceModel where toDispExpr = toDispExpr . (^. mk)
-- | Finds the derivation of the 'InstanceModel'. May contain Nothing.
instance HasDerivation      InstanceModel where derivations = deri
{--- | Finds 'Reference's contained in the 'InstanceModel'.
instance HasReference       InstanceModel where getReferences = rf-}
-- | Finds 'DecRef's contained in the 'InstanceModel'.
instance HasDecRef          InstanceModel where getDecRefs = rf
-- | Finds the 'ShortName' of the 'InstanceModel'.
instance HasShortName       InstanceModel where shortname = lb
-- | Finds the reference address of the 'InstanceModel'.
instance HasRefAddress      InstanceModel where getRefAdd l = RP (prepend $ abrv l) (ra l)
-- | Finds any additional notes for the 'InstanceModel'.
instance HasAdditionalNotes InstanceModel where getNotes = notes
-- | 'InstanceModel's have an 'Quantity'.
instance Quantity           InstanceModel where
-- | Finds the idea of an 'InstanceModel' (abbreviation).
instance CommonIdea         InstanceModel where abrv _ = abrv inModel
-- | Finds the reference address of an 'InstanceModel'.
instance Referable          InstanceModel where
  refAdd      = ra
  renderRef l = RP (prepend $ abrv l) (refAdd l)
-- | Finds the inputs of an 'InstanceModel'.
instance HasInputs          InstanceModel where
  inputs          = imInputs
-- | Finds the outputs and output constraints of an 'InstanceModel'.
instance HasOutput          InstanceModel where
  output          = imOutput . _1
  out_constraints = imOutput . _2
-- | Finds the output 'Symbol's of the 'InstanceModel'.
instance HasSymbol          InstanceModel where symbol = symbol . view output -- ???
-- | Finds the output 'Space' of the 'InstanceModel'.
instance HasSpace           InstanceModel where typ = output . typ
-- | Finds the units of the 'InstanceModel'.
instance MayHaveUnit        InstanceModel where getUnit = getUnit . view output

-- | Smart constructor for instance models with everything defined.
im :: ModelKinds -> Inputs -> Output -> 
  OutputConstraints -> [DecRef] -> Maybe Derivation -> String -> [Sentence] -> InstanceModel
im mkind = im' mkind (mkind ^. term)

-- | Smart constructor for instance models with no derivation.
imNoDeriv :: ModelKinds -> Inputs -> Output -> 
  OutputConstraints -> [DecRef] -> String -> [Sentence] -> InstanceModel
imNoDeriv mkind = imNoDeriv' mkind (mkind ^. term)

-- | Smart constructor for instance models with no references.
imNoRefs :: ModelKinds -> Inputs -> Output -> 
  OutputConstraints -> Maybe Derivation -> String -> [Sentence] -> InstanceModel
imNoRefs mkind = imNoRefs' mkind (mkind ^. term)

-- | Smart constructor for instance models with no derivations or references.
imNoDerivNoRefs :: ModelKinds -> Inputs -> Output -> 
  OutputConstraints -> String -> [Sentence] -> InstanceModel
imNoDerivNoRefs mkind = imNoDerivNoRefs' mkind (mkind ^. term)

-- | Smart constructor for instance models with everything defined.
im' :: ModelKinds -> NP -> Inputs -> Output -> 
  OutputConstraints -> [DecRef] -> Maybe Derivation -> String -> [Sentence] -> InstanceModel
im' mkind _ _  _ _  [] _  _  = error $ "Source field of " ++ mkind ^. uid ++ " is empty"
im' mkind n i o oc r der sn = 
  IM mkind n i (o, oc) r der (shortname' $ S sn) (prependAbrv inModel sn)

-- | Smart constructor for instance models with a custom term, and no derivation.
imNoDeriv' :: ModelKinds -> NP -> Inputs -> Output -> 
  OutputConstraints -> [DecRef] -> String -> [Sentence] -> InstanceModel
imNoDeriv' mkind _ _  _ _ [] _  = error $ "Source field of " ++ mkind ^. uid ++ " is empty"
imNoDeriv' mkind n i o oc r sn =
  IM mkind n i (o, oc) r Nothing (shortname' $ S sn) (prependAbrv inModel sn)

-- | Smart constructor for instance models with a custom term, and no references.
imNoRefs' :: ModelKinds -> NP -> Inputs -> Output -> 
  OutputConstraints -> Maybe Derivation -> String -> [Sentence] -> InstanceModel
imNoRefs' mkind n i o oc der sn = 
  IM mkind n i (o, oc) [] der (shortname' $ S sn) (prependAbrv inModel sn)

-- | Smart constructor for instance models with a custom term, and no derivations or references.
imNoDerivNoRefs' :: ModelKinds -> NP -> Inputs -> Output -> 
  OutputConstraints -> String -> [Sentence] -> InstanceModel
imNoDerivNoRefs' mkind n i o oc sn = 
  IM mkind n i (o, oc) [] Nothing (shortname' $ S sn) (prependAbrv inModel sn)

-- | For building a quantity with no constraint.
qwUC :: (Quantity q, MayHaveUnit q) => q -> Input 
qwUC x = (qw x, Nothing)

-- | For building a quantity with a constraint.
qwC :: (Quantity q, MayHaveUnit q) => q -> RealInterval Expr Expr -> Input 
qwC x y = (qw x, Just y)

-- | Grab all related 'QDefinition's from a list of instance models.
getEqModQdsFromIm :: [InstanceModel] -> [QDefinition]
getEqModQdsFromIm ims = getEqModQds (map _mk ims)
