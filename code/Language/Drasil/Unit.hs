{-# Language TemplateHaskell, TypeFamilies #-}
module Language.Drasil.Unit (
    UnitDefn(..)
  , from_udefn, makeDerU, unitCon
  , (^:), (/:), (*:), (*$), (/$),(^$), new_unit
  , scale, shift, fshift, fscale
  , derUC, derUC', derUC'', unitWrapper
  , fund, comp_unitdefn, derCUC, derCUC', derCUC'', getsymb
  , makeDerU', getunit, unitWrapper',getCu,getunit
  ) where

import Control.Lens (Simple, Lens', Lens, (^.), makeLenses, view)
import Control.Arrow (second)

import Language.Drasil.Classes (HasUID(uid), NamedIdea(term), Idea(getA),
  Definition(defn), ConceptDomain(cdom), HasUnitSymbol(usymb), IsUnit(udefn),
  UnitEq(uniteq))
import Language.Drasil.Chunk.Concept (ConceptChunk, dcc, cc')
import Language.Drasil.Symbol
import Language.Drasil.UnitLang

import Language.Drasil.NounPhrase (cn,cn',NP)

-- | for defining fundamental units

data UnitDefn = UD { _vc :: ConceptChunk, _fsymb :: USymb, _dsymb :: Maybe USymb,  _ud :: Maybe UDefn, _cu :: [UnitDefn]}
makeLenses ''UnitDefn

instance HasUID        UnitDefn where uid = vc . uid
instance NamedIdea     UnitDefn where term   = vc . term
instance Idea          UnitDefn where getA c = getA (c ^. vc)
instance Definition    UnitDefn where defn = vc . defn
instance Eq            UnitDefn where a == b = (a ^. usymb) == (b ^. usymb)
instance ConceptDomain UnitDefn where
  type DOM UnitDefn = ConceptChunk
  cdom = vc . cdom
instance HasUnitSymbol UnitDefn where usymb f (UD a b c e d) = fmap (\x -> UD a x c e d) (f b)
instance IsUnit        UnitDefn where udefn = ud

getunit :: UnitDefn -> [UnitDefn]
getunit a = view cu a

data UnitEquation = UE {_contributingUnit :: [UnitDefn], _us :: USymb}
makeLenses ''UnitEquation

getsymb :: UnitEquation -> USymb
getsymb a = view us a

getCu :: UnitEquation -> [UnitDefn]
getCu a = view contributingUnit a

-- | Create a derived unit chunk from a concept and a unit equation
makeDerU :: ConceptChunk -> UDefn -> UnitDefn
makeDerU concept eqn = UD concept (from_udefn eqn) Nothing (Just eqn) []

makeDerU' :: ConceptChunk -> UnitEquation -> UnitDefn
makeDerU' concept eqn = UD concept (from_udefn $ USynonym $ getsymb eqn) Nothing (Just $ USynonym $ getsymb eqn) (getCu eqn)

derCUC, derCUC' :: String -> String -> String -> Symbol -> UnitEquation -> UnitDefn
derCUC a b c s ue = UD (dcc a (cn b) c) (US [(s,1)]) Nothing (Just $ FUSynonym $ getsymb ue) (getCu ue)
derCUC' a b c s ue = UD (dcc a (cn' b) c) (US [(s,1)]) Nothing (Just $ FUSynonym $ getsymb ue) (getCu ue)
-- | Create a derived unit chunk from an id, term (as 'String'), definition,
-- symbol, and unit equation
derUC, derUC' :: String -> String -> String -> Symbol -> UDefn -> UnitDefn
-- | Uses self-plural term
derUC  a b c s u = UD (dcc a (cn b) c) (US [(s,1)]) Nothing (Just u) []
-- | Uses term that pluralizes by adding *s* to the end
derUC' a b c s u = UD (dcc a (cn' b) c) (US [(s,1)]) Nothing (Just u) []


derCUC'' :: String -> NP -> String -> Symbol -> UnitEquation -> UnitDefn
derCUC'' a b c s ue = UD (dcc a b c) (US [(s,1)]) Nothing (Just $ FUSynonym $ getsymb ue) (getCu ue)
-- | Create a derived unit chunk from an id, term (as noun phrase), definition, 
-- symbol, and unit equation
derUC'' :: String -> NP -> String -> Symbol -> UDefn -> UnitDefn
derUC'' a b c s u = UD (dcc a b c) (US [(s,1)]) Nothing (Just u) []

--FIXME: Make this use a meaningful identifier.
-- | Helper for fundamental unit concept chunk creation. Uses the same string
-- for the identifier, term, and definition.
unitCon :: String -> ConceptChunk
unitCon s = dcc s (cn' s) s
---------------------------------------------------------

==== BASE ====
-- | for defining fundamental units
data UnitDefn = UD { _vc :: ConceptChunk
                   , _u :: USymb
                   }

-- don't export this
vc :: Simple Lens UnitDefn ConceptChunk
vc f (UD a b) = fmap (\x -> UD x b) (f a)

instance HasUID        UnitDefn where uid = vc . uid
instance NamedIdea     UnitDefn where term   = vc . term
instance Idea          UnitDefn where getA c = getA (c ^. vc)
instance Definition    UnitDefn where defn = vc . defn
instance Eq            UnitDefn where a == b = (a ^. usymb) == (b ^. usymb)
instance ConceptDomain UnitDefn where
  type DOM UnitDefn = ConceptChunk
  cdom = vc . cdom
instance HasUnitSymbol UnitDefn where usymb f (UD a b) = fmap (\x -> UD a x) (f b)
instance IsUnit        UnitDefn

-- | for defining Derived units
data DerUChunk = DUC { _uc :: UnitDefn
                     , _eq :: UDefn
                     }

-- don't export this either
duc :: Simple Lens DerUChunk UnitDefn
duc f (DUC a b) = fmap (\x -> DUC x b) (f a)

instance HasUID        DerUChunk where uid  = duc . uid
instance NamedIdea     DerUChunk where term = duc . term
instance Idea          DerUChunk where getA c = getA (c ^. duc)
instance Definition    DerUChunk where defn = duc . defn
instance ConceptDomain DerUChunk where
  type DOM DerUChunk = ConceptChunk
  cdom = duc . cdom
instance HasUnitSymbol DerUChunk where usymb  = duc . usymb
instance IsUnit        DerUChunk where

instance UnitEq DerUChunk where
  uniteq f (DUC a b) = fmap (\x -> DUC a x) (f b)

----------------------------------------------------------
==== BASE ====

-- | For allowing lists to mix the two, thus forgetting
-- the definition part
unitWrapper :: (IsUnit u)  => u -> UnitDefn
unitWrapper u = UD (cc' u (u ^. defn)) (u ^. usymb) Nothing (u ^. udefn) []

unitWrapper' :: UnitDefn -> UnitDefn
unitWrapper' u = UD (cc' u (u ^. defn)) (u ^. usymb) Nothing (u ^. udefn) (getunit u)

--- These conveniences go here, because we need the class
-- | Combinator for raising a unit to a power
helperUnit :: UnitDefn -> [UnitDefn]
helperUnit a = case a ^. udefn of
  Just x -> case x of
    FUSynonym _ -> [a]
    FUScale _ _ -> [a]
    FUShift _ _ -> [a]
    _ -> getunit a
  Nothing -> [a]

(^:) :: UnitDefn -> Integer -> UnitEquation
u ^: i = UE (helperUnit u) (upow (u ^. usymb))
  where
    upow (US l) = US $ map (second (* i)) l

-- | Combinator for dividing one unit by another
(/:) :: UnitDefn -> UnitDefn -> UnitEquation
u1 /: u2 = let US l1 = u1 ^. usymb
               US l2 = u2 ^. usymb in
  UE ((helperUnit u1) ++ (helperUnit u2)) (US $ l1 ++ map (second negate) l2)

-- | Combinator for multiplying two units together
(*:) :: UnitDefn -> UnitDefn -> UnitEquation
u1 *: u2 = let US l1 = u1 ^. usymb
               US l2 = u2 ^. usymb in
  UE ((helperUnit u1) ++ (helperUnit u2)) (US $ l1 ++ l2)

-- | Combinator for multiplying a unit and a symbol
(*$) :: UnitDefn -> UnitEquation -> UnitEquation
u1 *$ u2 = let US l1 = u1 ^. usymb
               US l2 = getsymb u2 in
  UE ((helperUnit u1)++(getCu u2)) (US $ l1 ++ l2)

-- | Combinator for dividing a unit and a symbol
(/$) :: UnitDefn -> UnitEquation -> UnitEquation
u1 /$ u2 = let US l1 = u1 ^. usymb
               US l2 = getsymb u2 in
  UE ((helperUnit u1)++(getCu u2)) (US $ l1 ++ map (second negate) l2)

-- | Combinator for mulitiplying two unit equations
(^$) :: UnitEquation -> UnitEquation -> UnitEquation
u1 ^$ u2 = let US l1 = getsymb u1
               US l2 = getsymb u2 in
  UE ((getCu u1)++(getCu u2)) (US $ l1 ++ l2)

-- | Combinator for scaling one unit by some number
scale :: IsUnit s => Double -> s -> UDefn
scale a b = UScale a (b ^. usymb)

fscale :: IsUnit s => Double -> s -> UDefn
fscale a b = FUScale a (b ^. usymb)

-- | Combinator for shifting one unit by some number
shift :: IsUnit s => Double -> s -> UDefn
shift a b = UShift a (b ^. usymb)

fshift :: IsUnit s => Double -> s -> UDefn
fshift a b = FUShift a (b ^. usymb)
-- | Smart constructor for new derived units from existing units.
new_unit :: String -> UnitEquation -> UnitDefn
new_unit s u = makeDerU' (unitCon s) u

fund :: String -> String -> String -> UnitDefn
fund nam desc sym = UD (dcc nam (cn' nam) desc) (US [(Atomic sym, 1)]) Nothing Nothing []

comp_unitdefn :: UnitDefn -> UnitDefn -> Ordering
comp_unitdefn a b = comp_usymb (a ^. usymb) (b ^. usymb)
