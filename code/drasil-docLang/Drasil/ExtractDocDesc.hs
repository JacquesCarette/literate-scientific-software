{-# LANGUAGE LambdaCase #-}
module Drasil.ExtractDocDesc (getDocDesc, egetDocDesc, ciGetDocDesc) where

import Control.Lens((^.))
import Drasil.DocumentLanguage
import Language.Drasil hiding (Manual, Vector, Verb)
import Theory.Drasil (Theory(..))
import Data.List(transpose)

import Data.Functor.Constant (Constant(Constant))
import Data.Generics.Multiplate (Multiplate(multiplate, mkPlate), purePlate, preorderFold, foldFor)

data DLPlate f = DLPlate {
  docSec :: DocSection -> f DocSection,
  refSec :: RefSec -> f RefSec,
  introSec :: IntroSec -> f IntroSec,
  introSub :: IntroSub -> f IntroSub,
  stkSec :: StkhldrSec -> f StkhldrSec,
  stkSub :: StkhldrSub -> f StkhldrSub,
  gsdSec :: GSDSec -> f GSDSec,
  gsdSub :: GSDSub -> f GSDSub,
  ssdSec :: SSDSec -> f SSDSec,
  ssdSub :: SSDSub -> f SSDSub,
  pdSec :: ProblemDescription -> f ProblemDescription,
  pdSub :: PDSub -> f PDSub,
  scsSub :: SCSSub -> f SCSSub,
  reqSec :: ReqrmntSec -> f ReqrmntSec,
  reqSub :: ReqsSub -> f ReqsSub,
  lcsSec :: LCsSec -> f LCsSec,
  lcsSec' :: LCsSec' -> f LCsSec',
  ucsSec :: UCsSec -> f UCsSec,
  traceSec :: TraceabilitySec -> f TraceabilitySec,
  existSolnSec :: ExistingSolnSec -> f ExistingSolnSec,
  auxConsSec :: AuxConstntSec -> f AuxConstntSec,
  appendSec :: AppndxSec -> f AppndxSec
}

instance Multiplate DLPlate where
  multiplate p = DLPlate ds res intro intro' stk stk' gs gs' ss ss' pd pd' sc
    rs rs' lcp lcp' ucp ts es acs aps where
    res (RefProg c x) = RefProg <$> pure c <*> pure x
    ds (IntroSec x) = IntroSec <$> introSec p x
    ds (StkhldrSec x) = StkhldrSec <$> stkSec p x
    ds (GSDSec x) = GSDSec <$> gsdSec p x
    ds (SSDSec x) = SSDSec <$> ssdSec p x
    ds (ReqrmntSec x) = ReqrmntSec <$> reqSec p x
    ds (LCsSec x) = LCsSec <$> lcsSec p x
    ds (LCsSec' x) = LCsSec' <$> lcsSec' p x
    ds (UCsSec x) = UCsSec <$> ucsSec p x
    ds (TraceabilitySec x) = TraceabilitySec <$> traceSec p x
    ds (ExistingSolnSec x) = ExistingSolnSec <$> existSolnSec p x
    ds (AuxConstntSec x) = AuxConstntSec <$> auxConsSec p x
    ds (AppndxSec x) = AppndxSec <$> appendSec p x
    ds x = pure x

    intro (IntroProg s1 s2 progs) = IntroProg <$> pure s1 <*> pure s2 <*>
      traverse (introSub p) progs
    intro' (IPurpose s) = IPurpose <$> pure s
    intro' (IScope s1 s2) = IScope <$> pure s1 <*> pure s2
    intro' (IChar s1 s2 s3) = IChar <$> pure s1 <*> pure s2 <*> pure s3
    intro' (IOrgSec s1 c sect s2) = IOrgSec <$> pure s1 <*> pure c <*> pure sect <*> pure s2
    stk (StkhldrProg c s) = StkhldrProg <$> pure c <*> pure s
    stk (StkhldrProg2 progs) = StkhldrProg2 <$> traverse (stkSub p) progs
    stk' (Client c s) = Client <$> pure c <*> pure s
    stk' (Cstmr c) = Cstmr <$> pure c
    gs (GSDProg s1 c labcon s2) = GSDProg <$> pure s1 <*> pure c <*> pure labcon <*> pure s2
    gs (GSDProg2 x) = GSDProg2 <$> traverse (gsdSub p) x
    gs' (SysCntxt c) = SysCntxt <$> pure c
    gs' (UsrChars c) = UsrChars <$> pure c
    gs' (SystCons c s) = SystCons <$> pure c <*> pure s
    ss (SSDProg progs) = SSDProg <$> traverse (ssdSub p) progs
    ss' (SSDProblem prog) = SSDProblem <$> pdSec p prog
    ss' (SSDSolChSpec (SCSProg spec)) = SSDSolChSpec . SCSProg <$> traverse (scsSub p) spec
    pd (PDProg s sect progs) = PDProg <$> pure s <*> pure sect <*> traverse (pdSub p) progs
    pd' (Goals s ci) = Goals <$> pure s <*> pure ci
    pd' (PhySysDesc nm s lc c) = PhySysDesc <$> pure nm <*> pure s <*> pure lc <*> pure c
    sc Assumptions = pure Assumptions
    sc (TMs s f t) = TMs <$> pure s <*> pure f <*> pure t
    sc (GDs s f g d) = GDs <$> pure s <*> pure f <*> pure g <*> pure d
    sc (DDs s f dd d) = DDs <$> pure s <*> pure f <*> pure dd <*> pure d
    sc (IMs s f i d) = IMs <$> pure s <*> pure f <*> pure i <*> pure d
    sc (Constraints s1 s2 s3 l) = Constraints <$> pure s1 <*> pure s2 <*> pure s3 <*> pure l
    sc (CorrSolnPpties c) = CorrSolnPpties <$> pure c
    rs (ReqsProg reqs) = ReqsProg <$> traverse (reqSub p) reqs
    rs' (FReqsSub ci con) = FReqsSub <$> pure ci <*> pure con
    rs' (NonFReqsSub c) = NonFReqsSub <$> pure c
    lcp (LCsProg c) = LCsProg <$> pure c
    lcp' (LCsProg' c) = LCsProg' <$> pure c
    ucp (UCsProg c) = UCsProg <$> pure c
    ts (TraceabilityProg llc sen con sect) = TraceabilityProg <$> pure llc <*>
      pure sen <*> pure con <*> pure sect
    es (ExistSolnProg contents) = ExistSolnProg <$> pure contents
    acs (AuxConsProg ci qdef) = AuxConsProg <$> pure ci <*> pure qdef
    aps (AppndxProg con) = AppndxProg <$> pure con
  mkPlate b = DLPlate (b docSec) (b refSec) (b introSec) (b introSub) (b stkSec)
    (b stkSub) (b gsdSec) (b gsdSub) (b ssdSec) (b ssdSub) (b pdSec) (b pdSub)
    (b scsSub) (b reqSec) (b reqSub) (b lcsSec) (b lcsSec') (b ucsSec)
    (b traceSec) (b existSolnSec) (b auxConsSec) (b appendSec)

egetDocDesc :: DocDesc -> [Expr]
egetDocDesc = concatMap egetDocSec

exprOnlyPlate :: DLPlate (Constant [Expr])
exprOnlyPlate = preorderFold $ purePlate {
  refSec = Constant <$> \(RefProg c _) -> con [c],
  introSub = Constant <$> \case
    (IOrgSec _ _ s _) -> mSec [s]
    _ -> [],
  gsdSec = Constant <$> \case
    (GSDProg s1 c1 c2 s2) -> mSec s1 ++ con [c1] ++ con c2 ++ mSec s2
    (GSDProg2 _) -> [],
  gsdSub = Constant <$> \case
    (SysCntxt c) -> con c
    (UsrChars c) -> con c
    (SystCons c s) -> con c ++ mSec s,
  pdSec = Constant <$> \(PDProg _ s _) -> mSec s,
  pdSub = Constant <$> \case
    (PhySysDesc _ _ lc c) -> con [lc] ++ con c
    (Goals _ _) -> [],
  scsSub = Constant <$> \case
    Assumptions -> []
    (TMs _ _ t) -> let r = concatMap (\x -> x ^. invariants ++
                           defExp (x ^. defined_quant ++ x ^. defined_fun) ++
                           r (x ^. valid_context)) in r t
    (DDs _ _ d _) -> map sy d ++ defExp d
    (GDs _ _ g _) -> map (^. relat) g
    (IMs _ _ i _) -> map (^. relat) i
    (Constraints _ _ _ lc) -> con lc
    (CorrSolnPpties c) -> con c,
  reqSub = Constant <$> \case
    (FReqsSub _ c) -> con c
    (NonFReqsSub _) -> [],
  lcsSec = Constant <$> \(LCsProg c) -> con c,
  ucsSec = Constant <$> \(UCsProg c) -> con c,
  traceSec = Constant <$> \(TraceabilityProg lc _ c s) -> con lc ++ con c ++ mSec s,
  auxConsSec = Constant <$> \(AuxConsProg _ qdef) -> defExp qdef,
  existSolnSec = Constant <$> \(ExistSolnProg c) -> con c,
  appendSec = Constant <$> \(AppndxProg c) -> con c
  } where
    defExp :: DefiningExpr a => [a] -> [Expr]
    defExp = map (^. defnExpr)
    mSec :: [Section] -> [Expr]
    mSec = concatMap egetSec
    con :: HasContents a => [a] -> [Expr]
    con = concatMap egetCon'

egetDocSec :: DocSection -> [Expr]
egetDocSec x = foldFor docSec exprOnlyPlate x ++ concatMap sentToExp (getDocSec x)

sentToExp :: Sentence -> [Expr]
sentToExp ((:+:) s1 s2) = sentToExp s1 ++ sentToExp s2
sentToExp (E e) = [e]
sentToExp _ = []

egetSec :: Section -> [Expr]
egetSec (Section _ sc _ ) = concatMap egetSecCon sc

egetSecCon :: SecCons -> [Expr]
egetSecCon (Sub s) = egetSec s
egetSecCon (Con c) = egetCon' c

egetCon' :: HasContents a => a -> [Expr]
egetCon' c = egetCon (c ^. accessContents)

egetCon :: RawContent -> [Expr]
egetCon (EqnBlock e) = [e]
egetCon (Defini _ []) = []
egetCon (Defini dt (hd:tl)) = concatMap egetCon' (snd hd) ++ egetCon (Defini dt tl)
egetCon _ = []

getDocDesc :: DocDesc -> [Sentence]
getDocDesc = concatMap getDocSec

sentencePlate :: DLPlate (Constant [Sentence])
sentencePlate = preorderFold $ purePlate {
    refSec = Constant <$> \(RefProg c _) -> con [c],
    introSec = Constant <$> \(IntroProg s1 s2 _) -> [s1, s2],
    introSub = Constant <$> \case
      (IPurpose s) -> [s]
      (IScope s1 s2) -> [s1, s2]
      (IChar s1 s2 s3) -> concat [s1, s2, s3]
      (IOrgSec s1 _ _ s2) -> [s1, s2],
    stkSec = Constant <$> \case
      (StkhldrProg _ s) -> [s]
      _ -> [],
    stkSub = Constant <$> \case
      (Client _ s) -> [s]
      _ -> [],
    gsdSec = Constant <$> \case
      (GSDProg s1 c1 c2 s2) -> mSec s1 ++ con (c1 : c2) ++ mSec s2
      _ -> [],
    gsdSub = Constant <$> \case
      (SysCntxt c) -> con c
      (UsrChars c) -> con c
      (SystCons c s) -> con c ++ mSec s,
    pdSec = Constant <$> \(PDProg s sect _) -> s : mSec sect,
    pdSub = Constant <$> \case
      (PhySysDesc _ s lc c) -> s ++ con [lc] ++ con c
      (Goals s c) -> s ++ def c,
    scsSub = Constant <$> \case
      Assumptions -> []
      (TMs s _ t) -> let r = mappend s . concatMap (\x -> def (x ^. operations) ++
                             def (x ^. defined_quant) ++ notes [x] ++
                             r (x ^. valid_context)) in r t
      (DDs s _ d _) -> s ++ der d ++ notes d
      (GDs s _ d _) -> def d ++ s ++ der d ++ notes d
      (IMs s _ d _) -> s ++ der d ++ notes d
      (Constraints s1 s2 s3 lb) -> [s1, s2, s3] ++ con lb
      (CorrSolnPpties c) -> con c,
    reqSub = Constant <$> \case
      (FReqsSub c _) -> def c
      (NonFReqsSub c) -> def c,
    lcsSec = Constant <$> \(LCsProg c) -> con c,
    lcsSec' = Constant <$> \(LCsProg' c) -> def c,
    ucsSec = Constant <$> \(UCsProg c) -> con c,
    traceSec = Constant <$> 
      \(TraceabilityProg lc s c g) -> con lc ++
      s ++ con c ++ mSec g,
    existSolnSec = Constant <$> \(ExistSolnProg c) -> con c,
    auxConsSec = Constant <$> \(AuxConsProg _ qdef) -> def qdef,
    appendSec = Constant <$> \(AppndxProg c) -> con c
  } where
    def :: Definition a => [a] -> [Sentence]
    def = map (^. defn)
    der :: HasDerivation a => [a] -> [Sentence]
    der = concatMap (^. derivations)
    notes :: HasAdditionalNotes a => [a] -> [Sentence]
    notes = concatMap (^. getNotes)
    mSec :: [Section] -> [Sentence]
    mSec = concatMap getSec
    con :: HasContents a => [a] -> [Sentence]
    con = concatMap getCon'

getDocSec :: DocSection -> [Sentence]
getDocSec = foldFor docSec sentencePlate

getSec :: Section -> [Sentence]
getSec (Section t sc _ ) = t : concatMap getSecCon sc

getSecCon :: SecCons -> [Sentence]
getSecCon (Sub s) = getSec s
getSecCon (Con c) = getCon' c

getCon' :: HasContents a => a -> [Sentence]
getCon' = getCon . (^. accessContents)

getCon :: RawContent -> [Sentence]
getCon (Table s1 s2 t _) = isVar (s1, transpose s2) ++ [t]
getCon (Paragraph s)       = [s]
getCon EqnBlock{}          = []
getCon (Enumeration lst)   = getLT lst
getCon (Figure l _ _)    = [l]
getCon (Bib bref)          = getBib bref
getCon (Graph [(s1, s2)] _ _ l) = s1 : s2 : [l]
getCon Graph{}             = []
getCon (Defini _ [])       = []
getCon (Defini dt (hd:fs)) = concatMap getCon' (snd hd) ++ getCon (Defini dt fs)

-- This function is used in collecting sentence from table.
-- Since only the table's first Column titled "Var" should be collected,
-- this function is used to filter out only the first Column of Sentence.
isVar :: ([Sentence], [[Sentence]]) -> [Sentence]
isVar (S "Var" : _, hd1 : _) = hd1
isVar (_ : tl, _ : tl1) = isVar (tl, tl1)
isVar ([], _) = []
isVar (_, []) = []

getBib :: (HasFields c) => [c] -> [Sentence]
getBib a = map getField $ concatMap (^. getFields) a

getField :: CiteField -> Sentence
getField (Address s) = S s
getField Author{} = EmptyS
getField (BookTitle s) = S s
getField Chapter{} = EmptyS
getField Edition{} = EmptyS
getField Editor{} = EmptyS
getField HowPublished{} = EmptyS
getField (Institution s) = S s
getField (Journal s) = S s
getField Month{} = EmptyS
getField (Note s) = S s
getField Number{} = EmptyS
getField (Organization s) = S s
getField Pages{} = EmptyS
getField (Publisher s) = S s
getField (School s) = S s
getField (Series s) = S s
getField (Title s) = S s
getField (Type s) = S s
getField Volume{} = EmptyS
getField Year{} = EmptyS

getLT :: ListType -> [Sentence]
getLT (Bullet it) = concatMap (getIL . fst) it
getLT (Numeric it) = concatMap (getIL . fst) it
getLT (Simple lp) = concatMap getLP lp
getLT (Desc lp) = concatMap getLP lp
getLT (Definitions lp) = concatMap getLP lp

getLP :: ListTuple -> [Sentence]
getLP (t, it, _) = t : getIL it

getIL :: ItemType -> [Sentence]
getIL (Flat s) = [s]
getIL (Nested h lt) = h : getLT lt

ciGetDocDesc :: DocDesc -> [CI]
ciGetDocDesc = concatMap ciGetDocSec

ciGetDocSec :: DocSection -> [CI]
ciGetDocSec = foldFor docSec ciPlate

ciPlate :: DLPlate (Constant [CI])
ciPlate = preorderFold $ purePlate {
  introSub = Constant <$> \case
    (IOrgSec _ ci _ _) -> [ci]
    _ -> [],
  stkSec = Constant <$> \case
    (StkhldrProg ci _) -> [ci]
    (StkhldrProg2 _) -> [],
  stkSub = Constant <$> \case
   (Client ci _) -> [ci]
   (Cstmr ci) -> [ci],
   auxConsSec = Constant <$> \(AuxConsProg ci _) -> [ci]
}
