module Language.Drasil.Printing.Import.Sentence where

import Language.Drasil hiding (neg, sec, symbol, isIn)
import Database.Drasil (ChunkDB, defResolve, refResolve, refTable)
import Utils.Drasil (foldNums, checkValidStr)

import qualified Language.Drasil.Printing.AST as P
import Language.Drasil.Printing.PrintingInformation
  (PrintingInformation, ckdb, stg)

import Language.Drasil.Printing.Import.DisplayExpr (dispExpr)
import Language.Drasil.Printing.Import.Helpers
  (lookupC, lookupT, lookupS, lookupP)
import Language.Drasil.Printing.Import.Symbol (symbol, pUnit)

import Control.Lens ((^.))
import Data.Maybe (fromMaybe)


-- | Translates 'Sentence' to the printable representation of a 'Sentence' ('Spec').
spec :: PrintingInformation -> Sentence -> P.Spec
  -- make sure these optimizations are clear
spec sm (EmptyS :+: b)          = spec sm b
spec sm (a :+: EmptyS)          = spec sm a
spec sm (a :+: b)               = spec sm a P.:+: spec sm b
spec _ (S s)                    = either error P.S $ checkValidStr s invalidChars
  where invalidChars = ['<', '>', '\"', '&', '#', '$', '%', '&', '~', '^', '\\', '{', '}']
spec _ (Sy s)                   = P.E $ pUnit s
spec _ Percent                  = P.E $ P.MO P.Perc
spec _ (P s)                    = P.E $ symbol s
spec sm (SyCh s)                = P.E $ symbol $ lookupC (sm ^. stg) (sm ^. ckdb) s
spec sm (Ch TermStyle caps s)   = spec sm $ lookupT (sm ^. ckdb) s caps
spec sm (Ch ShortStyle caps s)  = spec sm $ lookupS (sm ^. ckdb) s caps
spec sm (Ch PluralTerm caps s)  = spec sm $ lookupP (sm ^. ckdb) s caps
spec sm (Ref u EmptyS notes) =
  let reff = refResolve u (sm ^. ckdb . refTable) in
  case reff of 
    (Reference _ (RP rp ra) sn) ->
      P.Ref P.Internal ra (spec sm $ renderShortName (sm ^. ckdb) rp sn)
    (Reference _ (Citation ra) _) ->
      P.Ref (P.Cite2 (spec sm (renderCitInfo notes)))    ra (spec sm $ S ra) 
    (Reference _ (URI ra) sn) ->
      P.Ref P.External    ra (spec sm $ renderURI sm sn)
spec sm (Ref u dName notes) =
  let reff = refResolve u (sm ^. ckdb . refTable) in
  case reff of 
    (Reference _ (RP _ ra) _) ->
      P.Ref P.Internal ra (spec sm dName)
    (Reference _ (Citation ra) _) ->
      P.Ref (P.Cite2 (spec sm (renderCitInfo notes)))   ra (spec sm dName) 
    (Reference _ (URI ra) _) ->
      P.Ref P.External    ra (spec sm dName)
spec sm (Quote q)          = P.Quote $ spec sm q
spec _  EmptyS             = P.EmptyS
spec sm (E e)              = P.E $ dispExpr e sm

-- | Renders the shortname of a reference/domain.
renderShortName :: ChunkDB -> IRefProg -> ShortName -> Sentence
renderShortName ctx (Deferred u) _ = S $ fromMaybe (error "Domain has no abbreviation.") $
  getA $ defResolve ctx u --Need defResolve instead of refResolve since only ConceptInstance
  -- uses this case for domains and we want the short name from there. 
  -- Used to be: S $ getRefAdd $ refResolve u (ctx ^. refTable)
renderShortName ctx (RConcat a b) sn = renderShortName ctx a sn :+: renderShortName ctx b sn
renderShortName _ (RS s) _ = S s
renderShortName _ Name sn = getSentSN sn

-- | Render a uniform resource locator as a 'Sentence'.
renderURI :: ctx -> ShortName -> Sentence
renderURI _ = getSentSN

-- | Renders citation information.
renderCitInfo :: RefInfo -> Sentence
renderCitInfo  None          = EmptyS
renderCitInfo (RefNote   rn) = sParen (S rn)
renderCitInfo (Equation [x]) = sParen (S "Eq." +:+ S (show x))
renderCitInfo (Equation  i ) = sParen (S "Eqs." +:+ foldNums "-" i)
renderCitInfo (Page     [x]) = sParen (S "pg." +:+ S (show x))
renderCitInfo (Page      i ) = sParen (S "pp." +:+ foldNums "-" i)
