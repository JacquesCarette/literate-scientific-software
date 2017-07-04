{- re-export many things to simplify external use -}
module Language.Drasil (
  -- Output.Formats
    DocType(SRS,MG,MIS,LPM,Website)
  -- SystemInformation
  , SystemInformation(..), Block(..)
  -- Recipe
  , Recipe(..)
  -- Expr
  , Expr(..), Relation, UFunc(..), BiFunc(..), Bound(..), DerivType(..)
  , log, abs, sin, cos, tan, sec, csc, cot, exp, sqrt, SymbolMap, symbolMap, vars
  , summation, product, cross
  -- all the stuff from Unicode
  , Greek(..), Special(..)
  -- Unit
  , Unit(..), UDefn(..), DerUChunk(..), FundUnit(..), UnitDefn(..)
  , from_udefn , makeDerU, unitCon
  , (^:), (/:), (*:), new_unit
  -- Chunk
  , Chunk(..), VarChunk(..), ConceptChunk
  , makeVC, vc, makeVCObj, SymbolForm(..)
  , dcc, dccWDS, cv, dcc', vc', ccs, cc, cc'
  , Quantity(..), ConVar(..), cvR, cvRs
  , Concept(..)
  , CommonIdea(..)
  --, commonidea, CI
  , commonIdea, CI, commonIdea', commonIdea''
  -- Chunk.NamedIdea
  , NamedIdea(..), NamedChunk, short, nc, nc', npnc
  , compoundterm, for, for', for'', of_, of_', of_'', of__, of'', compoundNC, compoundNC'
  , compoundNC'', compoundNC''', npnc', with, with', and_, and_', andRT, aNP, the, a_, theCustom
  -- Chunk.Constrained
  , Constrained(..), ConstrainedChunk(..), Constraint(..), ConstrConcept(..)
  , physc, sfwrc, constrained, cuc, cvc, constrained', cuc', constrainedNRV'
  , ConstrWrapper(..), cnstrw
  -- Chunk.Eq
  , QDefinition(..), fromEqn, fromEqn', getVC, equat
  -- Chunk.UncertainQuantity
  , UncertainQuantity(..), UncertainChunk(..), UncertQ, uq, uqNU, uqc, uqcNU, uqcND, uncrtnChunk, uvc
  , UncertainWrapper(..), uncrtnw
  -- Chunk.Unital
  , UnitalChunk(..), makeUCWDS, ucFromCV
  , uc, uc', ucs, ucs', ucsWS
  -- Chunk.Unitary
  , Unitary(..), UnitaryChunk, unitary
  -- Chunk.Relation
  , NamedRelation, makeNR, RelationConcept, makeRC, relat
  -- Chunk.Method
  , MethodChunk, fromEC, makeStdInputMethod, makeFileInputMethod
  , makeFileOutputMethod, makeMainMethod, input, output, exc, methcc, mType
  -- Chunk.Module
  , ModuleChunk, makeRecord, makeImpModule, makeImpModuleNoGen, makeUnimpModule
  , imp, hier, field, formatName, method, secret, uses
  -- Chunk.Req
  , ReqChunk(..)
  -- Chunk.LC
  , LCChunk(..)
  -- Chunk.Other
  , AssumpChunk, UCChunk
  --Chunk.Wrapper
  , cqs, qs, nw, CQSWrapper, QSWrapper, NWrapper
  --Chunk.UWrapper 
  , UWrapper, uw, ucw, UCWrapper
  -- Spec
  , USymb(..), Sentence(..), Accent(..), sParen, sSqBr
  , (+:+), (+:+.), (+.), sC, (+:), semiCol, sParenDash
  , sDash
  -- NounPhrase
  , NounPhrase(..), NP, pn, pn', pn'', pn''', pnIrr, cn, cn', cn'', cn''', cnIP
  , cnIrr, cnIES, cnICES, cnIS, cnUM, nounPhrase, nounPhrase', at_start, at_start'
  , CapitalizationRule(..)
  , PluralRule(..), compoundPhrase, compoundPhrase', compoundPhrase'', compoundPhrase''', titleize, titleize'
  , nounPhrase'', nounPhraseSP
  -- Document
  , LayoutObj(..), Document(..), DType(..), Section(..), Contents(..)
  , SecCons(..), ListType(..), ItemType(..), ListPair
  , section
  -- Reference
  , makeRef
  -- Space
  , Space(..)
  -- Symbol
  , Symbol(..), sub, sup, vec, hat, prime
  -- SymbolAlphabet
  , cA, cB, cC, cD, cE, cF, cG, cH, cI, cJ, cK, cL, cM, cN, cO, cP, cQ, cR, cS, cT, cU, cV, cW, cX, cY, cZ
  , lA, lB, lC, lD, lE, lF, lG, lH, lI, lJ, lK, lL, lM, lN, lO, lP, lQ, lR, lS, lT, lU, lV, lW, lX, lY, lZ
  -- Misc
  , mkTable, unit'2Contents, getAcc, unit_symb, introduceAbb, phrase, plural
  -- Printing.Helpers
  , capitalize, paren, sqbrac
  -- Generate
  , gen, genCode
  -- People
  , People, Person, person, HasName, name, manyNames, person', personWM
  , personWM', mononym
  -- CodeSpec
  , CodeSpec, codeSpec
) where

import Prelude hiding (log, sin, cos, tan, sqrt, id, return, print, break, exp, product)
import Language.Drasil.SystemInformation
import Language.Drasil.Expr (Expr(..), Relation, UFunc(..), BiFunc(..), 
          Bound(..),DerivType(..), log, sin, cos, tan, sqrt, sec, csc, cot, exp,
          summation, product, cross)
import Language.Drasil.Expr.Extract (SymbolMap, symbolMap, vars)
import Language.Drasil.Output.Formats (DocType(SRS,MG,MIS,LPM,Website))
import Language.Drasil.Document (LayoutObj(..), Document(..), DType(..)
  , Section(..), Contents(..), SecCons(..), ListType(..), ItemType(..), section
  , ListPair)
import Language.Drasil.Recipe (Recipe(..))
import Language.Drasil.Unicode -- all of it
import Language.Drasil.Unit -- all of it
import Language.Drasil.Chunk
import Language.Drasil.Chunk.NamedIdea
import Language.Drasil.Chunk.Concept
import Language.Drasil.Chunk.SymbolForm
import Language.Drasil.Chunk.CommonIdea
import Language.Drasil.Chunk.VarChunk
import Language.Drasil.Chunk.Quantity
import Language.Drasil.Chunk.UncertainQuantity
import Language.Drasil.Chunk.ConVar
import Language.Drasil.Chunk.Eq (QDefinition(..), fromEqn, fromEqn', getVC, equat)
import Language.Drasil.Chunk.Constrained
import Language.Drasil.Chunk.Unital(UnitalChunk(..), makeUCWDS, ucFromCV
                                  , uc, uc', ucs, ucs', ucsWS)
import Language.Drasil.Chunk.Unitary
import Language.Drasil.Chunk.Relation(NamedRelation, makeNR, RelationConcept, 
                                      makeRC, relat)
import Language.Drasil.Chunk.Req
import Language.Drasil.Chunk.LC
import Language.Drasil.Chunk.Method
import Language.Drasil.Chunk.Module
import Language.Drasil.Chunk.Other
import Language.Drasil.Chunk.Wrapper
import Language.Drasil.Chunk.Wrapper.QSWrapper
import Language.Drasil.Chunk.Wrapper.UWrapper
import Language.Drasil.NounPhrase hiding (at_start, at_start', titleize
                                          , titleize', phrase, plural)
import Language.Drasil.Space (Space(..))
import Language.Drasil.Spec (USymb(..), Sentence(..), Accent(..), 
                              sParen, sSqBr, sC, (+:+), (+:+.), (+.), (+:),
                              semiCol, sParenDash, sDash)
import Language.Drasil.Reference (makeRef)
import Language.Drasil.Symbol (Symbol(..), sub, sup, vec, hat, prime)
import Language.Drasil.SymbolAlphabet
import Language.Drasil.Misc -- all of it
import Language.Drasil.Printing.Helpers (capitalize, paren, sqbrac)
import Language.Drasil.Generate
import Language.Drasil.People (People, Person, person, HasName(..), manyNames
                               ,person', personWM, personWM', mononym)
import Language.Drasil.CodeSpec                             
