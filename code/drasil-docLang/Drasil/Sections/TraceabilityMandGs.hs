module Drasil.Sections.TraceabilityMandGs (generateTraceTable,tvAssumps,
  tvDataDefns, tvGenDefns, tvTheoryModels, tvInsModels, tvGoals, tvReqs,
  tvChanges, traceMatAssumpAssump, traceMatAssumpOther, traceMatRefinement, traceMatOtherReq,
  traceMatStandard) where

import Drasil.DocumentLanguage.Core (TraceConfig(TraceConfig))
import Drasil.DocumentLanguage.TraceabilityMatrix (generateTraceTableView,
  traceMReferrers, traceView, traceViewCC, TraceViewCat)

import Data.Drasil.Concepts.Documentation (assumption, assumpDom, chgProbDom,
  goalStmt, goalStmtDom, requirement, reqDom, item, section_, likelyChg,
  unlikelyChg)
import qualified Data.Drasil.TheoryConcepts as Doc (genDefn, dataDefn, inModel, thModel)
import Database.Drasil(SystemInformation, _sysinfodb, gendefTable, dataDefnTable,
  insmodelTable, theoryModelTable)
import Language.Drasil

-- | Makes a Traceability Table/Matrix that contains Items of Different Sections.
generateTraceTable :: SystemInformation -> LabelledContent
generateTraceTable = generateTraceTableView "Tracey"
  (titleize' item +:+ S "of Different" +:+ titleize' section_) [tvEverything] [tvEverything]

-- | Traceabiliy viewing everything. Takes a 'UID' and a 'ChunkDB'. Returns a list of 'UID's.
tvEverything :: TraceViewCat
tvEverything = flip (const id)
-- | Traceabiliy viewing assumptions. Takes a 'UID' and a 'ChunkDB'. Returns a list of 'UID's.
tvAssumps :: TraceViewCat
tvAssumps = traceViewCC assumpDom
-- | Traceabiliy viewing data definitions. Takes a 'UID' and a 'ChunkDB'. Returns a list of 'UID's.
tvDataDefns :: TraceViewCat
tvDataDefns = traceView dataDefnTable
-- | Traceabiliy viewing general definitions. Takes a 'UID' and a 'ChunkDB'. Returns a list of 'UID's.
tvGenDefns :: TraceViewCat
tvGenDefns = traceView gendefTable
-- | Traceabiliy viewing theory models. Takes a 'UID' and a 'ChunkDB'. Returns a list of 'UID's.
tvTheoryModels :: TraceViewCat
tvTheoryModels = traceView theoryModelTable
-- | Traceabiliy viewing instance models. Takes a 'UID' and a 'ChunkDB'. Returns a list of 'UID's.
tvInsModels :: TraceViewCat
tvInsModels = traceView insmodelTable
-- | Traceabiliy viewing goals. Takes a 'UID' and a 'ChunkDB'. Returns a list of 'UID's.
tvGoals :: TraceViewCat
tvGoals = traceViewCC goalStmtDom
-- | Traceabiliy viewing requirements. Takes a 'UID' and a 'ChunkDB'. Returns a list of 'UID's.
tvReqs :: TraceViewCat
tvReqs = traceViewCC reqDom
-- | Traceabiliy viewing changes. Takes a 'UID' and a 'ChunkDB'. Returns a list of 'UID's.
tvChanges :: TraceViewCat
tvChanges = traceViewCC chgProbDom

-- | Assumptions on the assumptions of a traceabiliy matrix.
traceMatAssumpAssump :: TraceConfig
traceMatAssumpAssump = TraceConfig "TraceMatAvsA" [plural assumption +:+
  S "on the" +:+ plural assumption] (titleize' assumption +:+
  S "and Other" +:+ titleize' assumption ) [tvAssumps] [tvAssumps]

-- | Other assumptions of the traceability matrix
traceMatAssumpOther :: TraceConfig
traceMatAssumpOther = TraceConfig "TraceMatAvsAll" [plural Doc.dataDefn,
  plural Doc.thModel, plural Doc.genDefn, plural Doc.inModel, plural requirement,
  plural likelyChg, plural unlikelyChg +:+ S "on the" +:+ plural assumption]
  (titleize' assumption +:+ S "and Other" +:+ titleize' item) [tvAssumps]
  [tvDataDefns, tvTheoryModels, tvGenDefns, tvInsModels, tvReqs, tvChanges]

-- | Refinement of the traceability matrix.
traceMatRefinement :: TraceConfig
traceMatRefinement = TraceConfig "TraceMatRefvsRef" [plural Doc.dataDefn,
  plural Doc.thModel, plural Doc.genDefn, plural Doc.inModel +:+
  S "with each other"] (titleize' item +:+ S "and Other" +:+ titleize' section_)
  [tvDataDefns, tvTheoryModels, tvGenDefns, tvInsModels]
  [tvDataDefns, tvTheoryModels, tvGenDefns, tvInsModels]

-- | Records other requirements. Converts the 'SystemInformation' into a 'TraceConfig'.
traceMatOtherReq :: SystemInformation -> TraceConfig
traceMatOtherReq si = TraceConfig "TraceMatAllvsR" [x plural +:+ S "on the" +:+
  plural Doc.dataDefn, plural Doc.thModel, plural Doc.genDefn, plural Doc.inModel]
  (x titleize' +:+ S "and Other" +:+ titleize' item) [tvDataDefns, tvTheoryModels,
  tvGenDefns, tvInsModels, tvReqs] [tvGoals, tvReqs] where
    x g = foldl (\a (f,t) -> a `sC'` case traceMReferrers (flip f $ _sysinfodb si) $
      _sysinfodb si of
      [] -> mempty
      _ -> g t) mempty [(tvReqs, requirement), (tvGoals, goalStmt)]
    sC' EmptyS b = b
    sC' a EmptyS = a
    sC' a b = sC a b

-- | Contains traceability matrix assumptions, other assumptions, refinement, and other requirements.
traceMatStandard :: SystemInformation -> [TraceConfig]
traceMatStandard s = map ($ s) [const traceMatAssumpAssump, const traceMatAssumpOther, const traceMatRefinement,
  traceMatOtherReq]
