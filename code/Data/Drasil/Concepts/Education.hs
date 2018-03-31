module Data.Drasil.Concepts.Education where

import Language.Drasil hiding (year)
import Data.Drasil.Concepts.Documentation
import Data.Drasil.Concepts.PhysicalProperties

calculus, civil, degree_, engineering, structural, mechanics,
  undergraduate, highSchool, physical_, chemistry :: NamedChunk

calculus        = nc "calculus"       (cn   "calculus"     )
civil           = nc "civil"          (cn'  "civil"        )--FIXME: Adjective
degree_         = nc "degree"         (cn'  "degree"       )
engineering     = nc "engineering"    (cn'  "engineering"  )
mechanics       = nc "mechanics"      (cn   "mechanics"    )
structural      = nc "structural"     (cn'  "structural"   )--FIXME: Adjective
undergraduate   = nc "undergraduate"  (cn'  "undergraduate")
highSchool      = nc "highSchool"     (cn'  "high school"  )
chemistry       = nc "chemistry"      (cn'  "chemistry"    )
physical_       = nc "physical"       (cn'  "physical"     )--FIXME: Adjective

undergradDegree, scndYrCalculus, solidMechanics, secondYear, structuralEng,
  structuralMechanics, civilEng, highSchoolCalculus, highSchoolPhysics,
  frstYr, physChem :: NamedChunk

civilEng                     = compoundNC civil engineering
physChem                     = compoundNC physical_ chemistry
highSchoolCalculus           = compoundNC highSchool calculus
highSchoolPhysics            = compoundNC highSchool physics
scndYrCalculus               = compoundNC secondYear calculus
frstYr                       = compoundNC first year
secondYear                   = compoundNC second_ year
solidMechanics               = compoundNC solid mechanics
structuralEng                = compoundNC structural engineering
structuralMechanics          = compoundNC structural mechanics
undergradDegree              = compoundNC undergraduate degree_
