module Data.Drasil.Concepts.Computation where

import Language.Drasil
import Data.Drasil.Concepts.Documentation (datum, input_, literacy, output_, 
    quantity, type_, value, variable)
import Data.Drasil.Concepts.Math (parameter)
import Data.Drasil.Phrase (compoundNC, compoundNCPlPh, compoundNCPlPl)
import Data.Drasil.IdeaDicts

modCalcDesc :: Sentence -> ConceptChunk
modCalcDesc = dccWDS "modCalcDesc" (cn' "calculation")

compcon :: [NamedChunk]
compcon = [application, computer, structure, dataStruct, dataStruct', dataType, dataType', 
  inDatum, outDatum, inParam, inVar, inValue, inQty, computerLiteracy, computerApp]

algorithm, sequence, tuple :: ConceptChunk
algorithm    = dcc "algorithm" (cn' "algorithm")
  "A series of steps to be followed in calculations and problem-solving operations"
sequence     = dcc "sequence"  (cn' "sequence") 
  ("A list that represents a countable number of ordered values of the same data type,"++
  " where the same value may occur more than once")
tuple        = dcc "tuple"     (cn' "tuple")
  "A list of values, potentially of different types"

application, computer, structure :: NamedChunk
os :: CI
-------------------------------------------------------------------------------
--  NC      |     |      id       |       term               |  abbreviation
-------------------------------------------------------------------------------
application  = nc  "application"    (cn' "application")      
computer     = nc  "computer"       (cn' "computer")         
structure    = nc  "structure"      (cn' "structure")         
os           = commonIdeaWithDict "os"      (cn' "operating system")    "OS"   [compScience]


dataStruct, dataStruct', dataType, dataType', 
  inDatum, outDatum, inParam, inVar, inValue, inQty,
  computerLiteracy, computerApp :: NamedChunk

dataStruct       = compoundNCPlPh datum structure
dataStruct'      = compoundNCPlPl datum structure
dataType         = compoundNCPlPh datum type_
dataType'        = compoundNCPlPl datum type_
inDatum          = compoundNC input_ datum
outDatum         = compoundNC output_ datum
inParam          = compoundNC input_ parameter
inVar            = compoundNC input_ variable
inValue          = compoundNC input_ value
inQty            = compoundNC input_ quantity
computerLiteracy = compoundNC computer literacy
computerApp      = compoundNC computer application

-- data types
char, integer, nat, real, string :: ConceptChunk
char    = dcc "character" (cn' "character")      "A single number or digit"
integer = dcc "integer"   (cn' "integer")        "A number without a fractional component"
nat     = dcc "natural"   (cn' "natural number") "An integer greater than zero" 
real    = dcc "real"      (cn' "real number")    "A number in (neg inf, pos inf)"
string  = dcc "string"    (cn' "string")         "A finite sequence of characters"
