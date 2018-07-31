module Drasil.DocumentLanguage.TraceabilityMatrix where

import Language.Drasil
import Drasil.DocumentLanguage.RefHelpers (ModelDB, ddRefDB, gdRefDB, imRefDB, 
    modelsFromDB, refDD, refGD, refIM, refTM, tmRefDB)

modelTraceTable :: ModelDB -> LabelledContent
modelTraceTable mdb = llcc (mkLabelRA'' "RefAdd") $ Table
  ( EmptyS : crossListItems ) [[{-data (rows)-}]] (S "Title") True {-<-- showLabel?-}
  where crossListItems = getRefs refTM tmDB ++ getRefs refGD gdDB ++
                         getRefs refDD ddDB ++ getRefs refIM imDB
        getRefs f db = map (f db) (modelsFromDB db)
        tmDB = tmRefDB mdb
        gdDB = gdRefDB mdb
        ddDB = ddRefDB mdb
        imDB = imRefDB mdb