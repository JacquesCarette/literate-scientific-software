{-# LANGUAGE PostfixOperators #-}

module Test.HelloWorld (helloWorld) where

import GOOL.Drasil (GSProgram, MSBody, MSBlock, MSStatement, SMethod, OOProg,
  ProgramSym(..), FileSym(..), BodySym(..), bodyStatements, oneLiner, 
  BlockSym(..), listSlice, TypeSym(..), StatementSym(..), AssignStatement(..), (&=), 
  DeclStatement(..), IOStatement(..), StringStatement(..), CommentStatement(..), ControlStatement(..), 
  VariableSym(..), listVar, Literal(..), VariableValue(..), CommandLineArgs(..), NumericExpression(..), BooleanExpression(..), Comparison(..),
  ValueExpression(..), extFuncApp, List(..),
  MethodSym(..), ModuleSym(..))
import Prelude hiding (return,print,log,exp,sin,cos,tan,const)
import Test.Helper (helper)

helloWorld :: (OOProg r) => GSProgram r
helloWorld = prog "HelloWorld" [docMod description 
  ["Brooks MacLachlan"] "" $ fileDoc (buildModule "HelloWorld" [] 
  [helloWorldMain] []), helper]

description :: String
description = "Tests various GOOL functions. It should run without errors."

helloWorldMain :: (OOProg r) => SMethod r
helloWorldMain = mainFunction (body [ helloInitVariables, 
    helloListSlice,
    block [ifCond [(valueOf (var "b" int) ?>= litInt 6, bodyStatements [varDecDef (var "dummy" string) (litString "dummy")]),
      (valueOf (var "b" int) ?== litInt 5, helloIfBody)] helloElseBody, helloIfExists,
    helloSwitch, helloForLoop, helloWhileLoop, helloForEachLoop, helloTryCatch]])

helloInitVariables :: (OOProg r) => MSBlock r
helloInitVariables = block [comment "Initializing variables",
  varDec $ var "a" int, 
  varDecDef (var "b" int) (litInt 5),
  listDecDef (var "myOtherList" (listType double)) [litDouble 1.0, 
    litDouble 1.5],
  varDecDef (var "oneIndex" int) (indexOf (valueOf $ var "myOtherList" 
    (listType double)) (litDouble 1.0)),
  printLn (valueOf $ var "oneIndex" int),
  var "a" int &= listSize (valueOf $ var "myOtherList" (listType double)),
  valStmt (listAdd (valueOf $ var "myOtherList" (listType double))
    (litInt 2) (litDouble 2.0)),
  valStmt (listAppend (valueOf $ var "myOtherList" (listType double)) 
    (litDouble 2.5)),
  varDec $ var "e" double,
  var "e" int &= listAccess (valueOf $ var "myOtherList"
    (listType double)) (litInt 1),
  valStmt (listSet (valueOf $ var "myOtherList" (listType double)) 
    (litInt 1) (litDouble 17.4)),
  listDec 7 (var "myName" (listType string)),
  stringSplit ' ' (var "myName" (listType string)) (litString "Brooks Mac"),
  printLn (valueOf $ var "myName" (listType string)),
  listDecDef (var "boringList" (listType bool)) 
    [litFalse, litFalse, litFalse, litFalse, litFalse],
  printLn (valueOf $ var "boringList" (listType bool)),
  listDec 2 $ var "mySlicedList" (listType double)]

helloListSlice :: (OOProg r) => MSBlock r
helloListSlice = listSlice (var "mySlicedList" (listType double)) 
  (valueOf $ var "myOtherList" (listType double)) (Just (litInt 1)) 
  (Just (litInt 3)) Nothing

helloIfBody :: (OOProg r) => MSBody r
helloIfBody = addComments "If body" (body [
  block [
    varDec $ var "c" int,
    varDec $ var "d" int,
    assign (var "a" int) (litInt 5),
    var "b" int &= (valueOf (var "a" int) #+ litInt 2),
    var "c" int &= (valueOf (var "b" int) #+ litInt 3),
    var "d" int &= valueOf (var "b" int),
    var "d" int &-= valueOf (var "a" int),
    var "c" int &-= valueOf (var "d" int),
    var "b" int &+= litInt 17,
    var "c" int &+= litInt 17,
    (&++) (var "a" int),
    (&++) (var "d" int),
    (&--) (var "c" int),
    (&--) (var "b" int),

    listDec 5 (var "myList" (listType int)),
    objDecDef (var "myObj" char) (litChar 'o'),
    constDecDef (const "myConst" string) (litString "Imconstant"),

    printLn (valueOf $ var "a" int),
    printLn (valueOf $ var "b" int),
    printLn (valueOf $ var "c" int),
    printLn (valueOf $ var "d" int),
    printLn (valueOf $ var "myOtherList" (listType double)),
    printLn (valueOf $ var "mySlicedList" (listType double)),
    
    printStrLn "Type an int",
    getInput (var "d" int),
    printStrLn "Type another",
    discardInput],
  
  block [
    printLn (litString " too"),
    printStr "boo",
    print litTrue,
    print (litInt 0),
    print (litChar 'c'),
    printLn (litTrue ?!),
    printLn (litInt 1 #~),
    printLn (litDouble 4.0 #/^),
    printLn (litInt (-4) #|),
    printLn (log (litDouble 2.0)),
    multi [printLn (ln (litDouble 2.0)),
    printLn (exp (litDouble 2.0 #~)),
    printLn (sin (litDouble 2.0)),
    printLn (cos (litDouble 2.0)),
    printLn (tan (litDouble 2.0))],
    printLn (tan (litDouble 2.0)),
    printLn (litTrue ?&& litFalse),
    printLn (litTrue ?|| litFalse),
    printLn (litTrue ?&& (litFalse ?!)),
    printLn ((litTrue ?&& litTrue) ?!),
    printLn (litInt 6 #+ litInt 2),
    printLn (litInt 6 #- litInt 2),
    printLn (litInt 6 #* litInt 2),
    printLn (litInt 6 #/ litInt 2),
    printLn (litInt 6 #% litInt 4),
    printLn (litInt 6 #^ litInt 2),
    printLn (litInt 6 #+ (litInt 2 #* litInt 3)),
    printLn (csc (litDouble 1.0)),
    printLn (sec (litDouble 1.0)),
    printLn (valueOf $ var "a" int),
    printLn (inlineIf litTrue (litInt 5) (litInt 0)),
    printLn (cot (litDouble 1.0))]])

helloElseBody :: (OOProg r) => MSBody r
helloElseBody = bodyStatements [printLn (arg 5)]

helloIfExists :: (OOProg r) => MSStatement r
helloIfExists = ifExists (valueOf $ var "boringList" (listType bool)) 
  (oneLiner (printStrLn "Ew, boring list!")) (oneLiner (printStrLn "Great, no bores!"))

helloSwitch :: (OOProg r) => MSStatement r
helloSwitch = switch (valueOf $ var "a" int) [(litInt 5, oneLiner (var "b" int &= litInt 10)), 
  (litInt 0, oneLiner (var "b" int &= litInt 5))]
  (oneLiner (var "b" int &= litInt 0))

helloForLoop :: (OOProg r) => MSStatement r
helloForLoop = forRange i (litInt 0) (litInt 9) (litInt 1) (oneLiner (printLn 
  (valueOf i)))
  where i = var "i" int

helloWhileLoop :: (OOProg r) => MSStatement r
helloWhileLoop = while (valueOf (var "a" int) ?< litInt 13) (bodyStatements 
  [printStrLn "Hello", (&++) (var "a" int)]) 

helloForEachLoop :: (OOProg r) => MSStatement r
helloForEachLoop = forEach i (valueOf $ listVar "myOtherList" double) 
  (oneLiner (printLn (extFuncApp "Helper" "doubleAndAdd" double [valueOf i, 
  litDouble 1.0])))
  where i = var "num" double

helloTryCatch :: (OOProg r) => MSStatement r
helloTryCatch = tryCatch (oneLiner (throw "Good-bye!"))
  (oneLiner (printStrLn "Caught intentional error"))
