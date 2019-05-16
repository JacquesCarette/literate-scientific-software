{-# LANGUAGE TypeFamilies #-}

module LanguageRenderer.NewCppRenderer (
    -- * C++ Code Configuration -- defines syntax of all C++ code
    CppSrcCode(..), CppHdrCode(..)
) where

import New (Label,
    PackageSym(..), RenderSym(..), KeywordSym(..), PermanenceSym(..),
    BodySym(..), BlockSym(..), ControlBlockSym(..), StateTypeSym(..),
    UnaryOpSym(..), BinaryOpSym(..), ValueSym(..), NumericExpression(..), 
    BooleanExpression(..), ValueExpression(..), Selector(..), FunctionSym(..), 
    SelectorFunction(..), StatementSym(..), ControlStatementSym(..), ScopeSym(..),
    MethodTypeSym(..), ParameterSym(..), MethodSym(..), StateVarSym(..), 
    ClassSym(..), ModuleSym(..))
import NewLanguageRenderer (Terminator(..), fileDoc',
    multiStateDocD, blockDocD, bodyDocD, intTypeDocD, charTypeDocD, 
    stringTypeDocD, typeDocD, listTypeDocD, voidDocD, constructDocD, 
    stateParamDocD, paramListDocD, methodListDocD, stateVarDocD, 
    stateVarListDocD, alwaysDel, ifCondDocD, switchDocD, forDocD, whileDocD, 
    stratDocD, assignDocD, plusEqualsDocD, plusPlusDocD, varDecDocD, 
    varDecDefDocD, objDecDefDocD, constDecDefDocD, statementDocD,
    returnDocD, commentDocD, freeDocD, notOpDocD, negateOpDocD, sqrtOpDocD, 
    absOpDocD, expOpDocD, sinOpDocD, cosOpDocD, tanOpDocD, asinOpDocD, 
    acosOpDocD, atanOpDocD, unOpDocD, equalOpDocD, notEqualOpDocD, 
    greaterOpDocD, greaterEqualOpDocD, lessOpDocD, lessEqualOpDocD, plusOpDocD, 
    minusOpDocD, multOpDocD, divideOpDocD, moduloOpDocD, powerOpDocD, andOpDocD,
    orOpDocD, binOpDocD, binOpDocD', litTrueD, litFalseD, litCharD, litFloatD, 
    litIntD, litStringD, defaultCharD, defaultFloatD, defaultIntD, 
    defaultStringD, varDocD, selfDocD, argDocD, objVarDocD, inlineIfDocD, 
    funcAppDocD, funcDocD, castDocD, objAccessDocD, castObjDocD, breakDocD, 
    continueDocD, staticDocD, dynamicDocD, privateDocD, publicDocD, dot, 
    observerListName, doubleSlash, addCommentsDocD, callFuncParamList, 
    getterName, setterName, setEmpty)
import Helpers (angles, blank, doubleQuotedText, oneTab, tripFst, tripSnd, 
    tripThird, vibcat, liftA4, liftA5, liftA6, liftList, lift1List,
    liftPair, lift3Pair, lift4Pair, liftPairFst, liftTripFst)

import Prelude hiding (break,print,(<>),sin,cos,tan,floor)
import qualified Data.Map as Map (fromList,lookup)
import Control.Applicative (Applicative, liftA2, liftA3)
import Text.PrettyPrint.HughesPJ (Doc, text, (<>), (<+>), braces, parens, comma,
  empty, equals, integer, semi, vcat, lbrace, rbrace, quotes, render, colon, 
  render)

-----------------
-- Source File --
-----------------

newtype CppSrcCode a = CPPSC {unCPPSC :: a}

instance Functor CppSrcCode where
    fmap f (CPPSC x) = CPPSC (f x)

instance Applicative CppSrcCode where
    pure = CPPSC
    (CPPSC f) <*> (CPPSC x) = CPPSC (f x)

instance Monad CppSrcCode where
    return = CPPSC
    CPPSC x >>= f = f x

instance PackageSym CppSrcCode where
    type Package CppSrcCode = ([(Doc, Label, Bool)], Label)
    packMods n ms = liftPairFst (sequence ms, n)

instance RenderSym CppSrcCode where
    type RenderFile CppSrcCode = (Doc, Label, Bool)
    fileDoc code = liftTripFst (liftA3 fileDoc' (top code) (fmap tripFst code) bottom, tripSnd $ unCPPSC code, tripThird $ unCPPSC code)
    top m = liftA3 cppstop m (list dynamic) endStatement
    bottom = return empty

instance KeywordSym CppSrcCode where
    type Keyword CppSrcCode = Doc
    endStatement = return semi
    endStatementLoop = return empty

    include n = return $ text "#include" <+> doubleQuotedText n
    inherit = return colon

    list _ = return $ text "vector"
    listObj = return empty

    blockStart = return lbrace
    blockEnd = return rbrace

    ifBodyStart = blockStart
    elseIf = return $ text "else if"
    
    iterForEachLabel = return empty
    iterInLabel = return empty

    commentStart = return doubleSlash
    
    printFunc = return $ text "std::cout"
    printLnFunc = return $ text "std::cout"
    printFileFunc = fmap fst -- is this right?
    printFileLnFunc = fmap fst

instance PermanenceSym CppSrcCode where
    type Permanence CppSrcCode = Doc
    static = return staticDocD
    dynamic = return dynamicDocD

instance BodySym CppSrcCode where
    type Body CppSrcCode = Doc
    body = liftList bodyDocD
    bodyStatements = block
    oneLiner s = bodyStatements [s]

    addComments s = liftA2 (addCommentsDocD s) commentStart

instance BlockSym CppSrcCode where
    type Block CppSrcCode = Doc
    block sts = (lift1List blockDocD endStatement (map (fmap fst) (map state sts)))

instance StateTypeSym CppSrcCode where
    type StateType CppSrcCode = Doc
    bool = return $ cppBoolTypeDoc
    int = return $ intTypeDocD
    float = return $ cppFloatTypeDoc
    char = return $ charTypeDocD
    string = return $ stringTypeDocD
    infile = return $ cppInfileTypeDoc
    outfile = return $ cppOutfileTypeDoc
    listType p st = liftA2 listTypeDocD st (list p)
    intListType p = listType p int
    floatListType p = listType p float
    boolListType = listType dynamic bool
    obj t = return $ typeDocD t
    enumType t = return $ typeDocD t
    iterator t = fmap cppIterTypeDoc (listType dynamic t)

-- ControlBlockSym Not translated yet
instance ControlBlockSym CppSrcCode where
    runStrategy l strats rv av = 
        case Map.lookup l (Map.fromList strats) of Nothing -> error $ "Strategy '" ++ l ++ "': RunStrategy called on non-existent strategy."
                                                   Just b  -> liftA2 stratDocD b (state resultState)
        where resultState = case av of Nothing    -> return (empty, Empty)
                                       Just vari  -> case rv of Nothing  -> error $ "Strategy '" ++ l ++ "': Attempt to assign null return to a Value."
                                                                Just res -> assign vari res

    listSlice t vnew vold b e s = 
        let l_temp = "temp"
            v_temp = var l_temp
            l_i = "i_temp"
            v_i = var l_i
        in
        block [
            (listDec l_temp 0 t),
            for (varDecDef l_i int (getB b)) (v_i ?< getE e) (getS s v_i)
                (oneLiner $ valState $ v_temp $. (listAppend (vold $. (listAccess v_i)))),
            (vnew &= v_temp)]
        where getB Nothing = litInt 0
              getB (Just n) = n
              getE Nothing = vold $. listSize
              getE (Just n) = n
              getS Nothing v = (&++) v
              getS (Just n) v = v &+= n

instance UnaryOpSym CppSrcCode where
    type UnaryOp CppSrcCode = Doc
    notOp = return $ notOpDocD
    negateOp = return $ negateOpDocD
    sqrtOp = return $ sqrtOpDocD
    absOp = return $ absOpDocD
    logOp = return $ text "log10"
    lnOp = return $ text "log"
    expOp = return $ expOpDocD
    sinOp = return $ sinOpDocD
    cosOp = return $ cosOpDocD
    tanOp = return $ tanOpDocD
    asinOp = return $ asinOpDocD
    acosOp = return $ acosOpDocD
    atanOp = return $ atanOpDocD
    floorOp = return $ text "floor"
    ceilOp = return $ text "ceil"

instance BinaryOpSym CppSrcCode where
    type BinaryOp CppSrcCode = Doc
    equalOp = return $ equalOpDocD
    notEqualOp = return $ notEqualOpDocD
    greaterOp = return $ greaterOpDocD
    greaterEqualOp = return $ greaterEqualOpDocD
    lessOp = return $ lessOpDocD
    lessEqualOp = return $ lessEqualOpDocD
    plusOp = return $ plusOpDocD
    minusOp = return $ minusOpDocD
    multOp = return $ multOpDocD
    divideOp = return $ divideOpDocD
    powerOp = return $ powerOpDocD
    moduloOp = return $ moduloOpDocD
    andOp = return $ andOpDocD
    orOp = return $ orOpDocD

instance ValueSym CppSrcCode where
    type Value CppSrcCode = (Doc, Maybe String)
    litTrue = return $ (litTrueD, Just "true")
    litFalse = return $ (litFalseD, Just "false")
    litChar c = return $ (litCharD c, Just $ "\'" ++ [c] ++ "\'")
    litFloat v = return $ (litFloatD v, Just $ show v)
    litInt v = return $ (litIntD v, Just $ show v)
    litString s = return $ (litStringD s, Just $ "\"" ++ s ++ "\"")

    defaultChar = return $ (defaultCharD, Just "space character")
    defaultFloat = return $ (defaultFloatD, Just "0.0")
    defaultInt = return $ (defaultIntD, Just "0")
    defaultString = return $ (defaultStringD, Just "empty string")
    defaultBool = litFalse

    ($->) = objVar
    ($:) = enumElement

    const = var
    var n = return $ (varDocD n, Just n)
    extVar _ = var
    self = return $ (selfDocD, Just "this")
    arg n = liftPairFst (liftA2 argDocD (litInt (n+1)) argsList, Nothing)
    enumElement _ e = return $ (text e, Just e)
    enumVar = var
    objVar o v = liftPairFst (liftA2 objVarDocD o v, Just $ valName o ++ "." ++ valName v)
    objVarSelf = var
    listVar n _ = var n
    n `listOf` t = listVar n t
    iterVar l = return $ (text $ "(*" ++ l ++ ")", Nothing)
    
    inputFunc = return $ (text "std::cin", Nothing)
    argsList = return $ (text "argv", Nothing)

    valName (CPPSC (v, s)) = case s of Nothing -> error $ "Attempt to print unprintable Value (" ++ render v ++ ")"
                                       Just valstr -> valstr

instance NumericExpression CppSrcCode where
    (#~) v = liftPairFst (liftA2 unOpDocD negateOp v, Nothing)
    (#/^) v = liftPairFst (liftA2 unOpDocD sqrtOp v, Nothing)
    (#|) v = liftPairFst (liftA2 unOpDocD absOp v, Nothing)
    (#+) v1 v2 = liftPairFst (liftA3 binOpDocD plusOp v1 v2, Nothing)
    (#-) v1 v2 = liftPairFst (liftA3 binOpDocD minusOp v1 v2, Nothing)
    (#*) v1 v2 = liftPairFst (liftA3 binOpDocD multOp v1 v2, Nothing)
    (#/) v1 v2 = liftPairFst (liftA3 binOpDocD divideOp v1 v2, Nothing)
    (#%) v1 v2 = liftPairFst (liftA3 binOpDocD moduloOp v1 v2, Nothing)
    (#^) v1 v2 = liftPairFst (liftA3 binOpDocD' powerOp v1 v2, Nothing)

    log v = liftPairFst (liftA2 unOpDocD logOp v, Nothing)
    ln v = liftPairFst (liftA2 unOpDocD lnOp v, Nothing)
    exp v = liftPairFst (liftA2 unOpDocD expOp v, Nothing)
    sin v = liftPairFst (liftA2 unOpDocD sinOp v, Nothing)
    cos v = liftPairFst (liftA2 unOpDocD cosOp v, Nothing)
    tan v = liftPairFst (liftA2 unOpDocD tanOp v, Nothing)
    csc v = (litFloat 1.0) #/ (sin v)
    sec v = (litFloat 1.0) #/ (cos v)
    cot v = (litFloat 1.0) #/ (tan v)
    arcsin v = liftPairFst (liftA2 unOpDocD asinOp v, Nothing)
    arccos v = liftPairFst (liftA2 unOpDocD acosOp v, Nothing)
    arctan v = liftPairFst (liftA2 unOpDocD atanOp v, Nothing)
    floor v = liftPairFst (liftA2 unOpDocD floorOp v, Nothing)
    ceil v = liftPairFst (liftA2 unOpDocD ceilOp v, Nothing)

instance BooleanExpression CppSrcCode where
    (?!) v = liftPairFst (liftA2 unOpDocD notOp v, Nothing)
    (?&&) v1 v2 = liftPairFst (liftA3 binOpDocD andOp v1 v2, Nothing)
    (?||) v1 v2 = liftPairFst (liftA3 binOpDocD orOp v1 v2, Nothing)

    (?<) v1 v2 = liftPairFst (liftA3 binOpDocD lessOp v1 v2, Nothing)
    (?<=) v1 v2 = liftPairFst (liftA3 binOpDocD lessEqualOp v1 v2, Nothing)
    (?>) v1 v2 = liftPairFst (liftA3 binOpDocD greaterOp v1 v2, Nothing)
    (?>=) v1 v2 = liftPairFst (liftA3 binOpDocD greaterEqualOp v1 v2, Nothing)
    (?==) v1 v2 = liftPairFst (liftA3 binOpDocD equalOp v1 v2, Nothing)
    (?!=) v1 v2 = liftPairFst (liftA3 binOpDocD notEqualOp v1 v2, Nothing)
   
instance ValueExpression CppSrcCode where
    inlineIf b v1 v2 = liftPairFst (liftA3 inlineIfDocD b v1 v2, Nothing)
    funcApp n vs = liftPairFst (liftList (funcAppDocD n) vs, Nothing)
    selfFuncApp = funcApp
    extFuncApp _ = funcApp
    stateObj t vs = liftPairFst (liftA2 cppStateObjDoc t (liftList callFuncParamList vs), Nothing)
    extStateObj _ = stateObj
    listStateObj = stateObj

    exists = notNull
    notNull v = v

instance Selector CppSrcCode where
    objAccess v f = liftPairFst (liftA2 objAccessDocD v f, Nothing)
    ($.) = objAccess

    objMethodCall o f ps = objAccess o (func f ps)
    objMethodCallVoid o f = objMethodCall o f []

    selfAccess = objAccess self

    listPopulateAccess _ _ = return (empty, Nothing)
    listSizeAccess v = objAccess v listSize

    listIndexExists v i = listSizeAccess v ?> i
    argExists i = objAccess argsList (listAccess (litInt $ fromIntegral i))
    
    indexOf l v = funcApp "find" [l $. iterBegin, l $. iterEnd, v] #- l $. iterBegin

    stringEqual v1 v2 = v1 ?== v2

    castObj f v = liftPairFst (liftA2 castObjDocD f v, Nothing)
    castStrToFloat v = funcApp "std::stod" [v]

instance FunctionSym CppSrcCode where
    type Function CppSrcCode = Doc
    func l vs = fmap funcDocD (funcApp l vs)
    cast targT _ = fmap castDocD targT
    castListToInt = cast (listType static int) int
    get n = fmap funcDocD (funcApp (getterName n) [])
    set n v = fmap funcDocD (funcApp (setterName n) [v])

    listSize = func "size" []
    listAdd _ v = fmap funcDocD (funcApp "push_back" [v])
    listPopulateInt _ = return empty
    listPopulateFloat _ = return empty
    listPopulateChar _ = return empty
    listPopulateBool _ = return empty
    listPopulateString _ = return empty
    listAppend v = fmap funcDocD (funcApp "push_back" [v])
    listExtendInt = listAppend defaultInt 
    listExtendFloat = listAppend defaultFloat 
    listExtendChar = listAppend defaultChar 
    listExtendBool = listAppend defaultBool
    listExtendString = listAppend defaultString
    listExtendList _ = fmap cppListExtendList

    iterBegin = fmap funcDocD (funcApp "begin" [])
    iterEnd = fmap funcDocD (funcApp "end" [])

instance SelectorFunction CppSrcCode where
    listAccess v = fmap funcDocD (funcApp "at" [v])
    listSet = liftA2 cppListSetDoc

    listAccessEnum t v = listAccess (castObj (cast int t) v)
    listSetEnum t i = listSet (castObj (cast int t) i)

    at l = listAccess (var l) -- parameter should be an Integer?

instance StatementSym CppSrcCode where
    type Statement CppSrcCode = (Doc, Terminator)
    assign v1 v2 = liftPairFst (liftA2 assignDocD v1 v2, Semi)
    assignToListIndex lst index v = valState $ lst $. listSet index v
    (&=) = assign
    (&.=) l = assign (var l)
    (&=.) v l = assign v (var l)
    (&-=) v1 v2 = v1 &= (v1 #- v2)
    (&.-=) l v = l &.= (var l #- v)
    (&+=) v1 v2 = liftPairFst (liftA2 plusEqualsDocD v1 v2, Semi)
    (&.+=) l v = (var l) &+= v
    (&++) v = liftPairFst (fmap plusPlusDocD v, Semi)
    (&.++) l = (&++) (var l)
    (&~-) v = v &= (v #- (litInt 1))
    (&.~-) l = (&~-) (var l)

    varDec l t = liftPairFst (fmap (varDecDocD l) t, Semi)
    varDecDef l t v = liftPairFst (liftA2 (varDecDefDocD l) t v, Semi)
    listDec l n t = liftPairFst (liftA2 (cppListDecDoc l) (litInt n) t, Semi) -- this means that the type you declare must already be a list. Not sure how I feel about this. On the bright side, it also means you don't need to pass permanence
    listDecDef l t vs = liftPairFst (liftA2 (cppListDecDefDoc l) t (liftList callFuncParamList vs), Semi)
    objDecDef l t v = liftPairFst (liftA2 (objDecDefDocD l) t v, Semi)
    objDecNew l t vs = liftPairFst (liftA2 (objDecDefDocD l) t (stateObj t vs), Semi)
    extObjDecNew l _ = objDecNew l
    objDecNewVoid l t = liftPairFst (liftA2 (objDecDefDocD l) t (stateObj t []), Semi)
    extObjDecNewVoid l _ = objDecNewVoid l
    constDecDef l t v = liftPairFst (liftA2 (constDecDefDocD l) t v, Semi)

    print _ v = liftPairFst (liftA2 (cppPrintDocD False) printFunc v, Semi)
    printLn _ v = liftPairFst (liftA2 (cppPrintDocD True) printLnFunc v, Semi)
    printStr s = liftPairFst (liftA2 (cppPrintDocD False) printFunc (litString s), Semi)
    printStrLn s = liftPairFst (liftA2 (cppPrintDocD True) printLnFunc (litString s), Semi)

    -- All below still needs to be translated
    printFile f _ v = liftPairFst (liftA2 (cppPrintDocD False) (printFileFunc f) v, Semi)
    printFileLn f _ v = liftPairFst (liftA2 (cppPrintDocD True) (printFileLnFunc f) v, Semi)
    printFileStr f s = liftPairFst (liftA2 (cppPrintDocD False) (printFileFunc f) (litString s), Semi)
    printFileStrLn f s = liftPairFst (liftA2 (cppPrintDocD True) (printFileLnFunc f) (litString s), Semi)

    -- Keep this block as is for now, I think this should work. Consult CppRenderer.hs if not
    printList t v = multi [(state (printStr "[")), (for (varDecDef "i" int (litInt 0)) ((var "i") ?< ((v $. listSize) #- (litInt 1))) ((&.++) "i") (bodyStatements [print t (v $. (listAccess (var "i"))), printStr ","])), (state (print t (v $. (listAccess ((v $. listSize) #- (litInt 1)))))), (printStr "]")]
    printLnList t v = multi [(state (printStr "[")), (for (varDecDef "i" int (litInt 0)) ((var "i") ?< ((v $. listSize) #- (litInt 1))) ((&.++) "i") (bodyStatements [print t (v $. (listAccess (var "i"))), printStr ","])), (state (print t (v $. (listAccess ((v $. listSize) #- (litInt 1)))))), (printStrLn "]")]
    printFileList f t v = multi [(state (printFileStr f "[")), (for (varDecDef "i" int (litInt 0)) ((var "i") ?< ((v $. listSize) #- (litInt 1))) ((&.++) "i") (bodyStatements [printFile f t (v $. (listAccess (var "i"))), printFileStr f ","])), (state (printFile f t (v $. (listAccess ((v $. listSize) #- (litInt 1)))))), (printFileStr f "]")]
    printFileLnList f t v = multi [(state (printFileStr f "[")), (for (varDecDef "i" int (litInt 0)) ((var "i") ?< ((v $. listSize) #- (litInt 1))) ((&.++) "i") (bodyStatements [printFile f t (v $. (listAccess (var "i"))), printFileStr f ","])), (state (printFile f t (v $. (listAccess ((v $. listSize) #- (litInt 1)))))), (printFileStrLn f "]")]

    getIntInput v = liftPairFst (liftA3 cppInput v inputFunc endStatement, Semi)
    getFloatInput v = liftPairFst (liftA3 cppInput v inputFunc endStatement, Semi)
    getBoolInput v = liftPairFst (liftA3 cppInput v inputFunc endStatement, Semi)
    getStringInput v = liftPairFst (liftA3 cppInput v inputFunc endStatement, Semi)
    getCharInput v = liftPairFst (liftA3 cppInput v inputFunc endStatement, Semi)
    discardInput = liftPairFst (fmap (cppDiscardInput " ") inputFunc, Semi)

    getIntFileInput f v = liftPairFst (liftA3 cppInput v f endStatement, Semi)
    getFloatFileInput f v = liftPairFst (liftA3 cppInput v f endStatement, Semi)
    getBoolFileInput f v = liftPairFst (liftA3 cppInput v f endStatement, Semi)
    getStringFileInput f v = liftPairFst (liftA3 cppInput v f endStatement, Semi)
    getCharFileInput f v = liftPairFst (liftA3 cppInput v f endStatement, Semi)
    discardFileInput f = liftPairFst (fmap (cppDiscardInput " ") f, Semi)

    openFileR f n = liftPairFst (liftA2 (cppOpenFile "std::fstream::in") f n, Semi)
    openFileW f n = liftPairFst (liftA2 (cppOpenFile "std::fstream::out") f n, Semi)
    openFileA f n = liftPairFst (liftA2 (cppOpenFile "std::fstream::app") f n, Semi)
    closeFile f = valState $ objMethodCall f "close" []

    getFileInputLine f v = valState $ funcApp "std::getLine" [f, v]
    discardFileLine f = liftPairFst (fmap (cppDiscardInput "\\n") f, Semi)
    stringSplit d vnew s = let l_ss = "ss"
                               v_ss = var l_ss
                               l_word = "word"
                               v_word = var l_word
                           in
        multi [
            valState $ vnew $. (func "clear" []),
            varDec l_ss (obj "std::stringstream"),
            valState $ objMethodCall v_ss "str" [s],
            varDec l_word string,
            while (funcApp "std::getline" [v_ss, v_word, litChar d]) (oneLiner $ valState $ vnew $. (listAppend v_word))
        ]

    break = return (breakDocD, Semi)
    continue = return (continueDocD, Semi)

    returnState v = liftPairFst (fmap returnDocD v, Semi)
    returnVar l = liftPairFst (fmap returnDocD (var l), Semi)

    valState v = liftPairFst (fmap fst v, Semi)

    comment cmt = liftPairFst (fmap (commentDocD cmt) commentStart, Empty)

    free v = liftPairFst (fmap freeDocD v, Semi)

    throw errMsg = liftPairFst (fmap cppThrowDoc (litString errMsg), Semi)

    initState fsmName initialState = varDecDef fsmName string (litString initialState)
    changeState fsmName toState = fsmName &.= (litString toState)

    initObserverList = listDecDef observerListName
    addObserver t o = valState $ obsList $. listAdd lastelem o
        where obsList = observerListName `listOf` t
              lastelem = obsList $. listSize

    state = fmap statementDocD
    loopState = fmap (statementDocD . setEmpty)
    multi = lift1List multiStateDocD endStatement

instance ControlStatementSym CppSrcCode where
    ifCond bs b = liftPairFst (lift4Pair ifCondDocD ifBodyStart elseIf blockEnd b bs, Empty)
    ifNoElse bs = ifCond bs $ body []
    switch v cs c = liftPairFst (lift3Pair switchDocD (state break) v c cs, Empty)
    switchAsIf v cs = ifCond cases
        where cases = map (\(l, b) -> (v ?== l, b)) cs

    ifExists _ ifBody _ = liftPairFst (ifBody, Empty) -- All variables are initialized in C++

    for sInit vGuard sUpdate b = liftPairFst (liftA6 forDocD blockStart blockEnd (loopState sInit) vGuard (loopState sUpdate) b, Empty)
    forRange i initv finalv stepv = for (varDecDef i int initv) ((var i) ?< finalv) (i &.+= stepv)
    forEach l t v = for (varDecDef l (iterator t) (v $. iterBegin)) (var l ?!= v $. iterEnd) ((&.++) l)
    while v b = liftPairFst (liftA4 whileDocD blockStart blockEnd v b, Empty)

    tryCatch tb cb = liftPairFst (liftA2 cppTryCatch tb cb, Empty)

    checkState l = switch (var l) 

    notifyObservers fn t ps = for initv (var index ?< (obsList $. listSize)) ((&.++) index) notify
        where obsList = observerListName `listOf` t
              index = "observerIndex"
              initv = varDecDef index int $ litInt 0
              notify = oneLiner $ valState $ (obsList $. at index) $. func fn ps

    getFileInputAll f v = let l_line = "nextLine"
                              v_line = var l_line
                          in
        multi [varDec l_line string,
            while ((?!) (funcApp "std::getline" [f, v_line]))
                (oneLiner $ valState $ v $. (listAppend $ v_line))]

instance ScopeSym CppSrcCode where
    type Scope CppSrcCode = Doc
    private = return privateDocD
    public = return publicDocD

    includeScope _ = return empty

instance MethodTypeSym CppSrcCode where
    type MethodType CppSrcCode = Doc
    mState t = t
    void = return voidDocD
    construct n = return $ constructDocD n

instance ParameterSym CppSrcCode where
    type Parameter CppSrcCode = Doc
    stateParam n = fmap (stateParamDocD n)
    pointerParam n = fmap (cppPointerParamDoc n)

instance MethodSym CppSrcCode where
    -- Bool is True if the method is a main method, False otherwise
    type Method CppSrcCode = (Doc, Bool)
    method n _ _ t ps b = liftPairFst (liftA5 (cppMethod n) t (liftList paramListDocD ps) b blockStart blockEnd, False)
    getMethod n t = method (getterName n) public dynamic t [] getBody
        where getBody = oneLiner $ returnState (self $-> (var n))
    setMethod setLbl paramLbl t = method (setterName setLbl) public dynamic void [(stateParam paramLbl t)] setBody
        where setBody = oneLiner $ (self $-> (var setLbl)) &=. paramLbl
    mainMethod b = liftPairFst (liftA4 cppMainMethod int b blockStart blockEnd, True)
    privMethod n = method n private dynamic
    pubMethod n = method n public dynamic
    constructor n = method n public dynamic (construct n)
    destructor n vs = 
        let i = "i"
            deleteStatements = map (fmap snd) vs
            loopIndexDec = varDec i int
            dbody = bodyStatements $ loopIndexDec : deleteStatements
        in pubMethod ('~':n) void [] dbody

    function = method

instance StateVarSym CppSrcCode where
    -- (Doc, Bool) is the corresponding destructor code for the stateVar
    type StateVar CppSrcCode = (Doc, (Doc, Terminator))
    stateVar del l s p t = liftPair (liftA4 (stateVarDocD l) (includeScope s) p t endStatement, if del < alwaysDel then return (empty, Empty) else free $ var l)
    privMVar del l = stateVar del l private dynamic
    pubMVar del l = stateVar del l public dynamic
    pubGVar del l = stateVar del l public static
    listStateVar del l s p t = 
        let i = "i"
            guard = var i ?< (var l $. listSize)
            loopBody = oneLiner $ free (var l $. at i)
            initv = (i &.= litInt 0)
            deleteLoop = for initv guard ((&.++) i) loopBody
        in liftPair (fmap fst $ stateVar del l s p t, if del < alwaysDel then return (empty, Empty) else deleteLoop)

instance ClassSym CppSrcCode where
    -- Bool is True if the class is a main class, False otherwise
    type Class CppSrcCode = (Doc, Bool)
    buildClass n _ _ vs fs = liftPairFst (liftList methodListDocD (fs ++ [destructor n vs]), any (snd . unCPPSC) fs)
    enum _ _ _ = return (empty, False)
    mainClass _ vs fs = liftPairFst (liftA2 (cppMainClass (null vs)) (liftList stateVarListDocD (map (fmap fst) vs)) (liftList methodListDocD fs), True)
    privClass n p = buildClass n p private
    pubClass n p = buildClass n p public

instance ModuleSym CppSrcCode where
    -- Label is module name
    -- Bool is True if the method is a main method, False otherwise
    type Module CppSrcCode = (Doc, Label, Bool)
    buildModule n l _ ms cs = liftTripFst (liftA3 cppModuleDoc (liftList vcat (map include l)) (liftList methodListDocD ms) (liftList vibcat (map (fmap fst) cs)), n, any (snd . unCPPSC) cs || any (snd . unCPPSC) ms)

-----------------
-- Header File --
-----------------

newtype CppHdrCode a = CPPHC {unCPPHC :: a}

instance Functor CppHdrCode where
    fmap f (CPPHC x) = CPPHC (f x)

instance Applicative CppHdrCode where
    pure = CPPHC
    (CPPHC f) <*> (CPPHC x) = CPPHC (f x)

instance Monad CppHdrCode where
    return = CPPHC
    CPPHC x >>= f = f x

instance PackageSym CppHdrCode where
    type Package CppHdrCode = ([(Doc, Label, Bool)], Label)
    packMods n ms = liftPairFst (sequence ms, n)

instance RenderSym CppHdrCode where
    type RenderFile CppHdrCode = (Doc, Label, Bool)
    fileDoc code = liftTripFst (liftA3 fileDoc' (top code) (fmap tripFst code) bottom, tripSnd $ unCPPHC code, tripThird $ unCPPHC code)
    top m = liftA3 cpphtop m (list dynamic) endStatement
    bottom = return $ text "#endif"

instance KeywordSym CppHdrCode where
    type Keyword CppHdrCode = Doc
    endStatement = return semi
    endStatementLoop = return empty

    include n = return $ text "#include" <+> doubleQuotedText n
    inherit = return colon

    list _ = return $ text "vector"
    listObj = return empty

    blockStart = return lbrace
    blockEnd = return rbrace

    ifBodyStart = blockStart
    elseIf = return $ text "else if"
    
    iterForEachLabel = return empty
    iterInLabel = return empty

    commentStart = return doubleSlash
    
    printFunc = return $ text "std::cout"
    printLnFunc = return $ text "std::cout"
    printFileFunc = fmap fst -- is this right?
    printFileLnFunc = fmap fst

instance PermanenceSym CppHdrCode where
    type Permanence CppHdrCode = Doc
    static = return staticDocD
    dynamic = return dynamicDocD

instance BodySym CppHdrCode where
    type Body CppHdrCode = Doc
    body = liftList bodyDocD
    bodyStatements = block
    oneLiner s = bodyStatements [s]

    addComments s = liftA2 (addCommentsDocD s) commentStart

instance BlockSym CppHdrCode where
    type Block CppHdrCode = Doc
    block sts = (lift1List blockDocD endStatement (map (fmap fst) (map state sts)))

instance StateTypeSym CppHdrCode where
    type StateType CppHdrCode = Doc
    bool = return $ cppBoolTypeDoc
    int = return $ intTypeDocD
    float = return $ cppFloatTypeDoc
    char = return $ charTypeDocD
    string = return $ stringTypeDocD
    infile = return $ cppInfileTypeDoc
    outfile = return $ cppOutfileTypeDoc
    listType p st = liftA2 listTypeDocD st (list p)
    intListType p = listType p int
    floatListType p = listType p float
    boolListType = listType dynamic bool
    obj t = return $ typeDocD t
    enumType t = return $ typeDocD t
    iterator t = fmap cppIterTypeDoc (listType dynamic t)

-- ControlBlockSym Not translated yet
instance ControlBlockSym CppHdrCode where
    runStrategy l strats rv av = 
        case Map.lookup l (Map.fromList strats) of Nothing -> error $ "Strategy '" ++ l ++ "': RunStrategy called on non-existent strategy."
                                                   Just b  -> liftA2 stratDocD b (state resultState)
        where resultState = case av of Nothing    -> return (empty, Empty)
                                       Just vari  -> case rv of Nothing  -> error $ "Strategy '" ++ l ++ "': Attempt to assign null return to a Value."
                                                                Just res -> assign vari res

    listSlice t vnew vold b e s = 
        let l_temp = "temp"
            v_temp = var l_temp
            l_i = "i_temp"
            v_i = var l_i
        in
        block [
            (listDec l_temp 0 t),
            for (varDecDef l_i int (getB b)) (v_i ?< getE e) (getS s v_i)
                (oneLiner $ valState $ v_temp $. (listAppend (vold $. (listAccess v_i)))),
            (vnew &= v_temp)]
        where getB Nothing = litInt 0
              getB (Just n) = n
              getE Nothing = vold $. listSize
              getE (Just n) = n
              getS Nothing v = (&++) v
              getS (Just n) v = v &+= n

instance UnaryOpSym CppHdrCode where
    type UnaryOp CppHdrCode = Doc
    notOp = return $ notOpDocD
    negateOp = return $ negateOpDocD
    sqrtOp = return $ sqrtOpDocD
    absOp = return $ absOpDocD
    logOp = return $ text "log10"
    lnOp = return $ text "log"
    expOp = return $ expOpDocD
    sinOp = return $ sinOpDocD
    cosOp = return $ cosOpDocD
    tanOp = return $ tanOpDocD
    asinOp = return $ asinOpDocD
    acosOp = return $ acosOpDocD
    atanOp = return $ atanOpDocD
    floorOp = return $ text "floor"
    ceilOp = return $ text "ceil"

instance BinaryOpSym CppHdrCode where
    type BinaryOp CppHdrCode = Doc
    equalOp = return $ equalOpDocD
    notEqualOp = return $ notEqualOpDocD
    greaterOp = return $ greaterOpDocD
    greaterEqualOp = return $ greaterEqualOpDocD
    lessOp = return $ lessOpDocD
    lessEqualOp = return $ lessEqualOpDocD
    plusOp = return $ plusOpDocD
    minusOp = return $ minusOpDocD
    multOp = return $ multOpDocD
    divideOp = return $ divideOpDocD
    powerOp = return $ powerOpDocD
    moduloOp = return $ moduloOpDocD
    andOp = return $ andOpDocD
    orOp = return $ orOpDocD

instance ValueSym CppHdrCode where
    type Value CppHdrCode = (Doc, Maybe String)
    litTrue = return $ (litTrueD, Just "true")
    litFalse = return $ (litFalseD, Just "false")
    litChar c = return $ (litCharD c, Just $ "\'" ++ [c] ++ "\'")
    litFloat v = return $ (litFloatD v, Just $ show v)
    litInt v = return $ (litIntD v, Just $ show v)
    litString s = return $ (litStringD s, Just $ "\"" ++ s ++ "\"")

    defaultChar = return $ (defaultCharD, Just "space character")
    defaultFloat = return $ (defaultFloatD, Just "0.0")
    defaultInt = return $ (defaultIntD, Just "0")
    defaultString = return $ (defaultStringD, Just "empty string")
    defaultBool = litFalse

    ($->) = objVar
    ($:) = enumElement

    const = var
    var n = return $ (varDocD n, Just n)
    extVar _ = var
    self = return $ (selfDocD, Just "this")
    arg n = liftPairFst (liftA2 argDocD (litInt (n+1)) argsList, Nothing)
    enumElement _ e = return $ (text e, Just e)
    enumVar = var
    objVar o v = liftPairFst (liftA2 objVarDocD o v, Just $ valName o ++ "." ++ valName v)
    objVarSelf = var
    listVar n _ = var n
    n `listOf` t = listVar n t
    iterVar l = return $ (text $ "(*" ++ l ++ ")", Nothing)
    
    inputFunc = return $ (text "std::cin", Nothing)
    argsList = return $ (text "argv", Nothing)

    valName (CPPHC (v, s)) = case s of Nothing -> error $ "Attempt to print unprintable Value (" ++ render v ++ ")"
                                       Just valstr -> valstr

instance NumericExpression CppHdrCode where
    (#~) v = liftPairFst (liftA2 unOpDocD negateOp v, Nothing)
    (#/^) v = liftPairFst (liftA2 unOpDocD sqrtOp v, Nothing)
    (#|) v = liftPairFst (liftA2 unOpDocD absOp v, Nothing)
    (#+) v1 v2 = liftPairFst (liftA3 binOpDocD plusOp v1 v2, Nothing)
    (#-) v1 v2 = liftPairFst (liftA3 binOpDocD minusOp v1 v2, Nothing)
    (#*) v1 v2 = liftPairFst (liftA3 binOpDocD multOp v1 v2, Nothing)
    (#/) v1 v2 = liftPairFst (liftA3 binOpDocD divideOp v1 v2, Nothing)
    (#%) v1 v2 = liftPairFst (liftA3 binOpDocD moduloOp v1 v2, Nothing)
    (#^) v1 v2 = liftPairFst (liftA3 binOpDocD' powerOp v1 v2, Nothing)

    log v = liftPairFst (liftA2 unOpDocD logOp v, Nothing)
    ln v = liftPairFst (liftA2 unOpDocD lnOp v, Nothing)
    exp v = liftPairFst (liftA2 unOpDocD expOp v, Nothing)
    sin v = liftPairFst (liftA2 unOpDocD sinOp v, Nothing)
    cos v = liftPairFst (liftA2 unOpDocD cosOp v, Nothing)
    tan v = liftPairFst (liftA2 unOpDocD tanOp v, Nothing)
    csc v = (litFloat 1.0) #/ (sin v)
    sec v = (litFloat 1.0) #/ (cos v)
    cot v = (litFloat 1.0) #/ (tan v)
    arcsin v = liftPairFst (liftA2 unOpDocD asinOp v, Nothing)
    arccos v = liftPairFst (liftA2 unOpDocD acosOp v, Nothing)
    arctan v = liftPairFst (liftA2 unOpDocD atanOp v, Nothing)
    floor v = liftPairFst (liftA2 unOpDocD floorOp v, Nothing)
    ceil v = liftPairFst (liftA2 unOpDocD ceilOp v, Nothing)

instance BooleanExpression CppHdrCode where
    (?!) v = liftPairFst (liftA2 unOpDocD notOp v, Nothing)
    (?&&) v1 v2 = liftPairFst (liftA3 binOpDocD andOp v1 v2, Nothing)
    (?||) v1 v2 = liftPairFst (liftA3 binOpDocD orOp v1 v2, Nothing)

    (?<) v1 v2 = liftPairFst (liftA3 binOpDocD lessOp v1 v2, Nothing)
    (?<=) v1 v2 = liftPairFst (liftA3 binOpDocD lessEqualOp v1 v2, Nothing)
    (?>) v1 v2 = liftPairFst (liftA3 binOpDocD greaterOp v1 v2, Nothing)
    (?>=) v1 v2 = liftPairFst (liftA3 binOpDocD greaterEqualOp v1 v2, Nothing)
    (?==) v1 v2 = liftPairFst (liftA3 binOpDocD equalOp v1 v2, Nothing)
    (?!=) v1 v2 = liftPairFst (liftA3 binOpDocD notEqualOp v1 v2, Nothing)
   
instance ValueExpression CppHdrCode where
    inlineIf b v1 v2 = liftPairFst (liftA3 inlineIfDocD b v1 v2, Nothing)
    funcApp n vs = liftPairFst (liftList (funcAppDocD n) vs, Nothing)
    selfFuncApp = funcApp
    extFuncApp _ = funcApp
    stateObj t vs = liftPairFst (liftA2 cppStateObjDoc t (liftList callFuncParamList vs), Nothing)
    extStateObj _ = stateObj
    listStateObj = stateObj

    exists = notNull
    notNull v = v

instance Selector CppHdrCode where
    objAccess v f = liftPairFst (liftA2 objAccessDocD v f, Nothing)
    ($.) = objAccess

    objMethodCall o f ps = objAccess o (func f ps)
    objMethodCallVoid o f = objMethodCall o f []

    selfAccess = objAccess self

    listPopulateAccess _ _ = return (empty, Nothing)
    listSizeAccess v = objAccess v listSize

    listIndexExists v i = listSizeAccess v ?> i
    argExists i = objAccess argsList (listAccess (litInt $ fromIntegral i))
    
    indexOf l v = funcApp "find" [l $. iterBegin, l $. iterEnd, v] #- l $. iterBegin

    stringEqual v1 v2 = v1 ?== v2

    castObj f v = liftPairFst (liftA2 castObjDocD f v, Nothing)
    castStrToFloat v = funcApp "std::stod" [v]

instance FunctionSym CppHdrCode where
    type Function CppHdrCode = Doc
    func l vs = fmap funcDocD (funcApp l vs)
    cast targT _ = fmap castDocD targT
    castListToInt = cast (listType static int) int
    get n = fmap funcDocD (funcApp (getterName n) [])
    set n v = fmap funcDocD (funcApp (setterName n) [v])

    listSize = func "size" []
    listAdd _ v = fmap funcDocD (funcApp "push_back" [v])
    listPopulateInt _ = return empty
    listPopulateFloat _ = return empty
    listPopulateChar _ = return empty
    listPopulateBool _ = return empty
    listPopulateString _ = return empty
    listAppend v = fmap funcDocD (funcApp "push_back" [v])
    listExtendInt = listAppend defaultInt 
    listExtendFloat = listAppend defaultFloat 
    listExtendChar = listAppend defaultChar 
    listExtendBool = listAppend defaultBool
    listExtendString = listAppend defaultString
    listExtendList _ = fmap cppListExtendList

    iterBegin = fmap funcDocD (funcApp "begin" [])
    iterEnd = fmap funcDocD (funcApp "end" [])

instance SelectorFunction CppHdrCode where
    listAccess v = fmap funcDocD (funcApp "at" [v])
    listSet = liftA2 cppListSetDoc

    listAccessEnum t v = listAccess (castObj (cast int t) v)
    listSetEnum t i = listSet (castObj (cast int t) i)

    at l = listAccess (var l) -- parameter should be an Integer?

instance StatementSym CppHdrCode where
    type Statement CppHdrCode = (Doc, Terminator)
    assign v1 v2 = liftPairFst (liftA2 assignDocD v1 v2, Semi)
    assignToListIndex lst index v = valState $ lst $. listSet index v
    (&=) = assign
    (&.=) l = assign (var l)
    (&=.) v l = assign v (var l)
    (&-=) v1 v2 = v1 &= (v1 #- v2)
    (&.-=) l v = l &.= (var l #- v)
    (&+=) v1 v2 = liftPairFst (liftA2 plusEqualsDocD v1 v2, Semi)
    (&.+=) l v = (var l) &+= v
    (&++) v = liftPairFst (fmap plusPlusDocD v, Semi)
    (&.++) l = (&++) (var l)
    (&~-) v = v &= (v #- (litInt 1))
    (&.~-) l = (&~-) (var l)

    varDec l t = liftPairFst (fmap (varDecDocD l) t, Semi)
    varDecDef l t v = liftPairFst (liftA2 (varDecDefDocD l) t v, Semi)
    listDec l n t = liftPairFst (liftA2 (cppListDecDoc l) (litInt n) t, Semi) -- this means that the type you declare must already be a list. Not sure how I feel about this. On the bright side, it also means you don't need to pass permanence
    listDecDef l t vs = liftPairFst (liftA2 (cppListDecDefDoc l) t (liftList callFuncParamList vs), Semi)
    objDecDef l t v = liftPairFst (liftA2 (objDecDefDocD l) t v, Semi)
    objDecNew l t vs = liftPairFst (liftA2 (objDecDefDocD l) t (stateObj t vs), Semi)
    extObjDecNew l _ = objDecNew l
    objDecNewVoid l t = liftPairFst (liftA2 (objDecDefDocD l) t (stateObj t []), Semi)
    extObjDecNewVoid l _ = objDecNewVoid l
    constDecDef l t v = liftPairFst (liftA2 (constDecDefDocD l) t v, Semi)

    print _ v = liftPairFst (liftA2 (cppPrintDocD False) printFunc v, Semi)
    printLn _ v = liftPairFst (liftA2 (cppPrintDocD True) printLnFunc v, Semi)
    printStr s = liftPairFst (liftA2 (cppPrintDocD False) printFunc (litString s), Semi)
    printStrLn s = liftPairFst (liftA2 (cppPrintDocD True) printLnFunc (litString s), Semi)

    -- All below still needs to be translated
    printFile f _ v = liftPairFst (liftA2 (cppPrintDocD False) (printFileFunc f) v, Semi)
    printFileLn f _ v = liftPairFst (liftA2 (cppPrintDocD True) (printFileLnFunc f) v, Semi)
    printFileStr f s = liftPairFst (liftA2 (cppPrintDocD False) (printFileFunc f) (litString s), Semi)
    printFileStrLn f s = liftPairFst (liftA2 (cppPrintDocD True) (printFileLnFunc f) (litString s), Semi)

    -- Keep this block as is for now, I think this should work. Consult CppRenderer.hs if not
    printList t v = multi [(state (printStr "[")), (for (varDecDef "i" int (litInt 0)) ((var "i") ?< ((v $. listSize) #- (litInt 1))) ((&.++) "i") (bodyStatements [print t (v $. (listAccess (var "i"))), printStr ","])), (state (print t (v $. (listAccess ((v $. listSize) #- (litInt 1)))))), (printStr "]")]
    printLnList t v = multi [(state (printStr "[")), (for (varDecDef "i" int (litInt 0)) ((var "i") ?< ((v $. listSize) #- (litInt 1))) ((&.++) "i") (bodyStatements [print t (v $. (listAccess (var "i"))), printStr ","])), (state (print t (v $. (listAccess ((v $. listSize) #- (litInt 1)))))), (printStrLn "]")]
    printFileList f t v = multi [(state (printFileStr f "[")), (for (varDecDef "i" int (litInt 0)) ((var "i") ?< ((v $. listSize) #- (litInt 1))) ((&.++) "i") (bodyStatements [printFile f t (v $. (listAccess (var "i"))), printFileStr f ","])), (state (printFile f t (v $. (listAccess ((v $. listSize) #- (litInt 1)))))), (printFileStr f "]")]
    printFileLnList f t v = multi [(state (printFileStr f "[")), (for (varDecDef "i" int (litInt 0)) ((var "i") ?< ((v $. listSize) #- (litInt 1))) ((&.++) "i") (bodyStatements [printFile f t (v $. (listAccess (var "i"))), printFileStr f ","])), (state (printFile f t (v $. (listAccess ((v $. listSize) #- (litInt 1)))))), (printFileStrLn f "]")]

    getIntInput v = liftPairFst (liftA3 cppInput v inputFunc endStatement, Semi)
    getFloatInput v = liftPairFst (liftA3 cppInput v inputFunc endStatement, Semi)
    getBoolInput v = liftPairFst (liftA3 cppInput v inputFunc endStatement, Semi)
    getStringInput v = liftPairFst (liftA3 cppInput v inputFunc endStatement, Semi)
    getCharInput v = liftPairFst (liftA3 cppInput v inputFunc endStatement, Semi)
    discardInput = liftPairFst (fmap (cppDiscardInput " ") inputFunc, Semi)

    getIntFileInput f v = liftPairFst (liftA3 cppInput v f endStatement, Semi)
    getFloatFileInput f v = liftPairFst (liftA3 cppInput v f endStatement, Semi)
    getBoolFileInput f v = liftPairFst (liftA3 cppInput v f endStatement, Semi)
    getStringFileInput f v = liftPairFst (liftA3 cppInput v f endStatement, Semi)
    getCharFileInput f v = liftPairFst (liftA3 cppInput v f endStatement, Semi)
    discardFileInput f = liftPairFst (fmap (cppDiscardInput " ") f, Semi)

    openFileR f n = liftPairFst (liftA2 (cppOpenFile "std::fstream::in") f n, Semi)
    openFileW f n = liftPairFst (liftA2 (cppOpenFile "std::fstream::out") f n, Semi)
    openFileA f n = liftPairFst (liftA2 (cppOpenFile "std::fstream::app") f n, Semi)
    closeFile f = valState $ objMethodCall f "close" []

    getFileInputLine f v = valState $ funcApp "std::getLine" [f, v]
    discardFileLine f = liftPairFst (fmap (cppDiscardInput "\\n") f, Semi)
    stringSplit d vnew s = let l_ss = "ss"
                               v_ss = var l_ss
                               l_word = "word"
                               v_word = var l_word
                           in
        multi [
            valState $ vnew $. (func "clear" []),
            varDec l_ss (obj "std::stringstream"),
            valState $ objMethodCall v_ss "str" [s],
            varDec l_word string,
            while (funcApp "std::getline" [v_ss, v_word, litChar d]) (oneLiner $ valState $ vnew $. (listAppend v_word))
        ]

    break = return (breakDocD, Semi)
    continue = return (continueDocD, Semi)

    returnState v = liftPairFst (fmap returnDocD v, Semi)
    returnVar l = liftPairFst (fmap returnDocD (var l), Semi)

    valState v = liftPairFst (fmap fst v, Semi)

    comment cmt = liftPairFst (fmap (commentDocD cmt) commentStart, Empty)

    free v = liftPairFst (fmap freeDocD v, Semi)

    throw errMsg = liftPairFst (fmap cppThrowDoc (litString errMsg), Semi)

    initState fsmName initialState = varDecDef fsmName string (litString initialState)
    changeState fsmName toState = fsmName &.= (litString toState)

    initObserverList = listDecDef observerListName
    addObserver t o = valState $ obsList $. listAdd lastelem o
        where obsList = observerListName `listOf` t
              lastelem = obsList $. listSize

    state = fmap statementDocD
    loopState = fmap (statementDocD . setEmpty)
    multi = lift1List multiStateDocD endStatement

instance ControlStatementSym CppHdrCode where
    ifCond bs b = liftPairFst (lift4Pair ifCondDocD ifBodyStart elseIf blockEnd b bs, Empty)
    ifNoElse bs = ifCond bs $ body []
    switch v cs c = liftPairFst (lift3Pair switchDocD (state break) v c cs, Empty)
    switchAsIf v cs = ifCond cases
        where cases = map (\(l, b) -> (v ?== l, b)) cs

    ifExists _ ifBody _ = liftPairFst (ifBody, Empty) -- All variables are initialized in C++

    for sInit vGuard sUpdate b = liftPairFst (liftA6 forDocD blockStart blockEnd (loopState sInit) vGuard (loopState sUpdate) b, Empty)
    forRange i initv finalv stepv = for (varDecDef i int initv) ((var i) ?< finalv) (i &.+= stepv)
    forEach l t v = for (varDecDef l (iterator t) (v $. iterBegin)) (var l ?!= v $. iterEnd) ((&.++) l)
    while v b = liftPairFst (liftA4 whileDocD blockStart blockEnd v b, Empty)

    tryCatch tb cb = liftPairFst (liftA2 cppTryCatch tb cb, Empty)

    checkState l = switch (var l) 

    notifyObservers fn t ps = for initv (var index ?< (obsList $. listSize)) ((&.++) index) notify
        where obsList = observerListName `listOf` t
              index = "observerIndex"
              initv = varDecDef index int $ litInt 0
              notify = oneLiner $ valState $ (obsList $. at index) $. func fn ps

    getFileInputAll f v = let l_line = "nextLine"
                              v_line = var l_line
                          in
        multi [varDec l_line string,
            while ((?!) (funcApp "std::getline" [f, v_line]))
                (oneLiner $ valState $ v $. (listAppend $ v_line))]

instance ScopeSym CppHdrCode where
    type Scope CppHdrCode = Doc
    private = return privateDocD
    public = return publicDocD

    includeScope _ = return empty

instance MethodTypeSym CppHdrCode where
    type MethodType CppHdrCode = Doc
    mState t = t
    void = return voidDocD
    construct n = return $ constructDocD n

instance ParameterSym CppHdrCode where
    type Parameter CppHdrCode = Doc
    stateParam n = fmap (stateParamDocD n)
    pointerParam n = fmap (cppPointerParamDoc n)

instance MethodSym CppHdrCode where
    -- Bool is True if the method is a main method, False otherwise
    type Method CppHdrCode = (Doc, Bool)
    method n _ _ t ps b = liftPairFst (liftA5 (cppMethod n) t (liftList paramListDocD ps) b blockStart blockEnd, False)
    getMethod n t = method (getterName n) public dynamic t [] getBody
        where getBody = oneLiner $ returnState (self $-> (var n))
    setMethod setLbl paramLbl t = method (setterName setLbl) public dynamic void [(stateParam paramLbl t)] setBody
        where setBody = oneLiner $ (self $-> (var setLbl)) &=. paramLbl
    mainMethod b = liftPairFst (liftA4 cppMainMethod int b blockStart blockEnd, True)
    privMethod n = method n private dynamic
    pubMethod n = method n public dynamic
    constructor n = method n public dynamic (construct n)
    destructor n vs = 
        let i = "i"
            deleteStatements = map (fmap snd) vs
            loopIndexDec = varDec i int
            dbody = bodyStatements $ loopIndexDec : deleteStatements
        in pubMethod ('~':n) void [] dbody

    function = method

instance StateVarSym CppHdrCode where
    -- (Doc, Bool) is the corresponding destructor code for the stateVar
    type StateVar CppHdrCode = (Doc, (Doc, Terminator))
    stateVar del l s p t = liftPair (liftA4 (stateVarDocD l) (includeScope s) p t endStatement, if del < alwaysDel then return (empty, Empty) else free $ var l)
    privMVar del l = stateVar del l private dynamic
    pubMVar del l = stateVar del l public dynamic
    pubGVar del l = stateVar del l public static
    listStateVar del l s p t = 
        let i = "i"
            guard = var i ?< (var l $. listSize)
            loopBody = oneLiner $ free (var l $. at i)
            initv = (i &.= litInt 0)
            deleteLoop = for initv guard ((&.++) i) loopBody
        in liftPair (fmap fst $ stateVar del l s p t, if del < alwaysDel then return (empty, Empty) else deleteLoop)

instance ClassSym CppHdrCode where
    -- Bool is True if the class is a main class, False otherwise
    type Class CppHdrCode = (Doc, Bool)
    buildClass n _ _ vs fs = liftPairFst (liftList methodListDocD (fs ++ [destructor n vs]), any (snd . unCPPHC) fs)
    enum _ _ _ = return (empty, False)
    mainClass _ vs fs = liftPairFst (liftA2 (cppMainClass (null vs)) (liftList stateVarListDocD (map (fmap fst) vs)) (liftList methodListDocD fs), True)
    privClass n p = buildClass n p private
    pubClass n p = buildClass n p public

instance ModuleSym CppHdrCode where
    -- Label is module name
    -- Bool is True if the method is a main method, False otherwise
    type Module CppHdrCode = (Doc, Label, Bool)
    buildModule n l _ ms cs = liftTripFst (liftA3 cppModuleDoc (liftList vcat (map include l)) (liftList methodListDocD ms) (liftList vibcat (map (fmap fst) cs)), n, any (snd . unCPPHC) cs || any (snd . unCPPHC) ms)
    -- Need to add libraries here

-- helpers
isDtor :: Label -> Bool
isDtor ('~':_) = True
isDtor _ = False

-- convenience
cppHeaderExt :: Label
cppHeaderExt = ".hpp"

cppstop :: (Doc, Label, Bool) -> Doc -> Doc -> Doc
cppstop (_, n, b) lst end = vcat [
    if b then empty else inc <+> doubleQuotedText (n ++ cppHeaderExt),
    blank,
    inc <+> angles (text "algorithm"),
    inc <+> angles (text "iostream"),
    inc <+> angles (text "fstream"),
    inc <+> angles (text "iterator"),
    inc <+> angles (text "string"),
    inc <+> angles (text "math.h"),
    inc <+> angles (text "sstream"),
    inc <+> angles (text "limits"),
    inc <+> angles lst,
    blank,
    usingNameSpace "std" (Just "string") end,
    usingNameSpace "std" (Just $ render lst) end,
    usingNameSpace "std" (Just "ifstream") end,
    usingNameSpace "std" (Just "ofstream") end]
    where inc = text "#include"

cpphtop :: (Doc, Label, Bool) -> Doc -> Doc -> Doc
cpphtop (_, n, _) lst end = vcat [
    text "#ifndef" <+> text n <> text "_h",
    text "#define" <+> text n <> text "_h",
    blank,
    inc <+> angles (text "string"),
    inc <+> angles lst,
    blank,
    usingNameSpace "std" (Just "string") end,
    usingNameSpace "std" (Just $ render lst) end,
    usingNameSpace "std" (Just "ifstream") end,
    usingNameSpace "std" (Just "ofstream") end]
    where inc = text "#include"

usingNameSpace :: Label -> Maybe Label -> Doc -> Doc
usingNameSpace n (Just m) end = text "using" <+> text n <> colon <> colon <> text m <> end
usingNameSpace n Nothing end = text "using namespace" <+> text n <> end

cppBoolTypeDoc :: Doc
cppBoolTypeDoc = text "bool"

cppFloatTypeDoc :: Doc
cppFloatTypeDoc = text "double"

cppInfileTypeDoc :: Doc
cppInfileTypeDoc = text "ifstream"

cppOutfileTypeDoc :: Doc
cppOutfileTypeDoc = text "ofstream"

cppIterTypeDoc :: Doc -> Doc
cppIterTypeDoc t = text "std::" <> t <> text "::iterator"

cppStateObjDoc :: Doc -> Doc -> Doc
cppStateObjDoc t ps = t <> parens ps

cppListSetDoc :: (Doc, Maybe String) -> (Doc, Maybe String) -> Doc
cppListSetDoc (i, _) (v, _) = dot <> text "at" <> parens i <+> equals <+> v

cppListDecDoc :: Label -> (Doc, Maybe String) -> Doc -> Doc
cppListDecDoc l (n, _) t = t <+> text l <> parens n

cppListDecDefDoc :: Label -> Doc -> Doc -> Doc
cppListDecDefDoc l t vs = t <+> text l <> braces vs

cppPrintDocD :: Bool -> Doc -> (Doc, Maybe String) -> Doc
cppPrintDocD newLn printFn (v, _) = printFn <+> text "<<" <+> v <+> end
    where end = if newLn then text "<<" <+> text "std::endl" else empty

cppThrowDoc :: (Doc, Maybe String) -> Doc
cppThrowDoc (errMsg, _) = text "throw" <> parens errMsg

cppTryCatch :: Doc -> Doc -> Doc
cppTryCatch tb cb= vcat [
    text "try" <+> lbrace,
    oneTab $ tb,
    rbrace <+> text "catch" <+> parens (text "...") <+> lbrace,
    oneTab $ cb,
    rbrace]

cppDiscardInput :: Label -> (Doc, Maybe String) -> Doc
cppDiscardInput sep (inFn, _) = inFn <> dot <> text "ignore" <+> parens 
    (text "std::numeric_limits<std::streamsize>::max()" <> comma <+> quotes (text sep))

cppInput :: (Doc, Maybe String) -> (Doc, Maybe String) -> Doc -> Doc
cppInput (v, _) (inFn, _) end = vcat [
    inFn <+> text ">>" <+> v <> end,
    inFn <> dot <> text "ignore (std::numeric_limits<std::streamsize>::max(), '\\n')"]

cppOpenFile :: Label -> (Doc, Maybe String) -> (Doc, Maybe String) -> Doc
cppOpenFile mode (f, _) (n, _) = f <> dot <> text "open" <> parens (n <> comma <+> text mode)

cppListExtendList :: Doc -> Doc
cppListExtendList t = dot <> text "push_back" <> parens (t <> parens (integer 0))

cppPointerParamDoc :: Label -> Doc -> Doc
cppPointerParamDoc n t = t <+> text "&" <> text n

cppMethod :: Label -> Doc -> Doc -> Doc -> Doc -> Doc -> Doc
cppMethod n t ps b bStart bEnd = vcat [ttype <+> text n <> parens ps <+> bStart,
    oneTab b,
    bEnd]
    where ttype | isDtor n = empty
                | otherwise = t

cppMainMethod :: Doc -> Doc -> Doc -> Doc -> Doc
cppMainMethod t b bStart bEnd = vcat [
    t <+> text "main" <> parens (text "int argc, const char *argv[]") <+> bStart,
    oneTab b,
    blank,
    text "return 0;",
    bEnd]

cppMainClass :: Bool -> Doc -> Doc -> Doc
cppMainClass b vs fs = vcat [
    vs,
    if b then empty else blank,
    fs]

cppModuleDoc :: Doc -> Doc -> Doc -> Doc
cppModuleDoc ls fs cs = vcat [
    ls,
    blank,
    cs,
    blank,
    fs]