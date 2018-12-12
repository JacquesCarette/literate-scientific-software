{-# LANGUAGE TypeFamilies #-}

module New (
    -- Types
    Class, Method, Body, Block, Conditional, Statement, Declaration, Value, StateType,
    Function, StateVar, IOType, IOSt, Scope, UnaryOp, BinaryOp, Permanence, 
    Label, Library, VarDecl, FunctionDecl, 
    -- Typeclasses
    RenderSym(..), KeywordSym(..), PermanenceSym(..), ClassSym(..), MethodSym(..), 
    BodySym(..), ConditionalSym(..), BlockSym(..), StateTypeSym(..), StatementSym(..), IOTypeSym(..),
    IOStSym(..), UnaryOpSym(..), BinaryOpSym(..), ValueSym(..), Selector(..), FunctionSym(..)
) where

import Text.PrettyPrint.HughesPJ (Doc)

type Class = Doc
type Method = Doc
type Body = Doc
type Block = Doc
type Conditional = Doc
type Statement = Doc
type Declaration = Doc
type Value = Doc
type StateType = Doc
type Function = Doc
type StateVar = Doc
type IOType = Doc
type IOSt = Doc
type Scope = Doc
type UnaryOp = Doc
type BinaryOp = Doc
type Permanence = Doc
-- type Keyword = Doc

type Label = String
type Library = String
type VarDecl = Declaration
type FunctionDecl = Method

class (ConditionalSym repr) => RenderSym repr where
    fileDoc :: repr Doc -> repr Doc
    top :: repr Block -- Block is a placeholder for all of these, should change
    codeBody :: repr Class -> repr Block
    bottom :: repr Block

class (ValueSym repr, PermanenceSym repr) => KeywordSym repr where
    type Keyword repr
    endStatement :: repr (Keyword repr)
    include :: repr (Keyword repr)
    list :: repr Permanence -> repr (Keyword repr)
    printFunc :: repr (Keyword repr)
    printLnFunc :: repr (Keyword repr)
    printFileFunc :: repr Value -> repr (Keyword repr)
    printFileLnFunc :: repr Value -> repr (Keyword repr)
    argsList :: repr (Keyword repr)
    listObj :: repr (Keyword repr)
    blockStart :: repr (Keyword repr)
    blockEnd :: repr (Keyword repr)
    ifBodyStart :: repr (Keyword repr)
    elseIf :: repr (Keyword repr)

class PermanenceSym repr where
    static :: repr Permanence
    dynamic :: repr Permanence

class ClassSym repr where
    buildClass :: Label -> Maybe Label -> repr Scope -> [repr StateVar] -> [repr Method] -> repr Class

class MethodSym repr where
    mainMethod :: repr Body -> repr Method

class (BlockSym repr) => BodySym repr where
    body :: [repr Block] -> repr Body
    bodyStatements :: [repr Statement] -> repr Body

-- Right now the Block is the top-level structure
class StatementSym repr => BlockSym repr where
    block   :: [repr Statement] -> repr Block

class StateTypeSym repr where
    bool   :: repr StateType
    int    :: repr StateType
    float  :: repr StateType
    char   :: repr StateType
    string :: repr StateType
    infile :: repr StateType
    outfile :: repr StateType
    listType :: repr Permanence -> repr StateType -> repr StateType
    intListType :: repr Permanence -> repr StateType
    intListType p = listType p int
    floatListType :: repr Permanence -> repr StateType
    floatListType p = listType p float
    obj :: Label -> repr StateType
    enumType :: Label -> repr StateType

class (BodySym repr) => ConditionalSym repr where
    ifCond :: [(repr Value, repr Body)] -> repr Body -> repr Conditional 
    switchCond :: [(repr Value, repr Body)] -> repr Body -> repr Conditional -- is there value in separating Literals into their own type?

class (PermanenceSym repr, StateTypeSym repr, ValueSym repr, IOStSym repr) => StatementSym repr where
    (&=)   :: repr Value -> repr Value -> repr Statement
    (&.=)  :: Label -> repr Value -> repr Statement
    (&=.)  :: repr Value -> Label -> repr Statement
    (&-=)  :: repr Value -> repr Value -> repr Statement
    (&.-=) :: Label -> repr Value -> repr Statement
    (&+=)  :: repr Value -> repr Value -> repr Statement
    (&.+=) :: Label -> repr Value -> repr Statement
    (&++)  :: repr Value -> repr Statement
    (&.++) :: Label -> repr Statement
    (&~-)  :: repr Value -> repr Statement
    (&.~-) :: Label -> repr Statement

    assign  :: repr Value -> repr Value -> repr Statement

    varDec  :: Label -> repr StateType -> repr Statement
    varDecDef :: Label -> repr StateType -> repr Value -> repr Statement
    listDec :: Label -> Integer -> repr StateType -> repr Statement
    listDecDef :: Label -> repr StateType -> [repr Value] -> repr Statement
    objDecDef :: Label -> repr StateType -> repr Value -> repr Statement
    constDecDef :: Label -> repr StateType -> repr Value -> repr Statement

    print      :: repr StateType -> repr Value -> repr Statement
    printLn    :: repr StateType -> repr Value -> repr Statement
    printStr   :: String -> repr Statement
    printStrLn :: String -> repr Statement

    print'      :: repr IOType -> repr StateType -> repr Value -> repr Statement
    printLn'    :: repr IOType -> repr StateType -> repr Value -> repr Statement
    printStr'   :: repr IOType -> String -> repr Statement
    printStrLn' :: repr IOType -> String -> repr Statement

    printFile      :: repr Value -> repr StateType -> repr Value -> repr Statement
    printFileLn    :: repr Value -> repr StateType -> repr Value -> repr Statement
    printFileStr   :: repr Value -> String -> repr Statement
    printFileStrLn :: repr Value -> String -> repr Statement

    getInput         :: repr StateType -> repr Value -> repr Statement
    getFileInput     :: repr Value -> repr StateType -> repr Value -> repr Statement
    discardFileInput :: repr Value -> repr Statement
    getFileInputLine :: repr Value -> repr Value -> repr Statement
    discardFileLine  :: repr Value -> repr Statement
    getFileInputAll  :: repr Value -> repr Value -> repr Statement

    openFileR :: repr Value -> repr Value -> repr Statement
    openFileW :: repr Value -> repr Value -> repr Statement
    closeFile :: repr Value -> repr Statement

    returnState    :: repr Value -> repr Statement
    returnVar :: Label -> repr Statement

    ioState :: repr IOSt -> repr Statement

    state :: repr Statement -> repr Statement

class ValueSym repr => IOTypeSym repr where
    console :: repr IOType
    file    :: repr Value -> repr IOType

class (KeywordSym repr, ValueSym repr) => IOStSym repr where
    out :: repr (Keyword repr) -> repr Value -> repr IOSt 

class UnaryOpSym repr where
    notOp :: repr UnaryOp
    negateOp :: repr UnaryOp
    sqrtOp :: repr UnaryOp
    absOp :: repr UnaryOp
    logOp :: repr UnaryOp
    lnOp :: repr UnaryOp
    expOp :: repr UnaryOp
    sinOp :: repr UnaryOp
    cosOp :: repr UnaryOp
    tanOp :: repr UnaryOp

class BinaryOpSym repr where
    equalOp :: repr BinaryOp
    notEqualOp :: repr BinaryOp
    greaterOp :: repr BinaryOp
    greaterEqualOp :: repr BinaryOp
    lessOp :: repr BinaryOp
    lessEqualOp :: repr BinaryOp
    plusOp :: repr BinaryOp
    minusOp :: repr BinaryOp
    multOp :: repr BinaryOp
    divideOp :: repr BinaryOp
    powerOp :: repr BinaryOp
    moduloOp :: repr BinaryOp
    andOp :: repr BinaryOp
    orOp :: repr BinaryOp

class (StateTypeSym repr) => ValueSym repr where
    litTrue   :: repr Value
    litFalse :: repr Value
    litChar   :: Char -> repr Value
    litFloat  :: Double -> repr Value
    litInt    :: Integer -> repr Value
    litString :: String -> repr Value

    defaultChar :: repr Value
    defaultFloat :: repr Value
    defaultInt :: repr Value
    defaultString :: repr Value

    (?!)  :: repr Value -> repr Value  -- where to specific infix?
    (?<)  :: repr Value -> repr Value -> repr Value
    (?<=) :: repr Value -> repr Value -> repr Value
    (?>)  :: repr Value -> repr Value -> repr Value
    (?>=) :: repr Value -> repr Value -> repr Value
    (?==) :: repr Value -> repr Value -> repr Value
    (?!=) :: repr Value -> repr Value -> repr Value
    (?&&) :: repr Value -> repr Value -> repr Value
    (?||) :: repr Value -> repr Value -> repr Value

    --arithmetic operators (#)
    (#~)  :: repr Value -> repr Value
    (#/^) :: repr Value -> repr Value
    (#|)  :: repr Value -> repr Value
    (#+)  :: repr Value -> repr Value -> repr Value
    (#-)  :: repr Value -> repr Value -> repr Value
    (#*)  :: repr Value -> repr Value -> repr Value
    (#/)  :: repr Value -> repr Value -> repr Value
    (#%)  :: repr Value -> repr Value -> repr Value
    (#^)  :: repr Value -> repr Value -> repr Value

     --other operators ($)
    ($->) :: repr Value -> repr Value -> repr Value
    ($:)  :: Label -> Label -> repr Value

    log :: repr Value -> repr Value
    ln :: repr Value -> repr Value
    exp :: repr Value -> repr Value
    sin :: repr Value -> repr Value
    cos :: repr Value -> repr Value
    tan :: repr Value -> repr Value
    csc :: repr Value -> repr Value
    sec :: repr Value -> repr Value
    cot :: repr Value -> repr Value

    const :: Label -> repr Value
    var :: Label -> repr Value
    extVar :: Library -> Label -> repr Value
--    global :: Label -> repr Value         -- not sure how this one works
    self :: repr Value
    arg :: Integer -> repr Value
    enumElement :: Label -> Label -> repr Value
    enumVar :: Label -> repr Value
    objVar :: repr Value -> repr Value -> repr Value
    objVarSelf :: Label -> repr Value
    listVar :: Label -> repr StateType -> repr Value
    inlineIf :: repr Value -> repr Value -> repr Value -> repr Value
    funcApp :: Label -> [repr Value] -> repr Value
    extFuncApp :: Library -> Label -> [repr Value] -> repr Value
    stateObj :: repr StateType -> [repr Value] -> repr Value
    listStateObj :: repr StateType -> [repr Value] -> repr Value

    exists :: repr Value -> repr Value
    notNull :: repr Value -> repr Value

class (FunctionSym repr, ValueSym repr) => Selector repr where
    ($.)  :: repr Value -> repr Function -> repr Value
    objAccess :: repr Value -> repr Function -> repr Value

class (ValueSym repr, StateTypeSym repr) => FunctionSym repr where
    func :: Label -> [repr Value] -> repr Function

    listSize   :: repr Function
    listAccess :: repr Value -> repr Function
    listAppend :: repr Value -> repr Function
    listExtend :: repr StateType -> repr Function