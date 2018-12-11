module Example.HelloWorld (helloWorld) where

import New (Class, Method, Body, Block, Statement, Declaration, Value, StateType,
  Function, StateVar, IOType, IOSt, Scope, UnaryOp, BinaryOp, Keyword, Label, Library, VarDecl, 
  FunctionDecl,
  RenderSym(..), KeywordSym(..), ClassSym(..), MethodSym(..), 
  BodySym(..), Symantics(..), StateTypeSym(..), StatementSym(..), IOTypeSym(..),
  IOStSym(..), UnaryOpSym(..), BinaryOpSym(..), ValueSym(..), Selector(..), FunctionSym(..))
import NewLanguageRenderer (makeCode, createCodeFiles)
import LanguageRenderer.NewJavaRenderer (JavaCode(..))
import Text.PrettyPrint.HughesPJ (Doc)
import System.Directory (setCurrentDirectory, createDirectoryIfMissing, getCurrentDirectory)
import Prelude hiding (return,print,log,exp,sin,cos,tan)

main :: IO()
main = do
  workingDir <- getCurrentDirectory
  createDirectoryIfMissing False "java"
  setCurrentDirectory "java"
  genCode [unJC helloWorld] ["HelloWorld"] [".java"]
  setCurrentDirectory workingDir
    
genCode :: [Doc] -> [Label] -> [Label] -> IO()
genCode files names exts = createCodeFiles $ makeCode files names exts

helloWorld :: (RenderSym repr) => repr Doc
helloWorld = fileDoc (
  block [
    printStrLn "Hello, world",
    printLn (string) (litString " too"),
    printStr "boo",
    print (bool) litTrue,
    printLn (float) defaultFloat,
    print (int) (litInt 0),
    print (char) (litChar 'c'),
    printLn (bool) ((?!) litTrue),
    printLn (int) ((#~) (litInt 1)),
    printLn (float) ((#/^) (litFloat 4.0)),
    printLn (int) ((#|) (litInt (-4))),
    printLn (float) (log ((#~) (litFloat 2.0))),
    printLn (float) (ln (litFloat 2.0)),
    printLn (float) (exp (litFloat 2.0)),
    printLn (float) (sin (litFloat 2.0)),
    printLn (float) (cos (litFloat 2.0)),
    printLn (float) (tan (litFloat 2.0)),
    printLn (float) (tan (litFloat 2.0)),
    printLn (bool) (litTrue ?&& litFalse),
    printLn (bool) (litTrue ?|| litFalse),
    printLn (bool) (litTrue ?&& ((?!) litFalse)),
    printLn (bool) ((?!) (litTrue ?&& litTrue)),
    printLn (int) ((litInt 6) #+ (litInt 2)),
    printLn (int) ((litInt 6) #- (litInt 2)),
    printLn (int) ((litInt 6) #* (litInt 2)),
    printLn (int) ((litInt 6) #/ (litInt 2)),
    printLn (int) ((litInt 6) #% (litInt 4)),
    printLn (int) ((litInt 6) #^ (litInt 2)),
    printLn (int) ((litInt 6) #+ ((litInt 2) #* (litInt 3))),
    printLn (float) (csc (litFloat 1.0)),
    printLn (float) (sec (litFloat 1.0)),
    printLn (float) (cot (litFloat 1.0)),
    printLn (int) (notNull (litInt 5)),
    printLn (int) (notNull (var "a")),
    printLn (int) (var "a"),
    printLn (int) (arg 5),
    printLn (int) (extVar "Lib" "var"),
    printLn (int) (self),
    printLn (int) (objVarSelf "thisOne"),
    printLn (int) (objVar (var "outer") (var "inner"))])