module Main where 
import Language.Haskell.Exts.Pretty (prettyPrint)
import Language.Haskell.Exts.Syntax
import System.Environment
import qualified Data.Map as M
import Text.Printf
import DSL
import Combinators
import Utils

convertors =
    [ ("Word8", ("W8#", "indexWord8OffAddr#"))
    , ("Word", ("W#", "indexWord8OffAddr#"))
    ]

process atoms = do
    let (mod, mapTypes) = foldl transform (iniModule, M.empty) atoms
    putStrLn $ prettyPrint mod
    where
        iniModule = addImport (mkImport "GHC.Prim")
                  $ addImport (mkImport "GHC.Word")
                  $ addImport (mkImport "GHC.Types")
                  $ addPragma (mkModulePragma "BangPatterns")
                  $ addPragma (mkModulePragma "MagicHash")
                  $ mkModule "Table"

        transform acc (Struct _ _) = acc
        transform (mod, acc) (Enum n fields)   = do
            (mod, fst $ foldl (addField) (acc,0) fields)
            where addField (acc,n) (fname, mval) =
                        let current = maybe n numberToInt mval
                            nacc = M.insert fname current acc
                         in (nacc, current+1)
        transform (mod, acc) (Table n indexer valueTy tfields) =
             let nmod = addDecl (mkF (doCamelCase n)) $ addDecl (mkTypesig (doCamelCase n)) mod
              in (nmod, acc)
            where fieldsToNumb "__" = toEnum 0xff
                  fieldsToNumb s    = case M.lookup s acc of
                                           Just i  -> toEnum i
                                           Nothing -> toEnum (read s :: Int)

                  mkTypesig n = TypeSig dummyLoc [Ident n] (TyFun (mkTyCon indexer) (mkTyCon valueTy))
                  tdata = concat $ map (replicate 1 . fieldsToNumb) tfields
                  tableLit = Lit $ PrimString tdata
                  mkF fname = mkFunBindWithBinds fname [ (PParen (PApp (UnQual (Ident "W8#")) [PVar (Ident "w")])) ]
                                    Nothing
                                    body
                                    [ mkPatBindBang "table" tableLit ]
                  body = App (Con (UnQual (Ident constructor))) 
                             (Paren (App 
                                         (App (Var (UnQual (Ident indexFct))) (Var (UnQual (Ident "table"))))
                                         (Paren (App (Var (UnQual (Ident "word2Int#"))) (Var (UnQual (Ident "w")))))
                             ))
                  indexFct = maybe (error ("unknown type: " ++ show indexer)) snd $ lookup indexer convertors
                  constructor = maybe (error ("unknown type: " ++ show valueTy)) fst $ lookup valueTy convertors

main = do
    args <- getArgs
    case args of
        [x] -> readDSL x >>= process
