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

data TableSz = Table1d Int
             | Table2d Int Int
             deriving (Show,Eq)

getTableLength (Table name tys fields)
    | length tys == 2 = if all isField fields
                            then Table1d (length fields)
                            else unsupported " 1d table contains rows"
    | length tys == 3 = let r = map getElemRow fields
                         in if all (== (head r)) r
                               then Table2d (length fields) (head r)
                               else unsupported " row not of same size"
    | otherwise       = unsupported ""
         where unsupported r = error ("unsupported table " ++ name ++ " length " ++ show (length tys) ++ r)
               isAllFields = all isField

               getElemRow (TableRow f)
                    | all isField f = length f
                    | otherwise     = unsupported (" row contains rows")

               isField (TableField _) = True
               isField _              = False

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
        transform (mod, acc) t@(Table n tys tfields) =
            case tableSz of
                Table1d x   ->
                    let nmod = addDecl (mkF (doCamelCase n)) $ addDecl (mkTypesig (doCamelCase n)) mod
                     in (nmod, acc)
                Table2d x y ->
                    let nmod = addDecl (mkF (doCamelCase n)) $ addDecl (mkTypesig (doCamelCase n)) mod
                     in (nmod, acc)
            where fieldsToNumb :: TableElem -> String
                  fieldsToNumb (TableField "__") = replicate 1 (toEnum 0xff)
                  fieldsToNumb (TableField s)    = case M.lookup s acc of
                                           Just i  -> replicate 1 (toEnum i)
                                           Nothing -> replicate 1 (toEnum (read s :: Int))
                  fieldsToNumb (TableRow e) = concatMap fieldsToNumb e
            
                  tableSz = getTableLength t

                  mkTypesig n = TypeSig dummyLoc [Ident n] $
                                            case tys of
                                                 [a,b]   -> TyFun (mkTyCon a) (mkTyCon b)
                                                 [a,b,c] -> TyFun (mkTyCon a) (TyFun (mkTyCon b) (mkTyCon c))
                                                 _       -> error "unsupported length"
                  tdata = concat $ map (fieldsToNumb) tfields
                  tableLit = Lit $ PrimString tdata

                  patConstructorMatch constructor var = PParen (PApp (UnQual (Ident constructor)) [PVar (Ident var)])

                  mkF fname = mkFunBindWithBinds fname patMatchs
                                    Nothing
                                    body
                                    [ mkPatBindBang "table" tableLit ]
                        where patMatchs = case tableSz of
                                               Table1d _   -> [ patConstructorMatch "W8#" "w" ]
                                               Table2d _ _ -> [ patConstructorMatch "W8#" "w1", patConstructorMatch "W8#" "w2" ]
                  body = App (Con (UnQual (Ident retConstructor))) 
                             (Paren (App 
                                         (App (Var (UnQual (Ident indexFct))) (Var (UnQual (Ident "table"))))
                                         (Paren (App (Var (UnQual (Ident "word2Int#"))) (indexer)))
                             ))
                  indexFct = maybe (error ("unknown type: " ++ show indexer)) snd $ lookup (head tys) convertors
                  retConstructor = maybe (error ("unknown type: " ++ show (last tys))) fst $ lookup (last tys) convertors
                  indexer = case tableSz of
                                Table1d _   -> (Var $ UnQual (Ident "w"))
                                Table2d x _ -> Paren (App (App (Var (UnQual (Ident "plusWord#")))
                                                                (Paren (App (App (Var (UnQual (Ident "timesWord#")))
                                                                                 (Var (UnQual (Ident "w1")))
                                                                            )
                                                                            (Paren (App (Var (UnQual (Ident "int2Word#"))) (Lit (PrimInt (fromIntegral x)))))
                                                                ))
                                                           )
                                                           (Var (UnQual (Ident "w2")))
                                                     )

main = do
    args <- getArgs
    case args of
        [x] -> readDSL x >>= process
