module Combinators
	( addImport
	, addDecl
	, addPragma
	, mkModule
	, mkImport
	, dummyLoc
	, mkTyCon
	, setTyConQual
	, mkModulePragma
	, tyAugment
	, mkDataDecl
	, mkGenerator
	, mkInstDecl
	, mkPlus
	, mkTimes
	, mkLiteralInt
	, mkIdent
	, mkQuotedVarOp
	, mkQuotedVarOpSymbol
	, mkTestBit
	, mkLet
	, mkFunBindWithBinds
	, mkPatBind
	, mkPatBindBang
	, mkPatBindWithBDecls
	, peekPtrPlus
	, peekPtrPlusSig
	, module Language.Haskell.Exts.Syntax
	) where

import Language.Haskell.Exts.Syntax

addImport :: ImportDecl -> Module -> Module
addImport im (Module loc name pragmas wt exs initialIms decls) =
	Module loc name pragmas wt exs (updatesImports initialIms) decls
	where updatesImports ims
		| any (== im) ims = ims
		| otherwise       = ims ++ [im]

addPragma :: ModulePragma -> Module -> Module
addPragma pragma (Module a b initialPragmas d e f g) =
	Module a b (updatedPragmas initialPragmas) d e f g
	where updatedPragmas pragmas
		| any (== pragma) pragmas = pragmas
		| otherwise               = pragma : pragmas

addDecl :: Decl -> Module -> Module
addDecl decl (Module loc name pragmas wt exs ims decls) =
	Module loc name pragmas wt exs ims (decls ++ [decl])

mkModule :: String -> Module
mkModule name = Module dummyLoc (ModuleName name) [] Nothing Nothing [] []

mkImport :: String -> ImportDecl
mkImport name = ImportDecl dummyLoc (ModuleName name) False False Nothing Nothing Nothing

dummyLoc = SrcLoc "dummy" 0 0

mkTyCon n = TyCon $ UnQual $ Ident n

setTyConQual qu (TyCon (UnQual ident)) = TyCon (Qual (ModuleName qu) ident)
setTyConQual _ v = v

mkModulePragma s = LanguagePragma dummyLoc [Ident s]

tyAugment t1 t2 = TyApp (mkTyCon t1) t2

------------------------------------------------------------
-- following is not combinators but helpers to create
-- necessary bits.
------------------------------------------------------------

mkDataDecl name dataDef derives = DataDecl dummyLoc DataType [] (Ident name) [] dataDef derives

mkGenerator name rhs = Generator dummyLoc (PVar (Ident name)) rhs

mkInstDecl instName typeName defs = InstDecl dummyLoc [] (UnQual $ Ident instName) [mkTyCon typeName] defs

mkLiteralInt :: Integral a => a -> Exp
mkLiteralInt = Lit . Int . fromIntegral

mkPlus lhs rhs = InfixApp lhs (QVarOp $ UnQual $ Symbol "+") rhs
mkTimes lhs rhs = InfixApp lhs (QVarOp $ UnQual $ Symbol "*") rhs
mkIdent = Var . UnQual . Ident
mkQuotedVarOp = QVarOp . UnQual . Ident
mkQuotedVarOpSymbol = QVarOp . UnQual . Symbol

-- | create a testbit expression "name `testBit` n"
mkTestBit name n = InfixApp (mkIdent name)
                            (mkQuotedVarOp "testBit")
                            (mkLiteralInt n)

mkLet defs = LetStmt (BDecls defs)

mkFunBindWithBinds name pats ty rhs binds = FunBind [ Match dummyLoc (Ident name) pats ty (UnGuardedRhs rhs) (BDecls binds) ]

mkPatBind bn an = PatBind dummyLoc (PVar $ Ident bn) Nothing (UnGuardedRhs an) (BDecls [])
mkPatBindBang bn an = PatBind dummyLoc (PBangPat $ PVar $ Ident bn) Nothing (UnGuardedRhs an) (BDecls [])
mkPatBindWithBDecls bn body bdecls = PatBind dummyLoc (PVar $ Ident bn) Nothing (UnGuardedRhs body) (BDecls bdecls)

-- | create an expression "peek (ptr `plusPtr` rhs)"
peekPtrPlus rhs = App (mkIdent "peek")
                      (Paren $ InfixApp (mkIdent "ptr")
                                        (mkQuotedVarOp "plusPtr")
                                        (Paren rhs))

-- | same as peekPtrPlus but add a signature
peekPtrPlusSig rhs sig = ExpTypeSig dummyLoc (peekPtrPlus rhs) signature
	where signature = TyApp (mkTyCon "IO") (mkTyCon sig)
