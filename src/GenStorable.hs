{-# LANGUAGE ViewPatterns #-}
module Main where

import Data.Char
import Data.List (find, findIndex)

import Control.Applicative ((<$>))
import Language.Haskell.Exts.Pretty (prettyPrint)
import Language.Haskell.Exts.Syntax
import System.Environment (getArgs)

import Combinators
import DSL
import Ty

capitalize []     = []
capitalize (x:xs) = toUpper x : xs

decapitalize [] = []
decapitalize (x:xs) = toLower x : xs

toHs :: DSLType -> TyName
toHs (Pointer _)  = error "pointer unsupported for now"
toHs (Type basic) = case basic of
	"uint8_t"  -> "Word8"
	"int8_t"   -> "Int8"
	"uint16_t" -> "Word16"
	"int16_t"  -> "Int16"
	"uint32_t" -> "Word32"
	"int32_t"  -> "Int32"
	"uint64_t" -> "Word64"
	"int64_t"  -> "Int64"
	_          -> basic

-- | create an expression that looks like the following
--   (ident `shiftR` n) .&. (2^sz-1)
mkGetBitsExp ident n sz = App (mkIdent "fromIntegral") (Paren expr)
	where expr = InfixApp
		(Paren (Paren (InfixApp (mkIdent ident) (mkQuotedVarOp "shiftR") (mkLiteralInt n))))
		(mkQuotedVarOpSymbol ".&.")
		(Paren (InfixApp (InfixApp (mkLiteralInt 2) (mkQuotedVarOpSymbol "^") (mkLiteralInt sz)) (mkQuotedVarOpSymbol "-") (mkLiteralInt 1)))

sizeofExpr constSize [] = mkLiteralInt constSize
sizeofExpr constSize tys = foldl mkPlus base (map toExpr tys)
	where
		base = Lit $ Int $ fromIntegral constSize
		toExpr n = App (mkIdent "sizeOf")
		               (Paren $ ExpTypeSig dummyLoc (mkIdent "undefined") (mkTyCon n))

equationToExp_ _        (EquationConst i) = mkLiteralInt i
equationToExp_ _        (EquationTimes n i) = mkLiteralInt n `mkTimes` mkLiteralInt i
equationToExp_ resolver (EquationPlus e1 e2) = equationToExp_ resolver e1 `mkPlus` equationToExp_ resolver e2
equationToExp_ resolver (EquationTimesSym n sym) = mkLiteralInt n `mkTimes` resolver sym

equationToExp resolver = equationToExp_ resolver . eqSimplify

getBofA :: Mapping -> CIndex -> [HsIndex]
getBofA mapping n = maybe (error ("no bs for a: " ++ show n)) (map fst) $ lookup n $ fieldMap mapping

getAofB :: Mapping -> HsIndex -> CIndex
getAofB mapping n = maybe (error ("no a for b: " ++ show n)) fst $ find (elem n . map fst . snd) $ fieldMap mapping

getNameToIndex :: Mapping -> String -> CIndex
getNameToIndex mapping name = case findIndex ((== name) . hsName) (hsDesc mapping) of
	Nothing  -> error ("no field name " ++ name)
	Just idx -> getAofB mapping $ HsIndex idx

isDirectMapping :: Mapping -> CIndex -> Bool
isDirectMapping mapping n = case getBofA mapping n of
	[]  -> error "empty mapping"
	[_] -> True
	_   -> False

hsDescIsVisible = not . elem AnnotationHidden . hsAnnotations
hsDescGetVisible = filter hsDescIsVisible

-- | generate a storable instance for new define type
--
-- at the moment only peek is generated, however it shouldn't be
-- too hard to generate poke as all required information is available.
generateInstance mapping = mkInstDecl "Storable" (structName mapping) $ map InsDecl [sizeofBind, alignmentBind, peekBind] where
	aToB = concatMap (\(aN, bs) -> case bs of
		[(_, Nothing)] -> [] -- skip direct mapping (b_i = a_j) in the let
		l              -> map (\(bi, Just (sz,nbBits)) ->
			if sz == 1
				then mkPatBind (toIdentifier bi) (mkTestBit (toIdentifier aN) (fromIntegral nbBits))
				else mkPatBind (toIdentifier bi) (mkGetBitsExp (toIdentifier aN) (fromIntegral nbBits) (fromIntegral sz))
			) l
		) (fieldMap mapping)

	-- create the peek body function with generators, possible lets and then a return.
	peekBody = generators ++
		(if null aToB then [] else [mkLet aToB]) ++
		[Qualifier (InfixApp (mkIdent "return") (mkQuotedVarOpSymbol "$") retVal)]

	szExpr = if eqIsConst $ structSize mapping
		then mkLiteralInt $ eqResolve undefined $ structSize mapping
		else mkLiteralInt (-1) -- FIXME calling error would be better.
	align = 4

	sizeofBind = FunBind [Match dummyLoc (Ident "sizeOf") [PWildCard] Nothing (UnGuardedRhs szExpr) (BDecls [])]
	alignmentBind = FunBind [Match dummyLoc (Ident "alignment") [PWildCard] Nothing (UnGuardedRhs (Lit (Int align))) (BDecls [])]
	peekBind = FunBind [Match dummyLoc (Ident "peek") [PVar (Ident "ptr")] Nothing (UnGuardedRhs (Do peekBody)) (BDecls [])]

	retVal_ = Con (UnQual (Ident $ structName mapping))
	retVal = foldl App retVal_ $ map (\(_,idx) ->
		if isDirectMapping mapping $ getAofB mapping idx
			then mkIdent $ toIdentifier $ getAofB mapping idx
			else mkIdent $ toIdentifier idx
		) $ filter (hsDescIsVisible . fst) $ (zip (hsDesc mapping) [(HsIndex 0)..])

	generators = map (\(cdesc, argName) ->
		let resolver n
			| n == "i"  = mkIdent "i"
			| otherwise = Paren $ App (mkIdent "fromIntegral") $ mkIdent $ toIdentifier $ getNameToIndex mapping n in
		let rhs = case cArrayDef cdesc of
			Nothing                          ->
				peekPtrPlusSig (equationToExp resolver $ cOffset cdesc) (cType cdesc)
			Just (ArrayStaticSize arraySize) ->
				mkArrayLoop resolver (cBytesize cdesc) (cOffset cdesc) $ mkLiteralInt arraySize
			Just (ArrayVariableSize vname)   ->
				let arrayNb = getNameToIndex mapping vname in
				mkArrayLoop resolver (cBytesize cdesc) (cOffset cdesc) $ mkIdent (toIdentifier arrayNb)
			in
		mkGenerator (toIdentifier argName) rhs) $ zip (cDesc mapping) [(CIndex 0)..]

	mkArrayLoop resolver sz offset arrayMaxExp =
		let body = peekPtrPlus ((equationToExp resolver offset) `mkPlus` (mkLiteralInt sz  `mkTimes` mkIdent "i")) in
		App (App (mkIdent "mapM") (Paren (Lambda dummyLoc [PVar (Ident "i")] body)))
		    (EnumFromTo (mkLiteralInt 0) (Paren (InfixApp (App (mkIdent "fromIntegral") $ arrayMaxExp)
			                                                  (mkQuotedVarOpSymbol "-")
			                                                  (mkLiteralInt 1))))

generateHaskellType mapping autoDerive =
	mkDataDecl (structName mapping) [QualConDecl dummyLoc [] [] (RecDecl (Ident $ structName mapping) fields)] derives
	where
		fields = map (\h -> field (hsName h) (hsType h))
		       $ hsDescGetVisible $ hsDesc mapping
		field fname ty = ([Ident (decapitalize (structName mapping) ++ capitalize fname)], UnBangedTy (mkTyCon ty))
		derives = map (\s -> (UnQual (Ident s), [])) autoDerive

-- | generate 2 haskell declarations, the first one an instance
--   of Storable for the type and the second one the data declaration for the new type.
generateDecl mapping = [ generateInstance mapping, generateHaskellType mapping derives ]
	where derives = ["Show","Eq"]

-- | group bitfields in a structure together
getStructureGrouped _ fields = reverse $ foldl groupTy [] fields where
	groupTy [] (_,_,Just _,Just _,_) = error "cannot declare type as bitfield and array at the same time."
	groupTy [] (ty,fname,Just b,Nothing,annots)  = [BitField (toHs ty) [(fname,b,annots)]]
	groupTy (x:xs) (ty,fname,Just b,Nothing,annots) = case x of
		(BitField oty l)
			| oty == toHs ty -> BitField oty (l ++ [(fname,b,annots)]) : xs
			| otherwise -> BitField (toHs ty) [(fname,b,annots)] : x : xs
		_                   -> BitField (toHs ty) [(fname,b,annots)] : x : xs
	groupTy acc (ty,fname,Nothing,a,annots) = Normal (toHs ty) fname a annots : acc
	groupTy _ _ = error "unexpected error when processing DSL atoms"

-- | use the grouped field to produce a mapping type that contains
-- the mapping between haskell and c fields, the c fields description
-- and the haskell fields description.
getStructureMapping tt name groupedFields = Mapping name size (reverse cdesc) hsdesc fieldmap
	where
		(size, cdesc) = foldl f (eqZero, []) groupedFields
			where
				f (offset,acc) (Normal ty _ adef annots) =
					let oneConstSz = resolveSize tt ty in
					let sz = case adef of
						Nothing                    -> EquationConst oneConstSz
						Just (ArrayStaticSize i)   -> EquationTimes oneConstSz i
						Just (ArrayVariableSize s) -> EquationTimesSym oneConstSz s in
					(offset `EquationPlus` sz, CDesc offset ty oneConstSz adef annots : acc)
				f (offset,acc) (BitField ty _) =
					let constSz = resolveSize tt ty in
					let sz = EquationConst constSz in
					(offset `EquationPlus` sz, CDesc offset ty constSz Nothing [] : acc)
		hsdesc = foldr f [] groupedFields where
			f (Normal ty fname Nothing annots)  acc = HsDesc fname ty 0 annots : acc
			f (Normal ty fname (Just _) annots) acc = HsDesc fname ("[" ++ ty ++ "]") 0 annots : acc
			f (BitField _ l) acc               = map toHsDesc l ++ acc
			toHsDesc (bname,sz,annots)         = HsDesc bname (if sz == 1 then "Bool" else "Int") (fromIntegral sz) annots

		fieldmap = reverse $ fst3 $ foldl mapC2hs ([],CIndex 0,HsIndex 0) groupedFields
		fst3 (x,_,_) = x

		mapC2hs (acc, aN, bN) (Normal _ _ _ _) = ((aN, [(bN, Nothing)]) : acc, succ aN, succ bN)
		mapC2hs (acc, aN, bN) (BitField _ l) = ((aN, bs) : acc, succ aN, bN+(fromIntegral $ length l))
			where
				bs :: [(HsIndex, Maybe (Int,Int))]
				bs = reverse $ snd $ foldl f (0,[]) (zip l [(0::Int)..])
				f (bitIndex, facc) ((_, bits,_), i) =
					(bitIndex+bits,(bN+fromIntegral i, Just (bits, bitIndex)):facc)

process :: String -> [Atom] -> IO ()
process modName atoms = do
	let (mapTypes, _) = foldl transform ([], initDictionary) atoms
	let decls = concatMap generateDecl mapTypes

	let module_ = foldl (flip addDecl) (mkModule modName) $ reverse decls
	putStrLn $ prettyPrint $ foldl (\acc i -> addImport (mkImport i) acc) module_
		[ "Foreign.Storable"
		, "Foreign.Ptr"
		, "Data.Word"
		, "Data.Int"
		, "Data.Bits"
		]
	where
		transform (prevTypes, tt) (Table {}) = (prevTypes, tt)
		transform (prevMappings, tt) (Struct name fields) =
			let groupedFields = getStructureGrouped name fields in
			let mapping = getStructureMapping tt name groupedFields in
			let newDef = (name, structSize mapping) in
			(mapping : prevMappings, appendDictionary newDef tt)
		transform (prevTypes, tt) (Enum _ _) = (prevTypes, tt)

data Args = Args
	{ moduleName :: String
	, others     :: [String]
	} 

parseArgs :: [String] -> Args -> Args
parseArgs l e = case l of
	[]              -> e
	"-m":modName:xs -> parseArgs xs $ e { moduleName = modName }
	x:xs            -> parseArgs xs $ e { others = x:others e }

main = do
	args <- (\l -> parseArgs l (Args "NoName" [])) <$> getArgs
	case others args of
		[x] -> readDSL x >>= process (moduleName args)
		_   -> usage
	where
		usage = putStrLn "usage: gen-storable <interface-file>"

