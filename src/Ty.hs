{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Ty where

type TyName = String

data Attrib =
	  AttribSkip
	| AttribSizeIsDefinedBy String
	deriving (Show,Eq)

data Dictionary = Dictionary [(TyName, Equation)]

initDictionary = Dictionary
	[ ("Bool", EquationConst 1)
	, ("Word8", EquationConst 1)
	, ("Word16", EquationConst 2)
	, ("Word32", EquationConst 4)
	, ("Word64", EquationConst 8)
	, ("Int8", EquationConst 1)
	, ("Int16", EquationConst 2)
	, ("Int32", EquationConst 4)
	, ("Int64", EquationConst 8)
	]

resolveSize :: Dictionary -> TyName -> Integer
resolveSize table@(Dictionary l) n = case lookup n l of
	Just e  -> eqResolve table e
	Nothing -> error ("cannot find type: " ++ show n)

getSize table@(Dictionary l) n = case lookup n l of
	Nothing -> error ("cannot find type: " ++ show n)
	Just eq -> eqResolve table eq

appendDictionary x (Dictionary l) = Dictionary (x:l)

data Equation =
	  EquationConst Integer
	| EquationTimesSym Integer String
	| EquationTimes Integer Integer
	| EquationPlus Equation Equation
	deriving (Show,Eq)

eqZero = EquationConst 0

eqIsConst (EquationConst _)      = True
eqIsConst (EquationTimesSym _ _) = False
eqIsConst (EquationTimes _ _)    = True
eqIsConst (EquationPlus e1 e2)   = eqIsConst e1 && eqIsConst e2

eqResolve _  (EquationConst i)         = i
eqResolve _  (EquationTimes n i)       = n * i
eqResolve tt (EquationTimesSym n name) = n * (resolveSize tt name)
eqResolve tt (EquationPlus e1 e2)      = eqResolve tt e1 + eqResolve tt e2

-- equation are only in the form k1 + k2 + .. + n1*s1 + ... + nn*sn
-- fold the constants together
eqSimplify eq
	| null vars = EquationConst consts
	| otherwise = EquationConst consts `EquationPlus` foldr1 EquationPlus vars
	where
		(consts, vars) = simplify eq
		simplify (EquationConst i)                    = (i, [])
		simplify (EquationTimes n i)                  = (n * i, [])
		simplify (EquationPlus e1 e2) = (k1+k2, l1 ++ l2) where
			(k1,l1) = simplify e1
			(k2,l2) = simplify e2
		simplify o                                    = (0, [o])

data Annotation =
	  AnnotationHidden
	| AnnotationVector
	| AnnotationOther String
	deriving (Show,Eq)

instance Read Annotation where
	readsPrec _ "hidden" = [(AnnotationHidden,"")]
	readsPrec _ "vector" = [(AnnotationVector,"")]
	readsPrec _ s        = [(AnnotationOther s,"")]

data FieldType = Normal TyName String (Maybe ArrayDef) [Annotation] | BitField TyName [(String,Int,[Annotation])]
	deriving (Show,Eq)

newtype CIndex = CIndex Int deriving (Show,Eq,Ord,Enum,Num)
newtype HsIndex = HsIndex Int deriving (Show,Eq,Ord,Enum,Num)

-- C to Haskell fields numbering
type FieldMap = [ (CIndex, [ (HsIndex, Maybe (Int, Int)) ]) ]

data CDesc = CDesc
	{ cOffset   :: Equation
	, cType     :: String -- the haskell name for the c type.
	, cBytesize :: Integer
	, cArrayDef :: Maybe ArrayDef
	, cAnnotations :: [Annotation]
	} deriving (Show,Eq)

data HsDesc = HsDesc
	{ hsName    :: String
	, hsType    :: String
	, hsBitsize :: Integer
	, hsAnnotations :: [Annotation]
	} deriving (Show,Eq)

data Mapping = Mapping
	{ structName :: String
	, structSize :: Equation
	, cDesc      :: [CDesc]
	, hsDesc     :: [HsDesc]
	, fieldMap   :: FieldMap
	} deriving (Show,Eq)

class VarIndex a where
	toIdentifier :: a -> String

instance VarIndex CIndex where
	toIdentifier (CIndex i) = "a" ++ show i

instance VarIndex HsIndex where
	toIdentifier (HsIndex i) = "b" ++ show i

data ArrayDef = ArrayStaticSize Integer | ArrayVariableSize String
	deriving (Show,Eq)
