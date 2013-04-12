module DSL (readDSL, parseDSL, numberToInt, Atom(..), TableElem(..), Number(..), DSLType(..)) where

import Control.Applicative ((<*), (*>), (<$>))
import Text.Parsec hiding (space)
import Ty
import Numeric

data DSLType =
      Type String
    | Pointer DSLType
    deriving (Show,Eq)

data Atom =
      Struct String [(DSLType, String, Maybe Int, Maybe ArrayDef, [Annotation])]
    | Table String [String] [TableElem]
    | Enum String [(String, Maybe Number)]
    deriving (Show,Eq)

data TableElem = TableRow [TableElem]
               | TableField String
               deriving (Show,Eq)

data Number = Hexadecimal Int
            | Octal       Int
            | Decimal     Int
            deriving (Show,Eq)

numberToInt :: Number -> Int
numberToInt (Hexadecimal i) = i
numberToInt (Octal i) = i
numberToInt (Decimal i) = i

readDSL :: FilePath -> IO [Atom]
readDSL path = retRight . parseDSL <$> readFile path
    where retRight = either (error . ("DSL error:" ++) . show) id

parseDSL :: String -> Either ParseError [Atom]
parseDSL content = parse document "input" content where

    document :: Parsec String () [Atom]
    document = many (eatVoid0 >> def)

    eatVoid = many1 $ choice [ commentLong, commentLine, eatSpaces1 ]
    eatVoid0 = optional eatVoid

    eatSpaces1 = many1 space >> return ()
    space = oneOf " \t\n";

    commentLong = try $ string "/*" >> manyTill anyChar (try $ string "*/") >> return ()
    commentLine = try $ string "//" >> manyTill anyChar (try $ char '\n') >> return ()

    number = hexNumber <|> octNumber <|> decNumber
    hexNumber = string "0x" >> many hexDigit >>= return . Hexadecimal . fst . head . readHex
    octNumber = string "0o" >> many octDigit >>= return . Octal . fst . head . readOct
    decNumber = many digit >>= return . Decimal . fst . head . readDec

    def :: Parsec String () Atom
    def = structDef <|> enumDef <|> tableDef
    structDef = do
        string "struct" >> eatVoid0 >> char '{' >> eatVoid0
        fields <- structField `endBy` char ';'
        _ <- eatVoid0 >> char '}'
        name <- eatVoid0 *> symbol <* eatVoid0
        _ <- char ';'
        eatVoid0
        return $ Struct name fields
    structField = try $ do
        ty <- getType
        name <- eatVoid0 *> symbol
        eatVoid0
        bitsize <- option Nothing (char ':' >> eatVoid0 >> many digit >>= return . Just . read)
        array   <- option Nothing ((char '[' *> arrayNumber <* char ']') >>= return . Just)
        annots  <- option [] (many (try (eatVoid0 >> char '#') >> symbol >>= return . read))
        return (ty,name,bitsize,array,annots)
    getType = do
        s <- eatVoid0 *> symbol
        case s of
            "pointer" -> Pointer <$> getType
            o         -> return $ Type o
    -- parse either a number or a @symbol
    arrayNumber = do
        eatVoid0
        an <- (ArrayStaticSize . read <$> many1 digit)
          <|> (char '@' >> symbol >>= return . ArrayVariableSize)
        eatVoid0
        return an

    enumDef = do
        string "enum" >> eatVoid0 >> char '{' >> eatVoid0
        fields <- enumField `sepBy` char ','
        _ <- char '}'
        name <- eatVoid0 *> symbol <* eatVoid0
        _ <- char ';'
        eatVoid0
        return $ Enum name fields
    enumField = do
        s  <- eatVoid0 *> symbol <* eatVoid0
        eq <- option Nothing ((char '=' *> eatVoid0 *> number <* eatVoid0) >>= return . Just)
        return (s,eq)

    tableDef = do
        string "table" >> eatVoid0
        string "::"
        tys <- (eatVoid0 *> symbol <* eatVoid0) `sepBy` (string "->")
        --value   <- eatVoid0 *> symbol <* eatVoid0

        _ <- char '{' >> eatVoid0
        fields <- (eatVoid0 *> tableElem <* eatVoid0) `sepBy` char ','
        _ <- char '}'
        name <- eatVoid0 *> symbol <* eatVoid0
        _ <- char ';'
        eatVoid0
        return $ Table name tys fields
    tableElem = tableRow <|> tableField
    tableRow  = do
        _ <- char '{' >> eatVoid0
        fields <- (eatVoid0 *> tableField <* eatVoid0) `sepBy` char ','
        _ <- char '}' >> eatVoid0
        return $ TableRow fields
    tableField = do
        s <- symbol
        return $ TableField s

    symbol = many1 $ oneOf (['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z'] ++ ['_'])
