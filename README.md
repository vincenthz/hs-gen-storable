Gen-storable
============

Gen-storable is a simple code generator for haskell, that generates
types and the Storable instance associated with it.

As input, it takes a file containing some pseudo C definitions of structure and
enums, and generate a haskell module for output.

    $ hs-gen-storable example/typelib.c

Only sized C types are supported as basic element for now (eg. uint32\_t,
uint8\_t, int16\_t), however you can compose the type you build.

Also supported are bitfields and arrays (either of static size, or of size
determined through one of the field of the structure).

For example:

    struct {
       uint32_t id;
       uint32_t offset;
    } Section;

Will transform into:

    Data Section = Section
                    { sectionId :: Word32
                    , sectionOffset :: Word32
                    } deriving (Show,Eq)
     
    instance Storable Section where
        sizeof _ = 8
        alignment _ = 4
        peek ptr = do
                   a1 <- peek (ptr `plusPtr` 0) :: IO Word32
                   a2 <- peek (ptr `plusPtr` 4) :: IO Word32
                   return $ Section a1 a2

Annotations
-----------

gen-storable supports annotations in the IDL to tweak code generation.
At the moment only the Hidden annotation is available, and give the
ability to hide fields which are only present in the structure for alignment
or generation purpose (see array with dynamic size). Annotation follows the
fields and start by #.

Future possible annotations:

 * generate vector instead of list for arrays
 * generate bytestring for array of word8
 * automatically handle big/little endian to host endianness

TODO
----

 * Generate code for Storable's poke.
 * add (unsigned) long and pointers support.
 * more code check.
 * check big-endian support for bitfields from C spec.
 * cleanup
 * identifier haskell<->c cleanup (eg. convert to camel case).
 * remove ugly code from DSL parsing.
 * generate newtype with no record field instead of data for structure with only one field.
 * add endianness annotation.
 * add array-as-vector annotation.
 * add array-of-uint8-as-byteString annotation.
 * add enum support.
 * add union support.
 * do import on demand.
 * handle alignment. at the moment assumes everything is aligned properly.
 * library'fication.

Other Examples
--------------

Bitfields:

    /* pseudo C like definition */
    struct { uint16_t a:1; uint16_t b:2; uint16_t c:3; uint16_t reserved:10; } BitfieldExample;

    /* output the following */
    data BitfieldExample = BitfieldExample
        { bitfieldExampleA :: Bool
        , bitfieldExampleB :: Int
        , bitfieldExampleC :: Int
        , bitfieldExampleReserved :: Int
        } deriving (Show, Eq)
 
    instance Storable BitfieldExample where
        sizeOf _ = 2
        alignment _ = 4
        peek ptr = do
               a0 <- peek (ptr `plusPtr` (0)) :: IO Word16
               let b0 = a0 `testBit` 0
                   b1 = fromIntegral (((a0 `shiftR` 1)) .&. (2 ^ 2 - 1))
                   b2 = fromIntegral (((a0 `shiftR` 3)) .&. (2 ^ 3 - 1))
                   b3 = fromIntegral (((a0 `shiftR` 6)) .&. (2 ^ 10 - 1))
               return $ BitfieldExample b0 b1 b2 b3

Inline arrays:

     /* pseudo C like definition .. */
     struct { uint8_t content[16]; } StaticArray;

     /* output the following */
     data StaticArray = StaticArray
         { staticArrayContent :: [Word8]
         } deriving (Show,Eq)
  
     instance Storable StaticArray where
         sizeOf _ = 16
         alignment _ = 4
         peek ptr = do
             a0 <- mapM (\i -> peek (ptr `plusPtr` i * 1)) [0..(16-1)]
             return $ StaticArray a0

Variable size array where the size depends from an earlier field:

     /* this structure represent a pascal like byte string.
        the first field represent the size of the next field. */

     struct { uint32_t length; uint8_t varray[@length]; } PString;

     /* will be transformed into the following haskell code */
     data PString = PString
          { pStringLength :: Word32
          , pStringVarray :: [Word8]
          } deriving (Show,Eq)

     instance Storable PString where
          sizeof _ = -1
          alignment _ = 4
          peek ptr = do
              a0 <- peek (ptr `plusPtr` 0) :: IO Word32
              a1 <- mapM (\i ptr `plusPtr` 4 + i * 1) [0..a0-1]
              return $ PString a0 a1
