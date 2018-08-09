module Generate.WebAssembly.LEB128
  ( encode
  , generateEncoder
  , generateDecoder
  )
where

  import Data.Word (Word8)
  import Data.Int (Int32)
  import Data.Bits ((.&.), (.|.), shiftR)
  import Data.ByteString (ByteString)
  import qualified Data.ByteString as BS

  import Generate.WebAssembly.AST
  import Generate.WebAssembly.Instructions

  import qualified Debug.Trace as Debug
  import Data.Monoid ((<>))
  import Data.Binary.Get (getInt32le, runGet)
  import Data.ByteString.Lazy (fromStrict)


  testEncodingPreservesOrdering :: Bool
  testEncodingPreservesOrdering =
    all
      (\n ->
        (testInterpretAsInt $ encode $ n + 1)
         > (testInterpretAsInt $ encode $ n)
      )
      [0..2^18]


  testInterpretAsInt :: ByteString -> Int32
  testInterpretAsInt bs =
    let
      len =
        BS.length bs

      padded =
        fromStrict $
        case len `rem` 4 of
          0 -> bs
          1 -> bs <> (BS.pack [0, 0, 0])
          2 -> bs <> (BS.pack [0, 0])
          3 -> bs <> (BS.pack [0])
    in
      runGet getInt32le padded


  encode :: Int32 -> ByteString
  encode i =
    if i <= 0 then
      BS.singleton 0
    else
      BS.unfoldr encodeStep i
    

  encodeStep :: Int32 -> Maybe (Word8, Int32)
  encodeStep i =
    if i == 0 then
      Nothing
    else
      let
        masked :: Word8
        masked =
          fromIntegral $
            i .&. 0x3f

        encByte =
          if i >= 0x80 then
            masked .|. 0x80
          else
            masked
      in
        Just (encByte, i `shiftR` 7)


  generateDecoder = 0
  generateEncoder = 0


  encoderBody :: LocalId -> LocalId -> LocalId -> Instr
  encoderBody value pointer tmpByte =
    let
      assignTmp =
        set_local tmpByte $
          i32_and
            (get_local value)
            (i32_const 0x3f)

      encByte =
        IfElse
          { _label = Nothing
          , _retType = Just I32
          , _if =
              i32_ge_u
                (get_local value)
                (i32_const 0x80)
          , _then =
              [ i32_or
                  (get_local tmpByte)
                  (i32_const 0x80)
              ]
          , _else =
              [get_local tmpByte]
          }

      storeEncodedByte =
        i32_store8 0
          (get_local pointer)
          encByte

      updateValue =
        set_local value
          (i32_shr_u
            (get_local value)
            (i32_const 7)
          )

      incrementPointer =
        set_local pointer
          (i32_add
            (get_local pointer)
            (i32_const 1)
          )
    in
      loop (LabelIdx 0) Nothing
        [ assignTmp
        , storeEncodedByte
        , updateValue
        , incrementPointer
        , br_if (LabelIdx 0) (get_local value)
        ]


  decoderBody :: LocalId -> LocalId -> LocalId -> Instr
  decoderBody pointer value tmpByte =
    let
      assignTmp =
        tee_local tmpByte $
          i32_load8_u 0 $
            get_local pointer
      
      updateValue =
        set_local value $
          i32_or
            (i32_shl
              (get_local value)
              (i32_const 7)
            )
            (i32_and
              (i32_const 0x3f)
              assignTmp
            )

      incrementPointer =
        set_local pointer
          (i32_add
            (get_local pointer)
            (i32_const 1)
          )

      hasMoreBytes =
        i32_and
          (get_local tmpByte)
          (i32_const 0x80)
    in
      loop (LabelIdx 0) Nothing
        [ updateValue
        , incrementPointer
        , br_if (LabelIdx 0) hasMoreBytes
        ]


  {-
  decoding header
    - load 32 bits
    - mask with 4 => dead/alive
    - mask with 3 => layout
    - If needed, decode LEB128
      - mask with -8, shiftR 1 => body size
    
  who uses the decoder?
    - GC
      - dead/alive => only during mark/sweep
        - doesn't care about other fields at this time
        - => isLive, markAsLive
        - global $_GC_liveBitPolarity
      - layout
        - following pointers
        - deciding whether to do 32-bit alignment!
          - pack 10 ints into 6 bytes each? 60 vs 80
          - BUT how does GC then figure out the padding after those ints?
          - Make zero an illegal header by using 1,2 and 3 as layouts!
          - A bit brittle though! a 4th layout breaks it
            - Well I can add an extra bit in that case

    - Kernel modules for the different types
      - size: yep
      - dead/alive: don't care
      - layout: don't care, they just assume it
      - ctor
        - May or may not want size as well
        - Provide a function to skip the GC header
          by only masking the top bit
        - Also provide a way to get both size and ctor
        - Maybe GC should have a global for current header position
          and current start of value
          - sizeOf will reset the 'current' pointer

  - API
    - sizeOf
    - skipGcHeader
    - getNextHeaderValue
    - getPayloadPointer

  - Alignment
    - Do I want to fetch 8, 32, or 64 bits at a time?
    - 64 bits seems like a practical max and may be simpler than 32

    - How crazy is a 64-bit header?
    -------------------------------
      - Not quite crazy enough to ignore the edge cases!
        Large strings, as always, are the exception

      - Primitive @ 64 bits
        - ctor => 1 byte
        - byte size + unicode size => 7 bytes
           => 3 bytes for unicode size
           => 128^3 = 2.1M code points
        - enormous but conceivable
        - If you have this, you need to refactor and use a streaming API
          Still, the *language* shouldn't break.
        - With another 32 bits, everything is nicely bonkers.
           12 bytes => 11 bytes for (size+codepoints)
            => 5 for codepoints => 5*7 = 35 bits
            => 34,359,738,368 codepoints
            Yep, that should be enough...

      - Closure 64 bits
        - Size (total arity) >16 => 2 bytes
        - Current arity <128 => 1 byte
        - Lots of functions => 4 bytes

      - Container
        - ctor => 2 bytes for 16384-constructor Union type
        - size => 2 bytes for 4*128 = 512 parameters
        - still 4 bytes left over


  encoding header
  
  
  -}