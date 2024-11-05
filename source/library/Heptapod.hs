-- | This module provides functions for building and generating version 7 UUIDs
-- as defined by section 5.7 of RFC 9562.
--
-- <https://datatracker.ietf.org/doc/html/rfc9562#name-uuid-version-7>
module Heptapod where

import qualified Control.Monad.IO.Class as IO
import qualified Data.Bits as Bits
import qualified Data.ByteString as ByteString
import qualified Data.Int as Int
import qualified Data.Time.Clock.System as Time
import qualified Data.UUID.Types as UUID
import qualified Data.Word as Word
import qualified System.Entropy as Entropy

-- | Generates a UUIDv7 using the current time (from 'Time.getSystemTime') and
-- random data (from 'Entropy.getEntropy').
generate :: IO.MonadIO m => m UUID.UUID
generate = IO.liftIO $ do
  t <- Time.getSystemTime
  -- Note that we only need 74 bits (12 + 62) of randomness. That's a little
  -- more than 9 bytes (72 bits), so we have to request 10 bytes (80 bits) of
  -- entropy. The extra 6 bits are discarded.
  b <- Entropy.getEntropy 10
  pure $
    let u8_u64 = fromIntegral :: Word.Word8 -> Word.Word64
        f = Bits.shift . u8_u64 . ByteString.index b
        r = f 0 0 + f 1 8
        s = f 2 0 + f 3 8 + f 4 16 + f 5 24 + f 6 32 + f 7 40 + f 8 48 + f 9 56
     in build t r s

-- | Builds a UUIDv7 using the provided fields. Typically you will want to use
-- the 'generate' function instead.
build ::
  -- | Corresponds to the @unix_ts_ms@ field.
  Time.SystemTime ->
  -- | Corresponds to the @rand_a@ field. Only the low 12 bits are used.
  Word.Word64 ->
  -- | Corresponds to the @rand_b@ field. Only the low 62 bits are used.
  Word.Word64 ->
  UUID.UUID
build t r s =
  let i64_u64 = fromIntegral :: Int.Int64 -> Word.Word64
      u32_u64 = fromIntegral :: Word.Word32 -> Word.Word64
      unix_ts_ms =
        Bits.shift
          ( (i64_u64 (Time.systemSeconds t) * 1000)
              + u32_u64 (div (Time.systemNanoseconds t) 1000000)
          )
          16
      ver = Bits.shift 0x7 12 :: Word.Word64
      rand_a = r Bits..&. 0x0fff
      var = Bits.shift 0x2 62 :: Word.Word64
      rand_b = s Bits..&. 0x3fffffffffffffff
   in UUID.fromWords64 (unix_ts_ms + ver + rand_a) (var + rand_b)
