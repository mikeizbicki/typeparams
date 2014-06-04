{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- | This modules extends the 'Prim' from "Data.Primitive" class to cases
-- where we don't know the primitive information (like the size) at compile
-- time.  Instead, we must pass in a 'PseudoPrimInfo' object that will get
-- evaluated on every function call and that contains the needed information.
--
-- For 'PseudoPrim' instances that are also 'Prim' instances, this involves 
-- no run time overhead.  For 'PseudoPrim' instances that cannot be made
-- 'Prim' instances, this involves a mild memory and speed bookkeeping 
-- overhead.
module Data.Params.PseudoPrim
    where

import GHC.Base (Int (..))
import GHC.Int
import GHC.Prim
import Data.Word

import Control.Monad.Primitive
import Data.Primitive

-------------------------------------------------------------------------------
-- PseudoPrim class

class PseudoPrim a where
    data family PseudoPrimInfo a
    pp_sizeOf#          :: PseudoPrimInfo a -> Int#
    pp_alignment#       :: PseudoPrimInfo a -> Int#
    pp_indexByteArray#  :: PseudoPrimInfo a -> ByteArray# -> Int# -> a
    pp_readByteArray#   :: PseudoPrimInfo a -> MutableByteArray# s -> Int# -> State# s -> (# State# s, a #)
    pp_writeByteArray#  :: PseudoPrimInfo a -> MutableByteArray# s -> Int# -> a -> State# s -> State# s

    -- | Do we need to evaluate the info in order to call these functions?
    seqInfo :: a -> Bool

    -- | If 'seqInfo' returns 'True', then this function is undefined.
    -- Otherwise, it containes an empty 'PseudoPrimInfo' whose type is
    -- sufficient to determine all the needed information.
    emptyInfo :: PseudoPrimInfo a

#define mkPseudoPrim(t,ppi) \
instance PseudoPrim t where\
    newtype PseudoPrimInfo t = ppi ()               ;\
    pp_sizeOf# a = sizeOf# (undefined :: t)         ;\
    pp_alignment# a = alignment# (undefined :: t)   ;\
    pp_indexByteArray# a = indexByteArray#          ;\
    pp_readByteArray# _ = readByteArray#            ;\
    pp_writeByteArray# _ = writeByteArray#          ;\
    seqInfo _ = False                               ;\
    emptyInfo = ppi ()               

mkPseudoPrim(Double,PseudoPrimInfo_Double)
mkPseudoPrim(Float,PseudoPrimInfo_Float)
mkPseudoPrim(Int,PseudoPrimInfo_Int)
mkPseudoPrim(Char,PseudoPrimInfo_Char)
mkPseudoPrim(Word8,PseudoPrimInfo_Word8)
mkPseudoPrim(Word16,PseudoPrimInfo_Word16)
mkPseudoPrim(Word32,PseudoPrimInfo_Word32)
mkPseudoPrim(Word64,PseudoPrimInfo_Word64)

-------------------------------------------------------------------------------
-- helper functions

{-# INLINE pp_sizeOf #-}
pp_sizeOf :: PseudoPrim a => PseudoPrimInfo a -> Int
pp_sizeOf x = I# (pp_sizeOf# x)

{-# INLINE pp_alignment #-}
pp_alignment :: PseudoPrim a => PseudoPrimInfo a -> Int
pp_alignment x = I# (pp_alignment# x)

{-# INLINE pp_readByteArray #-}
pp_readByteArray
  :: (PseudoPrim a, PrimMonad m) => PseudoPrimInfo a -> MutableByteArray (PrimState m) -> Int -> m a
pp_readByteArray ppi (MutableByteArray arr#) (I# i#) = primitive (pp_readByteArray# ppi arr# i#)

{-# INLINE pp_writeByteArray #-}
pp_writeByteArray
  :: (PseudoPrim a, PrimMonad m) => PseudoPrimInfo a -> MutableByteArray (PrimState m) -> Int -> a -> m ()
pp_writeByteArray ppi (MutableByteArray arr#) (I# i#) x = primitive_ (pp_writeByteArray# ppi arr# i# x)

{-# INLINE pp_indexByteArray #-}
pp_indexByteArray :: PseudoPrim a => PseudoPrimInfo a -> ByteArray -> Int -> a
pp_indexByteArray ppi (ByteArray arr#) (I# i#) = pp_indexByteArray# ppi arr# i#
