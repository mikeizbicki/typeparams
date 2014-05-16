{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

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

    seqInfo :: a -> Bool
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
