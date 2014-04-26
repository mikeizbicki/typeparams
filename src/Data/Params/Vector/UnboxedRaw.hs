{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- | An UnboxedRaw vector is just like the Unboxed vector, except that we no
-- longer store an offset to our position in the fixed size version.
-- This gives us a very thin wrapping around 'ByteArray's that is extremely
-- fast.
-- The only disadvantage is that we cannot make vectors of vectors of 
-- UnboxedRaw vectors.
-- If you don't need that capability, you should use this module.

module Data.Params.Vector.UnboxedRaw
    where

import Control.Monad
import Control.Monad.Primitive
import Control.DeepSeq
import Data.Primitive
import Data.Primitive.ByteArray
import Data.Primitive.Types
-- import GHC.Ptr
-- import Foreign.Ptr
-- import Foreign.ForeignPtr hiding (unsafeForeignPtrToPtr)
-- import Foreign.ForeignPtr.Unsafe
-- import Foreign.Marshal.Array
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Primitive.Mutable as VPM

import GHC.Base (Int (..))
import GHC.Int
import GHC.Prim
import GHC.TypeLits
import Data.Params

-------------------------------------------------------------------------------
-- immutable automatically sized vector

data family Vector (len::Maybe Nat) elem

instance (Show elem, VG.Vector (Vector len) elem) => Show (Vector len elem) where
    show v = "fromList "++show (VG.toList v)

instance (Eq elem, VG.Vector (Vector len) elem) => Eq (Vector len elem) where
    a == b = (VG.toList a) == (VG.toList b)

instance (Ord elem, VG.Vector (Vector len) elem) => Ord (Vector len elem) where
    compare a b = compare (VG.toList a) (VG.toList b)

---------------------------------------
-- fixed size 

newtype instance Vector (Just len) elem = Vector ByteArray

mkParams ''Vector


instance NFData (Vector (Just len) elem) where
    rnf a = seq a ()

instance 
    ( Prim elem 
    , KnownNat len
    ) => VG.Vector (Vector (Just len)) elem 
        where
    
    {-# INLINE basicUnsafeFreeze #-}
    basicUnsafeFreeze (MVector marr) = Vector `liftM` unsafeFreezeByteArray marr

    {-# INLINE basicUnsafeThaw #-}
    basicUnsafeThaw (Vector arr) = MVector `liftM` unsafeThawByteArray arr

    {-# INLINE [2] basicLength #-}
    basicLength _ = intparam (Proxy::Proxy len)

    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice j n v = if n /= intparam (Proxy::Proxy len) || j /= 0
        then error $ "Vector.basicUnsafeSlice not allowed to change size"
        else v

    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeIndexM (Vector arr) j = return $! indexByteArray arr j

    {-# INLINE basicUnsafeCopy #-}
    basicUnsafeCopy (MVector dst) (Vector src) = copyByteArray dst 0 src 0 (len*sz)
        where
            sz = sizeOf (undefined :: elem)
            len = intparam (Proxy::Proxy len)

    {-# INLINE elemseq #-}
    elemseq _ = seq

---------------------------------------
-- automatically sized

newtype instance Vector Nothing elem = Vector_Nothing (VP.Vector elem)

instance NFData elem => NFData (Vector Nothing elem) where
    rnf (Vector_Nothing v) = rnf v

instance Prim elem => VG.Vector (Vector Nothing) elem where 
    {-# INLINE basicUnsafeFreeze #-}
    basicUnsafeFreeze (MVector_Nothing v) = Vector_Nothing `liftM` VG.basicUnsafeFreeze v

    {-# INLINE basicUnsafeThaw #-}
    basicUnsafeThaw (Vector_Nothing v) = MVector_Nothing `liftM` VG.basicUnsafeThaw v

    {-# INLINE basicLength #-}
    basicLength (Vector_Nothing v) = VG.basicLength v

    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice i m (Vector_Nothing v) = Vector_Nothing $ VG.basicUnsafeSlice i m v

    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeIndexM (Vector_Nothing v) i = VG.basicUnsafeIndexM v i

    {-# INLINE basicUnsafeCopy #-}
    basicUnsafeCopy (MVector_Nothing mv) (Vector_Nothing v) = VG.basicUnsafeCopy mv v

    {-# INLINE elemseq #-}
    elemseq _ = seq

-------------------------------------------------------------------------------
-- mutable vector

type instance VG.Mutable (Vector len) = MVector len

data family MVector (len::Maybe Nat) s elem

---------------------------------------
-- fixed size

data instance MVector (Just len) s elem = MVector 
    {-#UNPACK#-}!(MutableByteArray s)

instance 
    ( Prim elem
    , KnownNat len
    ) => VGM.MVector (MVector (Just len)) elem 
        where

    {-# INLINE basicLength #-}
    basicLength _ = intparam (Proxy::Proxy len) 
    
    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice i m v = if m /= intparam (Proxy::Proxy len)
        then error $ "MVector.basicUnsafeSlice not allowed to change size; i="++show i++"; m="++show m++"; len="++show (intparam (Proxy::Proxy len))
        else v
 
    {-# INLINE basicOverlaps #-}
    basicOverlaps (MVector arr1) (MVector arr2)
        = sameMutableByteArray arr1 arr2

    {-# INLINE basicUnsafeNew #-}
    basicUnsafeNew n = MVector `liftM` newPinnedByteArray (len * sizeOf (undefined :: elem))
        where
            len = intparam (Proxy::Proxy len)

    {-# INLINE basicUnsafeRead #-}
    basicUnsafeRead (MVector arr) j = readByteArray arr j

    {-# INLINE basicUnsafeWrite #-}
    basicUnsafeWrite (MVector arr) j x = writeByteArray arr j x


    {-# INLINE basicUnsafeCopy #-}
    basicUnsafeCopy (MVector dst) (MVector src) = copyMutableByteArray dst 0 src 0 (len*sz)
        where
            sz = sizeOf (undefined :: elem)
            len = intparam (Proxy::Proxy len)
                    
    {-# INLINE basicUnsafeMove #-}
    basicUnsafeMove (MVector dst) (MVector src) = moveByteArray dst 0 src 0 (len * sz)
        where
            sz = sizeOf (undefined :: elem)
            len = intparam (Proxy::Proxy len)

    {-# INLINE basicSet #-}
    basicSet (MVector arr) x = setByteArray arr 0 (intparam(Proxy::Proxy len)) x
    
---------------------------------------
-- variable size

newtype instance MVector Nothing s elem = MVector_Nothing (VPM.MVector s elem)
mkParams ''MVector

instance Prim elem => VGM.MVector (MVector Nothing) elem where

    {-# INLINE basicLength #-}
    basicLength (MVector_Nothing v) = VGM.basicLength v

    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice i m (MVector_Nothing v) = MVector_Nothing $ VGM.basicUnsafeSlice i m v

    {-# INLINE basicOverlaps #-}
    basicOverlaps (MVector_Nothing v1) (MVector_Nothing v2) = VGM.basicOverlaps v1 v2

    {-# INLINE basicUnsafeNew #-}
    basicUnsafeNew i = MVector_Nothing `liftM` VGM.basicUnsafeNew i

    {-# INLINE basicUnsafeRead #-}
    basicUnsafeRead (MVector_Nothing v) i = VGM.basicUnsafeRead v i

    {-# INLINE basicUnsafeWrite #-}
    basicUnsafeWrite (MVector_Nothing v) i x = VGM.basicUnsafeWrite v i x

    {-# INLINE basicUnsafeCopy #-}
    basicUnsafeCopy (MVector_Nothing v1) (MVector_Nothing v2) = VGM.basicUnsafeCopy v1 v2

    {-# INLINE basicUnsafeMove #-}
    basicUnsafeMove (MVector_Nothing v1) (MVector_Nothing v2) = VGM.basicUnsafeMove v1 v2

    {-# INLINE basicSet #-}
    basicSet (MVector_Nothing v) x = VGM.basicSet v x

