{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module Data.Params.Vector.Unboxed
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

data instance Vector (Just len) elem = Vector 
    {-#UNPACK#-}!Int 
    {-#UNPACK#-}!ByteArray

mkParams ''Vector


instance NFData (Vector (Just len) elem) where
    rnf a = seq a ()

instance 
    ( Prim elem 
    , KnownNat len
    ) => VG.Vector (Vector (Just len)) elem 
        where
    
    {-# INLINE basicUnsafeFreeze #-}
    basicUnsafeFreeze (MVector i marr) = Vector i `liftM` unsafeFreezeByteArray marr

    {-# INLINE basicUnsafeThaw #-}
    basicUnsafeThaw (Vector i arr) = MVector i `liftM` unsafeThawByteArray arr

    {-# INLINE [2] basicLength #-}
    basicLength _ = intparam (Proxy::Proxy len)

    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice j n v = if n /= intparam (Proxy::Proxy len) || j /= 0
        then error $ "Vector.basicUnsafeSlice not allowed to change size"
        else v

    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeIndexM (Vector i arr) j = return $! indexByteArray arr (i+j)

    {-# INLINE basicUnsafeCopy #-}
    basicUnsafeCopy (MVector i dst) (Vector j src) = copyByteArray dst (i*sz) src (j*sz) (len*sz)
        where
            sz = sizeOf (undefined :: elem)
            len = intparam (Proxy::Proxy len)

    {-# INLINE elemseq #-}
    elemseq _ = seq

-------------------
-- primitive instance allows unboxing unboxed vectors

unInt :: Int -> Int#
unInt (I# i) = i

instance
    ( Prim elem
    , KnownNat len
    , Show elem
    ) => Prim (Vector (Just len) elem)
        where
    
    {-# INLINE sizeOf# #-}
    sizeOf# _ = unInt (sizeOf (undefined::elem) * (intparam (Proxy::Proxy len)))

    {-# INLINE alignment# #-}
    alignment# _ = unInt (sizeOf (undefined::elem) * (intparam (Proxy::Proxy len)))

    {-# INLINE indexByteArray# #-}
    indexByteArray# arr# i# = Vector ((I# i#)*(intparam (Proxy::Proxy len))) (ByteArray arr#)

    {-# INLINE readByteArray# #-}
    readByteArray# marr# i# s# = (# s#, Vector (I# i#) (ByteArray (unsafeCoerce# marr#)) #)

    {-# INLINE writeByteArray# #-}
    writeByteArray# marr# i# x s# = go 0 s#
        where
            go i s = ( if i >= intparam (Proxy::Proxy len)
                then s
                else go (i+1) 
                        (writeByteArray# marr# 
                            (i# *# (unInt ( intparam (Proxy::Proxy len))) +# (unInt i)) 
                            (x VG.! i)
                            s
                        )
                    )
                where 
                    iii = I# (i# *# (sizeOf# (undefined::Vector (Just len) elem)) +# (unInt i)) 

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
    {-#UNPACK#-}!Int 
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
    basicOverlaps (MVector i arr1) (MVector j arr2)
        = sameMutableByteArray arr1 arr2
        && (between i j (j+len) || between j i (i+len))
            where
                len = intparam (Proxy::Proxy len)
                between x y z = x >= y && x < z

    {-# INLINE basicUnsafeNew #-}
    basicUnsafeNew n = MVector 0 `liftM` newPinnedByteArray (len * sizeOf (undefined :: elem))
        where
            len = intparam (Proxy::Proxy len)

    {-# INLINE basicUnsafeRead #-}
    basicUnsafeRead (MVector i arr) j = readByteArray arr (i+j)

    {-# INLINE basicUnsafeWrite #-}
    basicUnsafeWrite (MVector i arr) j x = writeByteArray arr (i+j) x


    {-# INLINE basicUnsafeCopy #-}
    basicUnsafeCopy (MVector i dst) (MVector j src) = copyMutableByteArray dst (i*sz) src (j*sz) (len*sz)
        where
            sz = sizeOf (undefined :: elem)
            len = intparam (Proxy::Proxy len)
                    
    {-# INLINE basicUnsafeMove #-}
    basicUnsafeMove (MVector i dst) (MVector j src) = moveByteArray dst (i*sz) src (j*sz) (len * sz)
        where
            sz = sizeOf (undefined :: elem)
            len = intparam (Proxy::Proxy len)

    {-# INLINE basicSet #-}
    basicSet (MVector i arr) x = setByteArray arr i (intparam(Proxy::Proxy len)) x
    
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