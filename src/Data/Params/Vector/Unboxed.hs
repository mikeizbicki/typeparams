{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE OverlappingInstances #-}
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

data family Vector (len::Param Nat) elem

instance (Show elem, VG.Vector (Vector len) elem) => Show (Vector len elem) where
    show v = "fromList "++show (VG.toList v)

instance (Eq elem, VG.Vector (Vector len) elem) => Eq (Vector len elem) where
    a == b = (VG.toList a) == (VG.toList b)

instance (Ord elem, VG.Vector (Vector len) elem) => Ord (Vector len elem) where
    compare a b = compare (VG.toList a) (VG.toList b)

---------------------------------------
-- Static size 

data instance Vector (Static len) elem = Vector 
    {-#UNPACK#-}!Int 
    {-#UNPACK#-}!ByteArray

mkParams ''Vector

-- class SetParamInner n where
--     setParamInner :: SetParam p m => DefParam p m -> ((p m) => n m) -> n m
-- 
-- instance SetParamInner (Vector len)
--     setParamInner 

instance NFData (Vector (Static len) elem) where
    rnf a = seq a ()

instance 
    ( Prim elem 
    , KnownNat len
    ) => VG.Vector (Vector (Static len)) elem 
        where
    
    {-# INLINE basicUnsafeFreeze #-}
    basicUnsafeFreeze (MVector i marr) = Vector i `liftM` unsafeFreezeByteArray marr

    {-# INLINE basicUnsafeThaw #-}
    basicUnsafeThaw (Vector i arr) = MVector i `liftM` unsafeThawByteArray arr

    {-# INLINE [2] basicLength #-}
    basicLength _ = param_len (undefined::Vector (Static len) elem) 

    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice j n v = if n /= param_len (undefined::Vector (Static len) elem) || j /= 0
        then error $ "Vector.basicUnsafeSlice not allowed to change size"
        else v 

    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeIndexM (Vector i arr) j = return $! indexByteArray arr (i+j)

    {-# INLINE basicUnsafeCopy #-}
    basicUnsafeCopy (MVector i dst) (Vector j src) = copyByteArray dst (i*sz) src (j*sz) (len*sz)
        where
            sz = sizeOf (undefined :: elem)
            len = param_len (undefined::Vector (Static len) elem)

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
    ) => Prim (Vector (Static len) elem)
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
                    iii = I# (i# *# (sizeOf# (undefined::Vector (Static len) elem)) +# (unInt i)) 

---------------------------------------
-- RunTime size 

data instance Vector RunTime elem = Vector_RunTime 
    {-#UNPACK#-}!Int 
    {-#UNPACK#-}!ByteArray

instance NFData (Vector RunTime elem) where
    rnf a = seq a ()

instance 
    ( Prim elem 
    , Param_len (Vector RunTime elem)
    ) => VG.Vector (Vector RunTime) elem 
        where
    
    {-# INLINE basicUnsafeFreeze #-}
    basicUnsafeFreeze (MVector_RunTime len i marr) = if len==param_len (undefined::Vector RunTime elem)
        then Vector_RunTime i `liftM` unsafeFreezeByteArray marr
        else error $ "basicUnsafeFreeze cannot change RunTime vector size"
            ++ "; len="++show len
            ++ "; param_len="++show (param_len (undefined::Vector RunTime elem))

    {-# INLINE basicUnsafeThaw #-}
    basicUnsafeThaw (Vector_RunTime i arr) = MVector_RunTime (param_len (undefined::Vector RunTime elem)) i `liftM` unsafeThawByteArray arr

    {-# INLINE [2] basicLength #-}
    basicLength _ = param_len (undefined::Vector RunTime elem) 

    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice j n v = if n /= param_len (undefined::Vector RunTime elem) || j /= 0
        then error $ "Vector_RunTime.basicUnsafeSlice not allowed to change size"
        else v 

    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeIndexM (Vector_RunTime i arr) j = return $! indexByteArray arr (i+j)

    {-# INLINE basicUnsafeCopy #-}
    basicUnsafeCopy (MVector_RunTime n i dst) (Vector_RunTime j src) = if n==len
        then copyByteArray dst (i*sz) src (j*sz) (len*sz)
        else error "basicUnsafeCopy cannot change RunTime vector size"
        where
            sz = sizeOf (undefined :: elem)
            len = param_len (undefined::Vector RunTime elem)

    {-# INLINE elemseq #-}
    elemseq _ = seq

-------------------

instance
    ( Prim elem
    , Param_len (Vector RunTime elem)
    ) => Prim (Vector RunTime elem)
        where
    
    {-# INLINE sizeOf# #-}
    sizeOf# _ = unInt (sizeOf (undefined::elem) * (param_len (undefined::Vector RunTime elem)))

    {-# INLINE alignment# #-}
    alignment# _ = unInt (sizeOf (undefined::elem) * (param_len (undefined::Vector RunTime elem)))

    {-# INLINE indexByteArray# #-}
    indexByteArray# arr# i# = Vector_RunTime ((I# i#)*(param_len (undefined::Vector RunTime elem))) (ByteArray arr#)

    {-# INLINE readByteArray# #-}
    readByteArray# marr# i# s# = (# s#, Vector_RunTime (I# i#) (ByteArray (unsafeCoerce# marr#)) #)

    {-# INLINE writeByteArray# #-}
    writeByteArray# marr# i# x s# = go 0 s#
        where
            go i s = ( if i >= len
                then s
                else go (i+1) 
                        (writeByteArray# marr# 
                            (i# *# (unInt len) +# (unInt i)) 
                            (x VG.! i)
                            s
                        )
                    )
                where 
                    len = param_len (undefined::Vector RunTime elem)
                    iii = I# (i# *# (sizeOf# (undefined::Vector RunTime elem)) +# (unInt i)) 


---------------------------------------
-- Automatic sized

newtype instance Vector Automatic elem = Vector_Automatic (VP.Vector elem)

instance NFData elem => NFData (Vector Automatic elem) where
    rnf (Vector_Automatic v) = rnf v

instance Prim elem => VG.Vector (Vector Automatic) elem where 
    {-# INLINE basicUnsafeFreeze #-}
    basicUnsafeFreeze (MVector_Automatic v) = Vector_Automatic `liftM` VG.basicUnsafeFreeze v

    {-# INLINE basicUnsafeThaw #-}
    basicUnsafeThaw (Vector_Automatic v) = MVector_Automatic `liftM` VG.basicUnsafeThaw v

    {-# INLINE basicLength #-}
    basicLength (Vector_Automatic v) = VG.basicLength v

    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice i m (Vector_Automatic v) = Vector_Automatic $ VG.basicUnsafeSlice i m v

    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeIndexM (Vector_Automatic v) i = VG.basicUnsafeIndexM v i

    {-# INLINE basicUnsafeCopy #-}
    basicUnsafeCopy (MVector_Automatic mv) (Vector_Automatic v) = VG.basicUnsafeCopy mv v

    {-# INLINE elemseq #-}
    elemseq _ = seq

-------------------------------------------------------------------------------
-- mutable vector

type instance VG.Mutable (Vector len) = MVector len
-- type instance VG.Mutable (Vector (Static len)) = MVector (Static len)
-- type instance VG.Mutable (Vector Automatic) = MVector Automatic
-- type instance VG.Mutable (Vector RunTime) = MVector Automatic

data family MVector (len::Param Nat) s elem

---------------------------------------
-- static size

data instance MVector (Static len) s elem = MVector 
    {-#UNPACK#-}!Int 
    {-#UNPACK#-}!(MutableByteArray s)

instance 
    ( Prim elem
    , KnownNat len
    ) => VGM.MVector (MVector (Static len)) elem 
        where

    {-# INLINE basicLength #-}
    basicLength _ = param_len (undefined::MVector (Static len) s elem) 
    
    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice i m v = if m /= len
        then error $ "MVector.basicUnsafeSlice not allowed to change size"
            ++"; i="++show i
            ++"; m="++show m
            ++"; len="++show len 
        else v
        where
            len = param_len (undefined::MVector (Static len) s elem)
 
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
-- RunTime size

data instance MVector RunTime s elem = MVector_RunTime
    {-#UNPACK#-}!Int 
    {-#UNPACK#-}!Int 
    {-#UNPACK#-}!(MutableByteArray s)

instance 
    ( Prim elem
    ) => VGM.MVector (MVector RunTime) elem 
        where

    {-# INLINE basicLength #-}
    basicLength (MVector_RunTime n _ _) = n
    
    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice i m (MVector_RunTime n j v) = MVector_RunTime m (i+j) v
--     basicUnsafeSlice i m v = if m /= len
--         then error $ "MVector.basicUnsafeSlice not allowed to change size"
--             ++"; i="++show i
--             ++"; m="++show m
--             ++"; len="++show len 
--         else v
--         where
--             len = VGM.length v
 
    {-# INLINE basicOverlaps #-}
    basicOverlaps (MVector_RunTime m i arr1) (MVector_RunTime n j arr2)
        = sameMutableByteArray arr1 arr2
        && (between i j (j+m) || between j i (i+n))
            where
                between x y z = x >= y && x < z

    {-# INLINE basicUnsafeNew #-}
    basicUnsafeNew n = MVector_RunTime n 0 `liftM` newPinnedByteArray (n * sizeOf (undefined :: elem))

    {-# INLINE basicUnsafeRead #-}
    basicUnsafeRead (MVector_RunTime _ i arr) j = readByteArray arr (i+j)

    {-# INLINE basicUnsafeWrite #-}
    basicUnsafeWrite (MVector_RunTime _ i arr) j x = writeByteArray arr (i+j) x


--     {-# INLINE basicUnsafeCopy #-}
--     basicUnsafeCopy (MVector_RunTime n i dst) (MVector_RunTime m j src) 
--         = if n==m
--             then copyMutableByteArray dst (i*sz) src (j*sz) (n*sz)
--             else error "basicUnsafeCopy cannot change size of RunTime MVector"
--         where
--             sz = sizeOf (undefined :: elem)
--                     
--     {-# INLINE basicUnsafeMove #-}
--     basicUnsafeMove (MVector_RunTime n i dst) (MVector_RunTime m j src) 
--         = if n==m
--             then moveByteArray dst (i*sz) src (j*sz) (n * sz)
--             else error "basicUnsafeMove cannot change size of RunTime MVector"
--         where
--             sz = sizeOf (undefined :: elem)

    {-# INLINE basicSet #-}
    basicSet (MVector_RunTime n i arr) x = setByteArray arr i n x
    

---------------------------------------
-- Automatic size

newtype instance MVector Automatic s elem = MVector_Automatic (VPM.MVector s elem)
mkParams ''MVector

instance Prim elem => VGM.MVector (MVector Automatic) elem where

    {-# INLINE basicLength #-}
    basicLength (MVector_Automatic v) = VGM.basicLength v

    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice i m (MVector_Automatic v) = MVector_Automatic $ VGM.basicUnsafeSlice i m v

    {-# INLINE basicOverlaps #-}
    basicOverlaps (MVector_Automatic v1) (MVector_Automatic v2) = VGM.basicOverlaps v1 v2

    {-# INLINE basicUnsafeNew #-}
    basicUnsafeNew i = MVector_Automatic `liftM` VGM.basicUnsafeNew i

    {-# INLINE basicUnsafeRead #-}
    basicUnsafeRead (MVector_Automatic v) i = VGM.basicUnsafeRead v i

    {-# INLINE basicUnsafeWrite #-}
    basicUnsafeWrite (MVector_Automatic v) i x = VGM.basicUnsafeWrite v i x

    {-# INLINE basicUnsafeCopy #-}
    basicUnsafeCopy (MVector_Automatic v1) (MVector_Automatic v2) = VGM.basicUnsafeCopy v1 v2

    {-# INLINE basicUnsafeMove #-}
    basicUnsafeMove (MVector_Automatic v1) (MVector_Automatic v2) = VGM.basicUnsafeMove v1 v2

    {-# INLINE basicSet #-}
    basicSet (MVector_Automatic v) x = VGM.basicSet v x
