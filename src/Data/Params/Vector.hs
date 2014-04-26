{-# LANGUAGE PolyKinds, DataKinds #-}

module Data.Params.Vector
    where

import Control.DeepSeq
import Control.Monad
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

import GHC.TypeLits
import Data.Primitive
import Data.Primitive.ByteArray

import Data.Params

-------------------------------------------------------------------------------
-- immutable

data family Vector (n:: Maybe Nat) a 

instance NFData (VU.Vector a) => NFData (Vector Nothing a) where
    rnf (VectorNothing a) = rnf a

instance NFData (Vector (Just n) a) where
    rnf (VectorJust off arr) = seq off $ seq arr $ ()

newtype instance Vector Nothing a = VectorNothing { vec :: VU.Vector a }
data instance Vector (Just n) a = VectorJust 
    { off :: {-# UNPACK #-} !Int
    , arr :: {-# UNPACK #-} !ByteArray 
    }

type instance VG.Mutable (Vector n) = (MVector n)

instance VU.Unbox a => VG.Vector (Vector Nothing) a where
    {-# INLINE basicUnsafeFreeze #-}
    {-# INLINE basicUnsafeThaw #-}
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeFreeze (MVectorNothing v) = VectorNothing `liftM` VG.basicUnsafeFreeze v 
    basicUnsafeThaw (VectorNothing v) = undefined -- MVectorNothing `liftM` VG.basicUnsafeFreeze v
    basicLength (VectorNothing v) = VG.basicLength v
    basicUnsafeSlice i m (VectorNothing v) = VectorNothing $ VG.basicUnsafeSlice i m v
    basicUnsafeIndexM (VectorNothing v) i = VG.basicUnsafeIndexM v i

instance (Prim a, KnownNat n) => VG.Vector (Vector (Just n)) a where
    {-# INLINE basicUnsafeFreeze #-}
    {-# INLINE basicUnsafeThaw #-}
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeFreeze (MVectorJust off arr) = VectorJust off `liftM` unsafeFreezeByteArray arr
    basicUnsafeThaw (VectorJust off arr) = MVectorJust off `liftM` unsafeThawByteArray arr
    basicLength _ = fromIntegral $ natVal (Proxy :: Proxy n)
    basicUnsafeSlice = error "Cannot slice array"
    basicUnsafeIndexM (VectorJust off arr) j = return $! indexByteArray arr (off+j)

-------------------------------------------------------------------------------
-- mutable

data family MVector (n:: Maybe Nat) s a 

newtype instance MVector Nothing s a = MVectorNothing { mvec :: VU.MVector s a }
data instance MVector (Just n) s a = MVectorJust 
    { moff :: {-# UNPACK #-} !Int
    , marr :: {-# UNPACK #-} !(MutableByteArray s) 
    }

instance VGM.MVector VUM.MVector a => VGM.MVector (MVector Nothing) a where
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicOverlaps #-}
    {-# INLINE basicUnsafeNew #-}
    {-# INLINE basicUnsafeRead #-}
    {-# INLINE basicUnsafeWrite #-}
    basicLength v = VGM.basicLength $ mvec v
    basicUnsafeSlice i m v = MVectorNothing $ VGM.basicUnsafeSlice i m $ mvec v
    basicOverlaps v1 v2 = VGM.basicOverlaps (mvec v1) (mvec v2)
    basicUnsafeNew n = MVectorNothing `liftM` VGM.basicUnsafeNew n
    basicUnsafeRead v j = VGM.basicUnsafeRead (mvec v) j
    basicUnsafeWrite v j x = VGM.basicUnsafeWrite (mvec v) j x

instance (Prim a, KnownNat n) => VGM.MVector (MVector (Just n)) a where
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicOverlaps #-}
    {-# INLINE basicUnsafeNew #-}
    {-# INLINE basicUnsafeRead #-}
    {-# INLINE basicUnsafeWrite #-}
    basicLength _ = fromIntegral $ natVal (Proxy :: Proxy n)
    basicUnsafeSlice i m v = v --  error $ "Cannot slice vector i="++show i++" m="++show m
    basicOverlaps v1 v2 = sameMutableByteArray (marr v1) (marr v2)
    basicUnsafeNew _ = MVectorJust 0 `liftM` newByteArray (n*sizeOf (undefined::a))
        where
            n = fromIntegral $ natVal (Proxy :: Proxy n)

    basicUnsafeRead (MVectorJust off arr) j = readByteArray arr (off+j)
    basicUnsafeWrite (MVectorJust off arr) j x = writeByteArray arr (off+j) x
