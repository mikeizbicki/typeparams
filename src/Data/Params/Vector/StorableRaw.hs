{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PolyKinds #-}

module Data.Params.Vector.StorableRaw
    where

import Control.Monad
import Control.Monad.Primitive
import Control.DeepSeq
-- import Data.Primitive
import Data.Primitive.Addr
import Data.Primitive.ByteArray
import Data.Primitive.Types
import GHC.Ptr
import GHC.ForeignPtr
import Foreign.Ptr
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.Storable
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
import Data.Params.Vector

import Unsafe.Coerce

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
-- static size 

newtype instance Vector (Static len) elem = Vector (Ptr elem)

mkParams ''Vector

instance NFData (Vector (Static len) elem) where
    rnf a = seq a ()

instance 
    ( Storable elem 
    , KnownNat len
    ) => VG.Vector (Vector (Static len)) elem 
        where
    
    {-# INLINE basicUnsafeFreeze #-}
    basicUnsafeFreeze (MVector p) = return $ Vector p

    {-# INLINE basicUnsafeThaw #-}
    basicUnsafeThaw (Vector p) = return $ MVector p

    {-# INLINE [2] basicLength #-}
    basicLength _ = intparam (Proxy::Proxy len)

    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice j n v = if n /= intparam (Proxy::Proxy len) || j /= 0
        then error $ "Vector.basicUnsafeSlice not allowed to change size"
        else v

    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeIndexM (Vector p) i = return
                                   . unsafeInlineIO
                                   $ peekElemOff p i

    {-# INLINE basicUnsafeCopy #-}
    basicUnsafeCopy (MVector p) (Vector q)
        = unsafePrimToPrim
        $ Foreign.Marshal.Array.copyArray p q len
        where
            len = intparam (Proxy::Proxy len)

    {-# INLINE elemseq #-}
    elemseq _ = seq

-------------------
-- storable instance allows us to make vectors of vectors

-- class PseudoStorable a where
--     psizeOf :: a -> Int
--     palignment :: a -> Int
--     ppeek :: Ptr a -> IO a
--     ppoke :: Ptr a -> a -> IO ()

instance 
    ( Storable elem
    , KnownNat len
    ) => Storable (Vector (Static len) elem)
        where

    {-# INLINE sizeOf #-}
    sizeOf _ = len * sizeOf (undefined::elem)
        where
            len = intparam (Proxy::Proxy len)

    {-# INLINE alignment #-}
    alignment _ = alignment (undefined::elem)

    {-# INLINE peek #-}
    peek p = return $ Vector $ castPtr p

    {-# INLINE poke #-}
    poke p (Vector q) = unsafePrimToPrim
        $ Foreign.Marshal.Array.copyArray (castPtr p) q len
        where
            len = intparam (Proxy::Proxy len)

-- instance 
--     ( Storable elem
--     , ViewParam' GetParam_len (Vector RunTime elem)
--     ) => Storable (Vector RunTime elem)
--         where
-- 
--     {-# INLINE sizeOf #-}
--     sizeOf v = len * sizeOf (undefined::elem)
--         where
--             len = viewParam _len v
-- 
--     {-# INLINE alignment #-}
--     alignment _ = alignment (undefined::elem)
-- 
--     {-# INLINE peek #-}
--     peek p = unsafePrimToPrim $ do
--         fp <- newForeignPtr_ (castPtr p :: Ptr elem)
--         return $ Vector_RunTime fp
-- 
--     {-# INLINE poke #-}
--     poke p (Vector fq) = unsafePrimToPrim $ do
--         withForeignPtr fq $ \q -> 
--             Foreign.Marshal.Array.copyArray (castPtr p) q len
--         where
--             len = viewParam _len (undefined::Vector RunTime elem)
-- --             len = intparam (Proxy::Proxy len)

-------------------------------------------------------------------------------
-- mutable vector

type instance VG.Mutable (Vector len) = MVector len

data family MVector (len::Param Nat) s elem

---------------------------------------
-- fixed size

newtype instance MVector (Static len) s elem = MVector (Ptr elem)

instance 
    ( Storable elem
    , KnownNat len
    ) => VGM.MVector (MVector (Static len)) elem 
        where

    {-# INLINE basicLength #-}
    basicLength _ = intparam (Proxy::Proxy len) 
    
    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice i m v = if m /= intparam (Proxy::Proxy len)
        then error $ "MVector.basicUnsafeSlice not allowed to change size; i="++show i++"; m="++show m++"; len="++show (intparam (Proxy::Proxy len))
        else v
 
    {-# INLINE basicOverlaps #-}
    basicOverlaps (MVector p) (MVector q)
        = between p q (q `advancePtr` len) || between q p (p `advancePtr` len)
        where
            between x y z = x >= y && x < z
            len = intparam (Proxy::Proxy len)

    {-# INLINE basicUnsafeNew #-}
    basicUnsafeNew n = unsafePrimToPrim $ do
        p <- mallocBytes $ len * sizeOf (undefined::elem)
        return $ MVector p
        where
            len = intparam (Proxy::Proxy len)

    {-# INLINE basicUnsafeRead #-}
    basicUnsafeRead (MVector p) i = unsafePrimToPrim
        $ peekElemOff p i

    {-# INLINE basicUnsafeWrite #-}
    basicUnsafeWrite (MVector p) i x = unsafePrimToPrim
        $ pokeElemOff p i x

    {-# INLINE basicUnsafeCopy #-}
    basicUnsafeCopy (MVector p) (MVector q) = unsafePrimToPrim
        $ Foreign.Marshal.Array.copyArray p q len
        where
            len = intparam (Proxy::Proxy len)
                    
    {-# INLINE basicUnsafeMove #-}
    basicUnsafeMove (MVector p) (MVector q) = unsafePrimToPrim
        $ moveArray p q len
        where
            len = intparam (Proxy::Proxy len)

--     {-# INLINE basicSet #-}
--     basicSet (MVector i arr) x = setByteArray arr i (intparam(Proxy::Proxy len)) x
    
---------------------------------------
-- variable size

-- newtype instance MVector Automatic s elem = MVector_Automatic (VPM.MVector s elem)
-- mkParams ''MVector
-- 
-- instance Prim elem => VGM.MVector (MVector Automatic) elem where
-- 
--     {-# INLINE basicLength #-}
--     basicLength (MVector_Automatic v) = VGM.basicLength v
-- 
--     {-# INLINE basicUnsafeSlice #-}
--     basicUnsafeSlice i m (MVector_Automatic v) = MVector_Automatic $ VGM.basicUnsafeSlice i m v
-- 
--     {-# INLINE basicOverlaps #-}
--     basicOverlaps (MVector_Automatic v1) (MVector_Automatic v2) = VGM.basicOverlaps v1 v2
-- 
--     {-# INLINE basicUnsafeNew #-}
--     basicUnsafeNew i = MVector_Automatic `liftM` VGM.basicUnsafeNew i
-- 
--     {-# INLINE basicUnsafeRead #-}
--     basicUnsafeRead (MVector_Automatic v) i = VGM.basicUnsafeRead v i
-- 
--     {-# INLINE basicUnsafeWrite #-}
--     basicUnsafeWrite (MVector_Automatic v) i x = VGM.basicUnsafeWrite v i x
-- 
--     {-# INLINE basicUnsafeCopy #-}
--     basicUnsafeCopy (MVector_Automatic v1) (MVector_Automatic v2) = VGM.basicUnsafeCopy v1 v2
-- 
--     {-# INLINE basicUnsafeMove #-}
--     basicUnsafeMove (MVector_Automatic v1) (MVector_Automatic v2) = VGM.basicUnsafeMove v1 v2
-- 
--     {-# INLINE basicSet #-}
--     basicSet (MVector_Automatic v) x = VGM.basicSet v x


