{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverlappingInstances #-}

-- | Efficient vectors requiring a storable instance for the elements.
-- These vectors can be considerably faster than unboxed vectors in 
-- some cases.
-- This is because a storable vector of fixed size need only keep track
-- of a single pointer; everything else is known at compile time.
-- This lets us move some variables from memory into the assembly
-- instructions.
-- Tricks like this are not available in c code and can be very important.
-- We store less memory, use fewer registers, run fewer assembly instructions,
-- and have fewer cache misses.
-- In short, this data type is awesome.
-- 
-- A 'Storable' instance lets us create vectors of vectors; however,
-- I'm not 100% convinced that it is correct with respect to memory leaks.

module Data.Params.Vector.Storable
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
import Foreign.ForeignPtr hiding (unsafeForeignPtrToPtr)
import Foreign.ForeignPtr.Unsafe
import Foreign.Marshal.Array
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

-------------------------------------------------------------------------------
-- taken from Data.Vector.Storable.Internal

{-# INLINE getPtr #-}
getPtr :: ForeignPtr a -> Ptr a
getPtr (ForeignPtr addr _) = Ptr addr

{-# INLINE mallocVector #-}
mallocVector :: Storable a => Int -> IO (ForeignPtr a)
mallocVector =
#if __GLASGOW_HASKELL__ >= 605
    doMalloc undefined
        where
          doMalloc :: Storable b => b -> Int -> IO (ForeignPtr b)
          doMalloc dummy size = mallocPlainForeignPtrBytes (size * Foreign.Storable.sizeOf dummy)
#else
    mallocForeignPtrArray
#endif

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

newtype instance Vector (Just len) elem = Vector (ForeignPtr elem)

mkParams ''Vector

instance NFData (Vector (Just len) elem) where
    rnf a = seq a ()

instance 
    ( Storable elem 
    , KnownNat len
    ) => VG.Vector (Vector (Just len)) elem 
        where
    
    {-# INLINE basicUnsafeFreeze #-}
    basicUnsafeFreeze (MVector fp) = return $ Vector fp

    {-# INLINE basicUnsafeThaw #-}
    basicUnsafeThaw (Vector fp) = return $ MVector fp

    {-# INLINE [2] basicLength #-}
    basicLength _ = intparam (Proxy::Proxy len)

    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice j n v = if n /= intparam (Proxy::Proxy len) || j /= 0
        then error $ "Vector.basicUnsafeSlice not allowed to change size"
        else v

    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeIndexM (Vector fp) i = return
                                    . unsafeInlineIO
                                    $ withForeignPtr fp $ \p ->
                                      peekElemOff p i

    {-# INLINE basicUnsafeCopy #-}
    basicUnsafeCopy (MVector fp) (Vector fq)
        = unsafePrimToPrim
        $ withForeignPtr fp $ \p ->
          withForeignPtr fq $ \q ->
          Foreign.Marshal.Array.copyArray p q len
        where
            len = intparam (Proxy::Proxy len)

    {-# INLINE elemseq #-}
    elemseq _ = seq

-------------------
-- storable instance allows us to make vectors of vectors

instance 
    ( Storable elem
    , KnownNat len
    ) => Storable (Vector (Just len) elem)
        where

    sizeOf _ = len * sizeOf (undefined::elem)
        where
            len = intparam (Proxy::Proxy len)

    alignment _ = alignment (undefined::elem)

    peek p = unsafePrimToPrim $ do
        fp <- newForeignPtr_ (castPtr p :: Ptr elem)
        return $ Vector fp

    poke p (Vector fq) = unsafePrimToPrim $ do
        withForeignPtr fq $ \q -> 
            Foreign.Marshal.Array.copyArray (castPtr p) q len
        where
            len = intparam (Proxy::Proxy len)

-------------------------------------------------------------------------------
-- mutable vector

type instance VG.Mutable (Vector len) = MVector len

data family MVector (len::Maybe Nat) s elem

---------------------------------------
-- fixed size

newtype instance MVector (Just len) s elem = MVector (ForeignPtr elem)

instance 
    ( Storable elem
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
    basicOverlaps (MVector fp) (MVector fq)
        = between p q (q `advancePtr` len) || between q p (p `advancePtr` len)
        where
            between x y z = x >= y && x < z
            p = getPtr fp
            q = getPtr fq
            len = intparam (Proxy::Proxy len)

    {-# INLINE basicUnsafeNew #-}
    basicUnsafeNew n = unsafePrimToPrim $ do
        fp <- mallocVector len
        return $ MVector fp
        where
            len = intparam (Proxy::Proxy len)

    {-# INLINE basicUnsafeRead #-}
    basicUnsafeRead (MVector fp) i = unsafePrimToPrim
        $ withForeignPtr fp (`peekElemOff` i)

    {-# INLINE basicUnsafeWrite #-}
    basicUnsafeWrite (MVector fp) i x = unsafePrimToPrim
        $ withForeignPtr fp $ \p -> pokeElemOff p i x

    {-# INLINE basicUnsafeCopy #-}
    basicUnsafeCopy (MVector fp) (MVector fq) = unsafePrimToPrim
        $ withForeignPtr fp $ \p ->
          withForeignPtr fq $ \q ->
          Foreign.Marshal.Array.copyArray p q len
        where
            len = intparam (Proxy::Proxy len)
                    
    {-# INLINE basicUnsafeMove #-}
    basicUnsafeMove (MVector fp) (MVector fq) = unsafePrimToPrim
        $ withForeignPtr fp $ \p ->
          withForeignPtr fq $ \q ->
          moveArray p q len
        where
            len = intparam (Proxy::Proxy len)

--     {-# INLINE basicSet #-}
--     basicSet (MVector i arr) x = setByteArray arr i (intparam(Proxy::Proxy len)) x
    
---------------------------------------
-- variable size

-- newtype instance MVector Nothing s elem = MVector_Nothing (VPM.MVector s elem)
-- mkParams ''MVector
-- 
-- instance Prim elem => VGM.MVector (MVector Nothing) elem where
-- 
--     {-# INLINE basicLength #-}
--     basicLength (MVector_Nothing v) = VGM.basicLength v
-- 
--     {-# INLINE basicUnsafeSlice #-}
--     basicUnsafeSlice i m (MVector_Nothing v) = MVector_Nothing $ VGM.basicUnsafeSlice i m v
-- 
--     {-# INLINE basicOverlaps #-}
--     basicOverlaps (MVector_Nothing v1) (MVector_Nothing v2) = VGM.basicOverlaps v1 v2
-- 
--     {-# INLINE basicUnsafeNew #-}
--     basicUnsafeNew i = MVector_Nothing `liftM` VGM.basicUnsafeNew i
-- 
--     {-# INLINE basicUnsafeRead #-}
--     basicUnsafeRead (MVector_Nothing v) i = VGM.basicUnsafeRead v i
-- 
--     {-# INLINE basicUnsafeWrite #-}
--     basicUnsafeWrite (MVector_Nothing v) i x = VGM.basicUnsafeWrite v i x
-- 
--     {-# INLINE basicUnsafeCopy #-}
--     basicUnsafeCopy (MVector_Nothing v1) (MVector_Nothing v2) = VGM.basicUnsafeCopy v1 v2
-- 
--     {-# INLINE basicUnsafeMove #-}
--     basicUnsafeMove (MVector_Nothing v1) (MVector_Nothing v2) = VGM.basicUnsafeMove v1 v2
-- 
--     {-# INLINE basicSet #-}
--     basicSet (MVector_Nothing v) x = VGM.basicSet v x

