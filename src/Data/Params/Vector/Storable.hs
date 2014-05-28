{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PolyKinds #-}

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
import Data.Params.Vector

import Unsafe.Coerce

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

-- u :: Vector (Static 1) (Vector (Static 10) Int)
-- u = VG.singleton $ VG.fromList [1..10] 
-- 
-- u' :: Vector (Static 1) (Vector RunTime Int)
-- u' = withParam3 (_elem._len $ 10) $ VG.singleton $ VG.fromList [1..10] 
-- 
-- u'' :: Vector (Static 1) (Vector RunTime Int)
-- u'' = withParam3 (_elem._len $ 10) $ VG.singleton $ VG.fromList [1..10] 
-- 
-- v' = withParam3 (_len 10) $ VG.fromList [1..10] :: Vector RunTime Int
-- 
-- w :: Vector RunTime (Vector (Static 10) Int)
-- w = withParam3 (_len 1) $ VG.singleton $ VG.fromList [1..10] 
-- 
-- w' :: Vector RunTime (Vector RunTime Int)
-- w' = withParam3 (_len 1)
--    $ withParam3 (_elem._len $ 10)
--    $ VG.singleton $ VG.fromList [1..10] 

---------

-------------------------------------------------------------------------------
-- immutable automatically sized vector

data family Vector (len::Param Nat) elem
mkParams ''Vector

instance (Show elem, VG.Vector (Vector len) elem) => Show (Vector len elem) where
    show v = "fromList "++show (VG.toList v)

instance (Eq elem, VG.Vector (Vector len) elem) => Eq (Vector len elem) where
    a == b = (VG.toList a) == (VG.toList b)

instance (Ord elem, VG.Vector (Vector len) elem) => Ord (Vector len elem) where
    compare a b = compare (VG.toList a) (VG.toList b)

---------------------------------------
-- fixed size 

newtype instance Vector (Static len) elem = Vector (ForeignPtr elem)

instance NFData (Vector (Static len) elem) where
    rnf a = seq a ()

instance 
    ( Storable elem 
    , KnownNat len
    ) => VG.Vector (Vector (Static len)) elem 
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
    peek p = unsafePrimToPrim $ do
        fp <- newForeignPtr_ (castPtr p :: Ptr elem)
        return $ Vector fp

    {-# INLINE poke #-}
    poke p (Vector fq) = unsafePrimToPrim $ do
        withForeignPtr fq $ \q -> 
            Foreign.Marshal.Array.copyArray (castPtr p) q len
        where
            len = intparam (Proxy::Proxy len)

instance 
    ( Storable elem
    , ViewParam Param_len (Vector RunTime elem)
    ) => Storable (Vector RunTime elem)
        where

    {-# INLINE sizeOf #-}
    sizeOf v = len * sizeOf (undefined::elem)
        where
            len = viewParam _len v

    {-# INLINE alignment #-}
    alignment _ = alignment (undefined::elem)

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

newtype instance MVector (Static len) s elem = MVector (ForeignPtr elem)

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

