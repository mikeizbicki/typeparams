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

data family ParamDict (p1::k1) (p2::k2) m1 m2

newtype DummyNewtype a = DummyNewtype a

withParam3 :: 
    ( ReifiableConstraint p2
    ) => ParamDict p1 p2 m1 m2
      -> (p2 m2 => m1) 
      -> m1
withParam3 p = using' (unsafeCoerce DummyNewtype (\x -> unsafeCoerce p))

apWith1Param ::
    ( ReifiableConstraint p2
    ) => ParamDict p1 p2 m1 m2
      -> (p2 m2 => m1 -> n) 
      -> (p2 m2 => m1) 
      -> n
apWith1Param p = flip $ apUsing' 
    (unsafeCoerce DummyNewtype (\x -> unsafeCoerce p))

apWith2Param ::
    ( ReifiableConstraint p2
    , ReifiableConstraint p4
    ) => ParamDict p1 p2 m1 m2
      -> ParamDict p3 p4 m1 m3
      -> ((p2 m2,p4 m3) => m1 -> n)
      -> ((p2 m2,p4 m3) => m1)
      -> n
apWith2Param p1 p2 = flip $ apUsing2 
    (unsafeCoerce DummyNewtype (\x -> unsafeCoerce p1)) 
    (unsafeCoerce DummyNewtype (\x -> unsafeCoerce p2))

-------------------

---

type ViewParam' p m = ViewParam p p m m 

class ViewParam p1 p2 m1 m2 where
    viewParam :: (ParamType p2 -> ParamDict p1 p2 m1 m2) -> m1 -> ParamType p2

type TypeIndex' s t = forall (p1 :: * -> Constraint) (p2 :: * -> Constraint) m2. 
    ParamDict p1 p2 t m2 -> ParamDict (HasGetter p1 s) p2 s m2

class HasGetter loc s t where
    getGetter :: proxy loc -> (t -> a) -> s -> a


type family ParamType (p::k) :: *
type instance ParamType GetParam_len = Int

-- class EndoFunctor param f where
--     efmap :: proxy param -> (GetParam param f -> b) -> f -> SetParam param f b

-- len

class SetParam_len m where
    _len :: Int -> ParamDict GetParam_len GetParam_len m m

instance SetParam_len (Vector len elem) where
    _len = ParamDict_Vector_len

instance 
    ( GetParam_len (Vector len elem) 
--     ) => ViewParam GetParam_len GetParam_len (Vector len elem) (Vector len elem) 
    ) => ViewParam GetParam_len GetParam_len m (Vector len elem) 
        where
    viewParam _ _ = getParam_len (undefined :: Vector len elem)

newtype instance ParamDict GetParam_len GetParam_len (Vector len elem) m2 =
    ParamDict_Vector_len { getParamDict_Vector_len :: Int }

-- elem

instance 
    ( ViewParam p1 p2 elem m2
    , ApplyConstraintTo_elem p1 (Vector len elem)
    ) => ViewParam (ApplyConstraintTo_elem p1) p2 (Vector len elem) m2 
        where
    viewParam _ _ = viewParam (undefined :: ParamType p2 -> ParamDict p1 p2 elem m2) (undefined::elem)
     
class ApplyConstraintTo_elem (p :: * -> Constraint) s 

instance p elem => ApplyConstraintTo_elem p (Vector len elem)

class SetParam_elem m elem | m -> elem where
    _elem :: ParamDict p1 p2 elem m2 -> ParamDict (ApplyConstraintTo_elem p1 m) p2 m m2

instance SetParam_elem (Vector len elem) elem where
    _elem = ParamDict_Vector_elem

newtype instance ParamDict (ApplyConstraintTo_elem p1 (Vector len elem)) p2 (Vector len elem) m2 =
    ParamDict_Vector_elem { getParamDict_Vector_elem :: ParamDict p1 p2 elem m2 }

-- Either 

class TypeIndex_a m a | m -> a where
    _a :: ParamDict p1 p2 a m2 -> ParamDict (Meta_a p1 m) p2 m m2

instance TypeIndex_a (Either a b) a where
    _a = ParamDict_Either_a

class TypeIndex_b m b | m -> b where    
    _b :: ParamDict p1 p2 b m2 -> ParamDict (Meta_b p1 m) p2 m m2

instance TypeIndex_b (Either a b) b where
    _b = ParamDict_Either_b

newtype instance ParamDict (Meta_a p1 (Either a b)) p2 (Either a b) c =
    ParamDict_Either_a { getParamDict_Either_a :: ParamDict p1 p2 a c }

newtype instance ParamDict (Meta_b p1 (Either a b)) p2 (Either a b) c =
    ParamDict_Either_b { getParamDict_Either_b :: ParamDict p1 p2 b c }

class Meta_a (p :: * -> Constraint) e v | v -> e where
    meta_a :: (e -> a) -> v -> a

class Meta_b (p :: * -> Constraint) e v | v -> e where
    meta_b :: (e -> a) -> v -> a

instance Meta_a GetParam_len a (Either a b) where
    meta_a p _ = p (undefined::elem)

instance Meta_b GetParam_len b (Either a b) where
    meta_b p _ = p (undefined::elem)

-------------------

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
-- fixed size 

newtype instance Vector (Static len) elem = Vector (ForeignPtr elem)

mkParams ''Vector

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
    , ViewParam' GetParam_len (Vector RunTime elem)
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

