{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
-- {-# LANGUAGE OverlappingInstances #-}
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
-- Static size 

data instance Vector (Static len) elem = Vector 
    {-#UNPACK#-}!Int 
    {-#UNPACK#-}!ByteArray
mkParams ''Vector

u :: Vector (Static 1) (Vector (Static 10) Int)
u = VG.singleton $ VG.fromList [1..10] 

u' :: Vector (Static 1) (Vector RunTime Int)
u' = withParam2 (elem2.len2 $ 10) $ VG.singleton $ VG.fromList [1..10] 

u'' :: Vector (Static 1) (Vector RunTime Int)
u'' = withParam3' (elem2.len2) 10 $ VG.singleton $ VG.fromList [1..10] 

v' = withParam3 (len2 10) $ VG.fromList [1..10] :: Vector RunTime Int
-- v'' = withParam4 len3 10 $ VG.fromList [1..10] :: Vector RunTime Int

w :: Vector RunTime (Vector (Static 10) Int)
w = withParam3 (len2 1) $ VG.singleton $ VG.fromList [1..10] 

w' :: Vector RunTime (Vector RunTime Int)
w' = withParam3' len2 1 
   $ withParam3' (elem2.len2) 10 
   $ VG.singleton $ VG.fromList [1..10] 

-- u'' :: Vector (Static 1) (Vector Automatic Int)
-- u'' = VG.singleton $ VG.fromList [1..10]

class Meta_elem (p :: * -> Constraint) e v | v -> e where
    meta_elem :: (e -> a) -> v -> a

instance Meta_elem p elem (Vector len elem) where
    meta_elem p _ = p (undefined::elem)

class Meta_a (p :: * -> Constraint) e v | v -> e where
    meta_a :: (e -> a) -> v -> a

class Meta_b (p :: * -> Constraint) e v | v -> e where
    meta_b :: (e -> a) -> v -> a

instance Meta_a Param_len a (Either a b) where
    meta_a p _ = p (undefined::elem)

instance Meta_b Param_len b (Either a b) where
    meta_b p _ = p (undefined::elem)

-------------------

class ValidParam param datatype where
    viewParam :: ValidParamIndex param -> datatype -> ParamType param

type family ParamType (p::k) :: *
type instance ParamType ParamIndex_len = Int
type instance ParamType (ParamIndex_elem p) = ParamType p
type instance ParamType Param_len = Int

type ValidParamIndex param = ParamType param -> param
type family Index2Param p :: * -> Constraint
type instance Index2Param ParamIndex_len = Param_len
type instance Index2Param (ParamIndex_elem p) = Param_elem (Index2Param p)

type family ParamApply p  :: *
type instance ParamApply ParamIndex_len = Vector RunTime Int

data ParamIndex_len = ParamIndex_len Int
len3 = ParamIndex_len

instance KnownNat n => ValidParam ParamIndex_len (Vector (Static n) elem) where
    viewParam _ _ = fromIntegral $ natVal (Proxy::Proxy n)

data ParamIndex_elem p = ParamIndex_elem p
elem3 = ParamIndex_elem
class Param_elem (p :: * -> Constraint) m
instance p elem => Param_elem p (Vector len elem)

instance ReifiableConstraint p => ReifiableConstraint (Param_elem p) 

instance ValidParam p elem => ValidParam (ParamIndex_elem p) (Vector len elem) where
    viewParam _ _ = viewParam (undefined::ParamType p -> p) (undefined::elem)

withParam4 :: 
    ( ReifiableConstraint (Index2Param p) 
    ) => ValidParamIndex p -> ParamType p -> (Index2Param p (ParamApply p) => b) -> b
withParam4 pi pt = using' (unsafeCoerce (pi pt))

apWithParam4 :: 
    ( ReifiableConstraint (Index2Param p) 
    ) => ValidParamIndex p -> ParamType p -> (Index2Param p a => b -> c) -> (Index2Param p a => b) -> c
apWithParam4 pi pt = flip $ apUsing' (unsafeCoerce (pi pt))

-- withParam3 p = using' (reifiableConstraint p (\x -> getParamDict p))

-- class WithParam4 p where
--     withParam4 :: Def p a -> ((p a) => b) -> b

-- instance WithParam4 ParamIndex_len where
--     withParam4 = unsafeCoerce using

---------

data family ParamDict (p1::k1) (p2::k2) m1 m2

class ValidDictionary (p1::k1) (p2:: * -> Constraint) m1 m2 where
    getParamDict :: ParamDict p1 p2 m1 m2 -> ParamType p2
    reifiableConstraint :: ParamDict p1 p2 m1 m2 -> (a -> ParamType p2) -> Def p2 a

-- viewParam3 ::
--     (
--     ) => (ParamType p2 -> ParamDict p1 p2 m1 m2) -> m1 -> ParamType p2
-- viewParam3 = undefined

withParam3 :: 
    ( ValidDictionary p1 p2 m1 m2 
    , ReifiableConstraint p2
    ) => ParamDict p1 p2 m1 m2 -> (p2 m2 => m1) -> m1
withParam3 p = using' (reifiableConstraint p (\x -> getParamDict p))

withParam3' :: 
--     ( ValidDictionary p1 p2 m1 m2 
    ( ReifiableConstraint p2
    ) => (ParamType p2 -> ParamDict p1 p2 m1 m2) -> ParamType p2 -> (p2 m2 => m1) -> m1
-- withParam3' p pt = using' (reifiableConstraint (p pt) (\x -> getParamDict (p pt)))
withParam3' pi pt = using' (unsafeCoerce FIXME (\x -> unsafeCoerce (pi pt)))

apWithParam3 ::
    ( ValidDictionary p1 p2 m1 m2 
    , ReifiableConstraint p2
    ) => ParamDict p1 p2 m1 m2 -> (p2 m2 => m1 -> n) -> (p2 m2 => m1) -> n
apWithParam3 p = flip $ apUsing' (reifiableConstraint p (\x -> getParamDict p))

data FIXME a = FIXME a
apWithParam3' ::
--     ( ValidDictionary p1 p2 m1 m2 
    ( ReifiableConstraint p2
    ) => (ParamType p2 -> ParamDict p1 p2 m1 m2) 
      -> ParamType p2 
      -> (p2 m2 => m1 -> n) 
      -> (p2 m2 => m1) 
      -> n
apWithParam3' p pt = flip $ apUsing' ( unsafeCoerce FIXME (\x -> unsafeCoerce (p pt)))
-- apWithParam3' pi pt = flip $ apUsing' (unsafeCoerce (\a -> pi pt))

apWith2Param ::
    ( ReifiableConstraint p2
    , ReifiableConstraint p3
    ) => (ParamDict p1 p2 m1 m2)
      -> (ParamDict p3 p4 m1 m3)
      -> ((p2 m2,p4 m3) => m1 -> n)
      -> ((p2 m2,p4 m3) => m1)
      -> n
apWith2Param = undefined

class ViewParam p1 p2 m1 m2 where
    viewParam3 :: (ParamType p2 -> ParamDict p1 p2 m1 m2) -> m2 -> ParamType p2

instance Param_len m2 => ViewParam Param_len Param_len (Vector len elem) m2 where
-- instance Param_len (Vector len elem) => ViewParam Param_len Param_len (Vector len elem) (Vector len elem) where
    viewParam3 _ = param_len

instance 
    ( ViewParam p1 p2 elem m2
    ) => ViewParam (Meta_elem p1 (Vector len elem)) p2 (Vector len elem) m2 
        where
    viewParam3 _ = viewParam3 (undefined :: ParamType p2 -> ParamDict p1 p2 elem m2)
     
-- instance 
--     ( ValidDictionary p1 p2 (Vector len elem) m2
--     ) => ValidDictionary (Meta_elem p1 (Vector len elem)) p2 (Vector len elem) m2 
--         where

-- len

class BuildParamDict_len m elem | m -> elem where
    len2 :: Int -> ParamDict Param_len Param_len m m

instance BuildParamDict_len (Vector len elem) (Vector len elem) where
    len2 = ParamDict_Vector_len

instance ValidDictionary Param_len Param_len (Vector RunTime elem) m2 where
    getParamDict = getParamDict_Vector_len
    reifiableConstraint p = Def_Param_len

-- newtype instance ParamDict Param_len Param_len (Vector RunTime elem) m2 =
newtype instance ParamDict Param_len Param_len (Vector len elem) m2 =
    ParamDict_Vector_len { getParamDict_Vector_len :: Int }

-- elem

class BuildParamDict_elem m elem | m -> elem where
    elem2 :: ()--WithParam2 p1 p2 elem m2 
         => ParamDict p1 p2 elem m2 
         -> ParamDict (Meta_elem p1 m) p2 m m2 

instance BuildParamDict_elem (Vector len elem) elem where
    elem2 = ParamDict_Vector_elem

instance 
    ( ValidDictionary p1 p1 elem elem
    ) => ValidDictionary p1 p1 (Vector len elem) elem
        where
    getParamDict = unsafeCoerce (getParamDict :: ParamDict p1 p1 elem elem -> ParamType p1)
    reifiableConstraint = unsafeCoerce (reifiableConstraint :: ParamDict p1 p1 elem elem -> (a -> ParamType p1) -> Def p1 a) 

instance 
    ( ValidDictionary p1 p2 (Vector len elem) m2
    ) => ValidDictionary (Meta_elem p1 (Vector len elem)) p2 (Vector len elem) m2 
        where
    getParamDict = unsafeCoerce getParamDict_Vector_elem
    reifiableConstraint = unsafeCoerce (reifiableConstraint :: ParamDict p1 p2 (Vector len elem) m2 -> (a -> ParamType p2) -> Def p2 a)

newtype instance ParamDict (Meta_elem p1 (Vector len elem)) p2 (Vector len elem) m2 =
    ParamDict_Vector_elem { getParamDict_Vector_elem :: ParamDict p1 p2 elem m2 }

-- Either 

newtype instance ParamDict (Meta_b p1 (Either a b)) p2 (Either a b) c =
    ParamDict_Either_b { getParamDict_Either_b :: ParamDict p1 p2 b c }

newtype instance ParamDict (Meta_a p1 (Either a b)) p2 (Either a b) c =
    ParamDict_Either_a { getParamDict_Either_a :: ParamDict p1 p2 a c }

-------------------
-- param2

class WithParam2 p1 p2 m1 m2 where
    withParam2 :: ParamDict p1 p2 m1 m2 -> (p2 m2 => m1) -> m1
    apWithParam2 :: ParamDict p1 p2 m1 m2 -> (p2 m2 => m1 -> n) -> (p2 m2 => m1) -> n

instance WithParam2 Param_len Param_len (Vector RunTime elem) m2 where
    withParam2 p = using' (Def_Param_len (\x -> getParamDict_Vector_len p)) 
    apWithParam2 p = flip (apUsing' (Def_Param_len (\x -> getParamDict_Vector_len p)))

instance 
    ( WithParam2 p1 p2 elem m2
    ) => WithParam2 (Meta_elem p1 (Vector len elem)) p2 (Vector len elem) m2
        where
    withParam2 (ParamDict_Vector_elem p) = unsafeCoerce $ withParam2 p
    apWithParam2 (ParamDict_Vector_elem p) = unsafeCoerce $ apWithParam2 p

instance
    ( WithParam2 p1 p2 b c
    ) => WithParam2 (Meta_b p1 (Either a b)) p2 (Either a b) c 
        where
    withParam2 (ParamDict_Either_b p) = unsafeCoerce $ withParam2 p
    apWithParam2 (ParamDict_Either_b p) = unsafeCoerce $ apWithParam2 p

instance
    ( WithParam2 p1 p2 a c
    ) => WithParam2 (Meta_a p1 (Either a b)) p2 (Either a b) c 
        where
    withParam2 (ParamDict_Either_a p) = unsafeCoerce $ withParam2 p
    apWithParam2 (ParamDict_Either_a p) = unsafeCoerce $ apWithParam2 p

-------------------

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

instance Prim elem => SetParam Param_len (Vector RunTime elem) (Vector Automatic elem) where
    setParam p v = VG.fromList $ apWithParam p VG.toList v 

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

instance Prim elem => Param_len (Vector Automatic elem) where
    param_len v = VG.length v

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
