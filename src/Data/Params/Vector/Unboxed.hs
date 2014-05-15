{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE CPP #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-}
-- {-# LANGUAGE OverlappingInstances #-}
module Data.Params.Vector.Unboxed
    where

import Control.Monad
import Control.Monad.Primitive
import Control.DeepSeq
-- import Data.Primitive
import Data.Primitive.ByteArray
-- import Data.Primitive.Types
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
import Data.Params.PseudoPrim
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
    {-#UNPACK#-}!(PseudoPrimInfo elem)
    {-#UNPACK#-}!ByteArray
mkParams ''Vector

u :: Vector (Static 1) (Vector (Static 10) Int)
u = VG.singleton $ VG.fromList [1..10] 

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

instance NFData (Vector (Static len) elem) where
    rnf a = seq a ()

instance 
    ( PseudoPrim elem 
    , KnownNat len
    ) => VG.Vector (Vector (Static len)) elem 
        where
    
    {-# INLINE basicUnsafeFreeze #-}
    basicUnsafeFreeze (MVector i ppi marr) = Vector i ppi `liftM` unsafeFreezeByteArray marr

    {-# INLINE basicUnsafeThaw #-}
    basicUnsafeThaw (Vector i ppi arr) = MVector i ppi `liftM` unsafeThawByteArray arr

    {-# INLINE [2] basicLength #-}
    basicLength _ = getParam_len (undefined::Vector (Static len) elem) 

    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice j n v = if n /= getParam_len (undefined::Vector (Static len) elem) || j /= 0
        then error $ "Vector.basicUnsafeSlice not allowed to change size"
        else v 

    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeIndexM (Vector i ppi arr) j = return $! pp_indexByteArray ppi arr (i+j)

--     {-# INLINE basicUnsafeCopy #-}
--     basicUnsafeCopy (MVector i ppi1 dst) (Vector j ppi2src) = 
--         copyByteArray dst (i*sz) src (j*sz) (len*sz)
--         where
--             sz = pp_sizeOf (undefined :: elem)
--             len = getParam_len (undefined::Vector (Static len) elem)

    {-# INLINE elemseq #-}
    elemseq _ = seq

-------------------
-- primitive instance allows unboxing unboxed vectors

unInt :: Int -> Int#
unInt (I# i) = i

instance
    ( PseudoPrim elem
    , KnownNat len
    , Show elem
    ) => PseudoPrim (Vector (Static len) elem)
        where
    
    newtype PseudoPrimInfo (Vector (Static len) elem) = 
        PseudoPrimInfo_VectorStatic (PseudoPrimInfo elem)

    {-# INLINE pp_sizeOf# #-}
    pp_sizeOf# (PseudoPrimInfo_VectorStatic ppi) = 
        unInt (pp_sizeOf ppi * (intparam (Proxy::Proxy len)))

    {-# INLINE pp_alignment# #-}
    pp_alignment# (PseudoPrimInfo_VectorStatic ppi) = 
        unInt (pp_alignment ppi)
--         unInt (pp_sizeOf ppi * (intparam (Proxy::Proxy len)))

    {-# INLINE pp_indexByteArray# #-}
    pp_indexByteArray# (PseudoPrimInfo_VectorStatic ppi) arr# i# = 
        Vector ((I# i#)*(intparam (Proxy::Proxy len))) ppi (ByteArray arr#)

    {-# INLINE pp_readByteArray# #-}
    pp_readByteArray# (PseudoPrimInfo_VectorStatic ppi) marr# i# s# = 
        (# s#, Vector (I# i#) ppi (ByteArray (unsafeCoerce# marr#)) #)

    {-# INLINE pp_writeByteArray# #-}
    pp_writeByteArray# (PseudoPrimInfo_VectorStatic ppi) marr# i# x s# = go 0 s#
        where
            go i s = ( if i >= intparam (Proxy::Proxy len)
                then s
                else go (i+1) 
                        (pp_writeByteArray# ppi marr# 
                            (i# *# (unInt ( intparam (Proxy::Proxy len))) +# (unInt i)) 
                            (x VG.! i)
                            s
                        )
                    )
                where 
                    iii = I# (i# *# (pp_sizeOf# ppi) +# (unInt i)) 

    {-# INLINE seqInfo #-}
    seqInfo _ = seqInfo (undefined::elem)

    {-# INLINE emptyInfo #-}
    emptyInfo = PseudoPrimInfo_VectorStatic emptyInfo 

----------------------------------------
-- RunTime size 

data instance Vector RunTime elem = Vector_RunTime 
    {-#UNPACK#-}!Int 
    {-#UNPACK#-}!(PseudoPrimInfo elem)
    {-#UNPACK#-}!ByteArray

instance NFData (Vector RunTime elem) where
    rnf a = seq a ()


type family GetParam (p :: k1) (s :: k2) :: k3
type instance GetParam SetParam_elem (Vector len elem) = elem
type instance GetParam SetParam_len (Vector RunTime elem) = RunTime
type instance GetParam SetParam_len (Vector len elem) = len

type family SetParam (p :: k1) (s :: k2) (t :: k3) :: *
type instance SetParam SetParam_elem (Vector len elem) elem' = Vector len elem'
type instance SetParam SetParam_len (Vector len elem) len' = Vector len' elem

class {- GetParam loc t ~ RunTime => -} SetConfig loc t t' where -- | loc t s -> t' where
--     setConfig :: proxy loc -> t -> SetParam loc t Automatic
    setConfig :: proxy loc -> t -> t'

-- instance 
--     ( ViewParam' GetParam_len (Vector RunTime elem)
--     , PseudoPrim elem
--     ) => SetConfig SetParam_len (Vector RunTime elem) (Vector Automatic elem)
--         where
--     setConfig _ v = VG.fromList $ VG.toList v
-- 
-- manageParam_len :: PseudoPrim elem => Int -> Vector RunTime elem -> Vector Automatic elem
-- manageParam_len n v = apWith1Param (_len n) (setConfig (Proxy :: Proxy SetParam_len)) v
-- 
-- v5 = manageParam_len 5 v' -- :: Vector (Automatic s0) Int
-- v6 = manageParam_len 6 v' -- :: Vector (Automatic s1) Int
-- vv = VG.zipWith (+) v5 v6

-- vv :: forall (s0::Nat) (s1::Nat). Vector (Automatic s0) Int
-- vv = VG.zipWith (+) v5 v6
--     where
--         v5 = manageParam_len 5 v' :: Vector (Automatic s0) Int
--         v6 = manageParam_len 6 v' :: Vector (Automatic s1) Int

-- instance 
--     ( PseudoPrim elem 
--     ) => SetParam 
--             GetParam_len 
--             (Vector RunTime elem) 
--             (Vector (Automatic 1) elem) 
--         where
--     setParam p v = VG.fromList $ apWithParam p VG.toList v 

instance 
    ( PseudoPrim elem 
--     , GetParam_len (Vector RunTime elem)
    , ViewParam' GetParam_len (Vector RunTime elem)
    ) => VG.Vector (Vector RunTime) elem 
        where
    
    {-# INLINE basicUnsafeFreeze #-}
--     basicUnsafeFreeze (MVector_RunTime len i marr) = if len==getParam_len (undefined::Vector RunTime elem)
    basicUnsafeFreeze (MVector_RunTime len i ppi marr) = 
        if len == viewParam _len (undefined::Vector RunTime elem)
            then Vector_RunTime i ppi `liftM` unsafeFreezeByteArray marr
            else error $ "basicUnsafeFreeze cannot change RunTime vector size"
                ++ "; len="++show len
                ++ "; getParam_len="++show (viewParam _len (undefined::Vector RunTime elem))

    {-# INLINE basicUnsafeThaw #-}
    basicUnsafeThaw (Vector_RunTime i ppi arr) = 
        MVector_RunTime (viewParam _len (undefined::Vector RunTime elem)) i ppi `liftM` unsafeThawByteArray arr
--         MVector_RunTime (getParam_len (undefined::Vector RunTime elem)) i `liftM` unsafeThawByteArray arr

    {-# INLINE [2] basicLength #-}
    basicLength _ = viewParam _len (undefined::Vector RunTime elem) 
--     basicLength _ = getParam_len (undefined::Vector RunTime elem) 

    {-# INLINE basicUnsafeSlice #-}
--     basicUnsafeSlice j n v = if n /= getParam_len (undefined::Vector RunTime elem) || j /= 0
    basicUnsafeSlice j n v = 
        if n /= viewParam _len (undefined::Vector RunTime elem) || j /= 0
            then error $ "Vector_RunTime.basicUnsafeSlice not allowed to change size"
            else v 

    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeIndexM (Vector_RunTime i ppi arr) j = return $! pp_indexByteArray ppi arr (i+j)

--     {-# INLINE basicUnsafeCopy #-}
--     basicUnsafeCopy (MVector_RunTime n i dst) (Vector_RunTime j src) = if n==len
--         then copyByteArray dst (i*sz) src (j*sz) (len*sz)
--         else error "basicUnsafeCopy cannot change RunTime vector size"
--         where
--             sz = pp_sizeOf (undefined :: elem)
-- --             len = getParam_len (undefined::Vector RunTime elem)
--             len = viewParam _len (undefined::Vector RunTime elem)

    {-# INLINE elemseq #-}
    elemseq _ = seq

-------------------

instance
    ( PseudoPrim elem
    , GetParam_len (Vector RunTime elem)
    ) => PseudoPrim (Vector RunTime elem)
        where

    newtype PseudoPrimInfo (Vector RunTime elem) = 
        PseudoPrimInfo_VectorRunTime (PseudoPrimInfo elem)
    
    {-# INLINE pp_sizeOf# #-}
    pp_sizeOf# (PseudoPrimInfo_VectorRunTime ppi) = 
        unInt (pp_sizeOf ppi * (getParam_len (undefined::Vector RunTime elem)))

    {-# INLINE pp_alignment# #-}
    pp_alignment# (PseudoPrimInfo_VectorRunTime ppi) = 
        unInt (pp_alignment ppi)
--         unInt (pp_sizeOf (undefined::elem) * (getParam_len (undefined::Vector RunTime elem)))

    {-# INLINE pp_indexByteArray# #-}
    pp_indexByteArray# (PseudoPrimInfo_VectorRunTime ppi)arr# i# = 
        Vector_RunTime ((I# i#)*(getParam_len (undefined::Vector RunTime elem))) ppi (ByteArray arr#)

    {-# INLINE pp_readByteArray# #-}
    pp_readByteArray# (PseudoPrimInfo_VectorRunTime ppi) marr# i# s# = 
        (# s#, Vector_RunTime (I# i#) ppi (ByteArray (unsafeCoerce# marr#)) #)

    {-# INLINE pp_writeByteArray# #-}
    pp_writeByteArray# (PseudoPrimInfo_VectorRunTime ppi) marr# i# x s# = go 0 s#
        where
            go i s = ( if i >= len
                then s
                else go (i+1) 
                        (pp_writeByteArray# ppi marr# 
                            (i# *# (unInt len) +# (unInt i)) 
                            (x VG.! i)
                            s
                        )
                    )
                where 
                    len = getParam_len (undefined::Vector RunTime elem)
                    iii = I# (i# *# (pp_sizeOf# ppi) +# (unInt i)) 

    {-# INLINE seqInfo #-}
    seqInfo _ = seqInfo (undefined::elem)

    {-# INLINE emptyInfo #-}
    emptyInfo = PseudoPrimInfo_VectorRunTime emptyInfo

---------------------------------------
-- Automatic sized

data instance Vector Automatic elem = Vector_Automatic
    {-#UNPACK#-}!Int 
    {-#UNPACK#-}!Int 
    {-#UNPACK#-}!(PseudoPrimInfo elem)
    {-#UNPACK#-}!ByteArray

instance PseudoPrim elem => VG.Vector (Vector Automatic) elem where

    {-# INLINE basicUnsafeFreeze #-}
    basicUnsafeFreeze (MVector_Automatic i n ppi marr)
        = Vector_Automatic i n ppi `liftM` unsafeFreezeByteArray marr

    {-# INLINE basicUnsafeThaw #-}
    basicUnsafeThaw (Vector_Automatic i n ppi arr)
      = MVector_Automatic i n ppi `liftM` unsafeThawByteArray arr

    {-# INLINE basicLength #-}
    basicLength (Vector_Automatic _ n _ _) = n

    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice j n (Vector_Automatic i _ ppi arr) = Vector_Automatic (i+j) n ppi arr

    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeIndexM (Vector_Automatic i _ ppi arr) j = return $! pp_indexByteArray ppi arr (i+j)

--     {-# INLINE basicUnsafeCopy #-}
--     basicUnsafeCopy (MVector_Automatic i n dst) (Vector_Automatic j _ src)
--       = copyByteArray dst (i*sz) src (j*sz) (n*sz)
--       where
--         sz = sizeOf (indefinido :: a)

    {-# INLINE elemseq #-}
    elemseq _ = seq

magic :: forall len len2.
    ( KnownNat len
    , KnownNat len2
--     , PseudoPrim elem
    ) => Vector (Static len) (Vector (Static len2) Int)
      -> Vector (Static len) (Vector Automatic Int)
magic (Vector i ppi arr) = Vector i (PseudoPrimInfo_VectorAutomatic j n emptyInfo) arr
    where
        j = fromIntegral $ natVal (Proxy::Proxy len2)
        n = j*pp_sizeOf (emptyInfo::PseudoPrimInfo Int)

instance PseudoPrim elem => PseudoPrim (Vector Automatic elem) where
    data PseudoPrimInfo (Vector Automatic elem) = PseudoPrimInfo_VectorAutomatic
        {-#UNPACK#-}!Int -- ^ length
        {-#UNPACK#-}!Int -- ^ sizeOf
        {-#UNPACK#-}!(PseudoPrimInfo elem)
    
    {-# INLINE pp_sizeOf# #-}
    pp_sizeOf# (PseudoPrimInfo_VectorAutomatic _ s _) = unInt s

    {-# INLINE pp_alignment# #-}
    pp_alignment# (PseudoPrimInfo_VectorAutomatic _ _ ppi) = 
        unInt (pp_alignment ppi)

    {-# INLINE pp_indexByteArray# #-}
    pp_indexByteArray# (PseudoPrimInfo_VectorAutomatic len _ ppi) arr# i# = 
        Vector_Automatic ((I# i#)*len) len ppi (ByteArray arr#)

    {-# INLINE pp_readByteArray# #-}
    pp_readByteArray# (PseudoPrimInfo_VectorAutomatic len _ ppi) marr# i# s# = 
        (# s#, Vector_Automatic (I# i#) len ppi (ByteArray (unsafeCoerce# marr#)) #)

    {-# INLINE pp_writeByteArray# #-}
    pp_writeByteArray# (PseudoPrimInfo_VectorAutomatic len _ ppi) marr# i# x s# = go 0 s#
        where
            go i s = ( if i >= len
                then s
                else go (i+1) 
                        (pp_writeByteArray# ppi marr# 
                            (i# *# (unInt len) +# (unInt i)) 
                            (x VG.! i)
                            s
                        )
                    )
                where 
                    iii = I# (i# *# (pp_sizeOf# ppi) +# (unInt i)) 

    {-# INLINE seqInfo #-}
    seqInfo _ = False

    {-# INLINE emptyInfo #-}
    emptyInfo = error "emptyInfo of PseudoPrimInfo_VectorAutomatic"


-------------------------------------------------------------------------------
-- mutable vector

data family MVector (len::Param Nat) s elem

type instance VG.Mutable (Vector len) = MVector len

---------------------------------------
-- static size

data instance MVector (Static len) s elem = MVector 
    {-#UNPACK#-}!Int 
    {-#UNPACK#-}!(PseudoPrimInfo elem)
    {-#UNPACK#-}!(MutableByteArray s)

instance 
    ( PseudoPrim elem
    , KnownNat len
    ) => VGM.MVector (MVector (Static len)) elem 
        where

    {-# INLINE basicLength #-}
    basicLength _ = fromIntegral $ natVal (Proxy::Proxy len)
    
    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice i m v = if m /= len
        then error $ "MVector.basicUnsafeSlice not allowed to change size"
            ++"; i="++show i
            ++"; m="++show m
            ++"; len="++show len 
        else v
        where
--             len = getParam_len (undefined::MVector (Static len) s elem)
            len = intparam (Proxy::Proxy len)
 
    {-# INLINE basicOverlaps #-}
    basicOverlaps (MVector i ppi1 arr1) (MVector j ppi2 arr2)
        = sameMutableByteArray arr1 arr2
        && (between i j (j+len) || between j i (i+len))
            where
                len = intparam (Proxy::Proxy len)
                between x y z = x >= y && x < z

    {-# INLINE basicUnsafeNew #-}
    basicUnsafeNew n = if seqInfo (undefined::elem)
        then error "basicUnsafeNew: seqInfo"
        else do
            arr <- newPinnedByteArray (len * pp_sizeOf (emptyInfo :: PseudoPrimInfo elem))
            return $ MVector 0 (emptyInfo::PseudoPrimInfo elem) arr
        where
            len = intparam (Proxy::Proxy len)

    {-# INLINE basicUnsafeRead #-}
    basicUnsafeRead (MVector i ppi arr) j = pp_readByteArray ppi arr (i+j)

    {-# INLINE basicUnsafeWrite #-}
    basicUnsafeWrite (MVector i ppi arr) j x = pp_writeByteArray ppi arr (i+j) x

--     {-# INLINE basicUnsafeCopy #-}
--     basicUnsafeCopy (MVector i ppi dst) (MVector j ppi src) = 
--         copyMutableByteArray ppi dst (i*sz) src (j*sz) (len*sz)
--         where
--             sz = pp_sizeOf (undefined :: elem)
--             len = intparam (Proxy::Proxy len)
--                     
--     {-# INLINE basicUnsafeMove #-}
--     basicUnsafeMove (MVector i dst) (MVector j src) = moveByteArray dst (i*sz) src (j*sz) (len * sz)
--         where
--             sz = pp_sizeOf (undefined :: elem)
--             len = intparam (Proxy::Proxy len)
-- 
--     {-# INLINE basicSet #-}
--     basicSet (MVector i arr) x = setByteArray arr i (intparam(Proxy::Proxy len)) x
    

---------------------------------------
-- RunTime size

data instance MVector RunTime s elem = MVector_RunTime
    {-#UNPACK#-}!Int 
    {-#UNPACK#-}!Int 
    {-#UNPACK#-}!(PseudoPrimInfo elem)
    {-#UNPACK#-}!(MutableByteArray s)

instance 
    ( PseudoPrim elem
    ) => VGM.MVector (MVector RunTime) elem 
        where

    {-# INLINE basicLength #-}
    basicLength (MVector_RunTime n _ ppi _) = n
    
    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice i m (MVector_RunTime n j ppi v) = MVector_RunTime m (i+j) ppi v
--     basicUnsafeSlice i m v = if m /= len
--         then error $ "MVector.basicUnsafeSlice not allowed to change size"
--             ++"; i="++show i
--             ++"; m="++show m
--             ++"; len="++show len 
--         else v
--         where
--             len = VGM.length v
 
    {-# INLINE basicOverlaps #-}
    basicOverlaps (MVector_RunTime m i ppi1 arr1) (MVector_RunTime n j ppi2 arr2)
        = sameMutableByteArray arr1 arr2
        && (between i j (j+m) || between j i (i+n))
            where
                between x y z = x >= y && x < z

    {-# INLINE basicUnsafeNew #-}
    basicUnsafeNew n = if seqInfo (undefined::elem)
        then error "basicUnsafeNew: seqInfo"
        else do
            arr <- newPinnedByteArray (n * pp_sizeOf (emptyInfo :: PseudoPrimInfo elem))
            return $ MVector_RunTime n 0 emptyInfo arr

    {-# INLINE basicUnsafeRead #-}
    basicUnsafeRead (MVector_RunTime _ i ppi arr) j = pp_readByteArray ppi arr (i+j)

    {-# INLINE basicUnsafeWrite #-}
    basicUnsafeWrite (MVector_RunTime _ i ppi arr) j x = pp_writeByteArray ppi arr (i+j) x


--     {-# INLINE basicUnsafeCopy #-}
--     basicUnsafeCopy (MVector_RunTime n i dst) (MVector_RunTime m j src) 
--         = if n==m
--             then copyMutableByteArray dst (i*sz) src (j*sz) (n*sz)
--             else error "basicUnsafeCopy cannot change size of RunTime MVector"
--         where
--             sz = pp_sizeOf (undefined :: elem)
--                     
--     {-# INLINE basicUnsafeMove #-}
--     basicUnsafeMove (MVector_RunTime n i dst) (MVector_RunTime m j src) 
--         = if n==m
--             then moveByteArray dst (i*sz) src (j*sz) (n * sz)
--             else error "basicUnsafeMove cannot change size of RunTime MVector"
--         where
--             sz = pp_sizeOf (undefined :: elem)
-- 
--     {-# INLINE basicSet #-}
--     basicSet (MVector_RunTime n i arr) x = setByteArray arr i n x
    

---------------------------------------
-- Automatic size

data instance MVector Automatic s elem = MVector_Automatic
    {-#UNPACK#-}!Int 
    {-#UNPACK#-}!Int 
    {-#UNPACK#-}!(PseudoPrimInfo elem)
    {-#UNPACK#-}!(MutableByteArray s)

instance 
    ( PseudoPrim elem
    ) => VGM.MVector (MVector Automatic) elem 
        where

    {-# INLINE basicLength #-}
    basicLength (MVector_Automatic _ n ppi _) = n
    
    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice i m (MVector_Automatic j n ppi v) = MVector_Automatic (i+j) m ppi v
--     basicUnsafeSlice i m v = if m /= len
--         then error $ "MVector.basicUnsafeSlice not allowed to change size"
--             ++"; i="++show i
--             ++"; m="++show m
--             ++"; len="++show len 
--         else v
--         where
--             len = VGM.length v
 
    {-# INLINE basicOverlaps #-}
    basicOverlaps (MVector_Automatic i m ppi1 arr1) (MVector_Automatic j n ppi2 arr2)
        = sameMutableByteArray arr1 arr2
        && (between i j (j+m) || between j i (i+n))
            where
                between x y z = x >= y && x < z

    {-# INLINE basicUnsafeNew #-}
    basicUnsafeNew n = if seqInfo (undefined::elem)
        then error "basicUnsafeNew: seqInfo"
        else do
            arr <- newPinnedByteArray (n * pp_sizeOf (emptyInfo :: PseudoPrimInfo elem))
            return $ MVector_Automatic 0 n emptyInfo arr

    {-# INLINE basicUnsafeRead #-}
    basicUnsafeRead (MVector_Automatic i _ ppi arr) j = pp_readByteArray ppi arr (i+j)

    {-# INLINE basicUnsafeWrite #-}
    basicUnsafeWrite (MVector_Automatic i _ ppi arr) j x = pp_writeByteArray ppi arr (i+j) x


--     {-# INLINE basicUnsafeCopy #-}
--     basicUnsafeCopy (MVector_Automatic i n dst) (MVector_Automatic j m src) 
--         = if n==m
--             then copyMutableByteArray dst (i*sz) src (j*sz) (n*sz)
--             else error "basicUnsafeCopy cannot change size of Automatic MVector"
--         where
--             sz = pp_sizeOf (undefined :: elem)
--                     
--     {-# INLINE basicUnsafeMove #-}
--     basicUnsafeMove (MVector_Automatic i n dst) (MVector_Automatic j m src) 
--         = if n==m
--             then moveByteArray dst (i*sz) src (j*sz) (n * sz)
--             else error "basicUnsafeMove cannot change size of Automatic MVector"
--         where
--             sz = pp_sizeOf (undefined :: elem)
-- 
--     {-# INLINE basicSet #-}
--     basicSet (MVector_Automatic i n arr) x = setByteArray arr i n x
