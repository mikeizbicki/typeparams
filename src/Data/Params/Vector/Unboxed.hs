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

u = VG.singleton $ VG.fromList [1..10] :: Vector (Static 1) (Vector (Static 10) Int)
u' = withInnerParam (len 10) $ VG.singleton $ VG.fromList [1..10] :: Vector (Static 1) (Vector RunTime Int)
-- u'' = withParam (Def_Meta_elem (len 10)) (VG.singleton (VG.fromList [1..10]) :: Vector (Static 1) (Vector RunTime Int))

class Meta_elem (p :: * -> Constraint) e v {-| v -> e-} where
    meta_elem :: (e -> a) -> v -> a

instance Meta_elem Param_len elem (Vector len elem) where
    meta_elem p _ = p (undefined::elem)

class Meta_a (p :: * -> Constraint) e v | v -> e where
    meta_a :: (e -> a) -> v -> a

class Meta_b (p :: * -> Constraint) e v | v -> e where
    meta_b :: (e -> a) -> v -> a

instance Meta_a Param_len a (Either a b) where
    meta_a p _ = p (undefined::elem)

instance Meta_b Param_len b (Either a b) where
    meta_b p _ = p (undefined::elem)

-- class WithMeta_elem m where
--     elem :: (WithParam param elem) => DefParam param elem -> DefParam (Meta_elem p) m
-- 
-- instance WithMeta_elem (Vector len elem) where
--     elem = Def_Meta_elem

-- class Meta_elem2 p m

-- instance Meta_elem p (Vector len elem) => WithParam (Meta_elem p) (Vector len elem) where
instance 
    ( WithParam param elem
    , Meta_elem param elem (Vector len elem)
    ) => WithParam (Meta_elem param elem) (Vector len elem) 
        where
    data DefParam (Meta_elem param elem) (Vector len elem) = 
        Def_Meta_elem
        { unDef_Meta_elem :: DefParam param elem
        }

--     withParam (Def_Meta_elem p) a = withParam p a
    withParam (Def_Meta_elem p) a = withInnerParam p a
--     apWithParam (Def_Meta_elem p) f a = apWithInnerParam p f a

instance
    ( WithParam param b
    , Meta_b param b (Either a b)
    ) => WithParam (Meta_b param b) (Either a b) 
        where
    data DefParam (Meta_b param b) (Either a b) = Def_Meta_b
        { unDef_Meta_b :: DefParam param b
        }
    type ParamConstraint (Meta_b param b) (Either a b) = ParamConstraint param b
    withParam (Def_Meta_b p) x = withInnerParam p x
    apWithParam (Def_Meta_b p) x = apWithParam (unsafeCoerce p) (unsafeCoerce x)


class WithParam2 p1 p2 m1 m2 where
    data DefParam2 p1 p2 m1 m2 :: *

    -- | dynamically specifies a single 'RunTime' parameter of function output
    withParam2 :: DefParam2 p1 p2 m1 m2 -> (p2 m2 => m1) -> m1

    apWithParam2 :: DefParam2 p1 p2 m1 m2 -> (p2 m2 => m1 -> n) -> (p2 m2 => m1) -> n

class WithParam2_len m elem | m -> elem where
    len2 :: Int -> DefParam2 Param_len Param_len m m

instance WithParam2_len (Vector RunTime elem) (Vector RunTime elem) where
    len2 = DefParam2_Vector_len . len

class WithParam2_elem m elem | m -> elem where
    elem2 :: WithParam2 p1 p2 elem m2 
         => DefParam2 p1 p2 elem m2 
         -> DefParam2 (Meta_elem p1 m) p2 m m2 

instance WithParam2_elem (Vector len elem) elem where
    elem2 = DefParam2_Vector_elem

-- instance WithParam2 Param_len Param_len (Vector RunTime elem) (Vector RunTime elem) where
--     data DefParam2 Param_len Param_len (Vector RunTime elem) (Vector RunTime elem) =
instance WithParam2 Param_len Param_len (Vector RunTime elem) m2 where
    data DefParam2 Param_len Param_len (Vector RunTime elem) m2 =
        DefParam2_Vector_len
        { unDefParam2_Vector_len :: DefParam Param_len (Vector RunTime elem)
        }
    withParam2 (DefParam2_Vector_len p) = unsafeCoerce $ withParam p
    apWithParam2 (DefParam2_Vector_len p) = unsafeCoerce $ apWithParam p 

instance 
    ( WithParam2 p1 p2 elem m2
    ) => WithParam2 (Meta_elem p1 (Vector len elem)) p2 (Vector len elem) m2
        where
    data DefParam2 (Meta_elem p1 (Vector len elem)) p2 (Vector len elem) m2 =
        DefParam2_Vector_elem
        { unDefParam2_Vector_elem :: DefParam2 p1 p2 elem m2
        }
    withParam2 (DefParam2_Vector_elem p) = unsafeCoerce $ withParam2 p
    apWithParam2 (DefParam2_Vector_elem p) = unsafeCoerce $ apWithParam2 p

instance
    ( WithParam2 p1 p2 b c
    ) => WithParam2 (Meta_b p1 (Either a b)) p2 (Either a b) c 
        where
    data DefParam2 (Meta_b p1 (Either a b)) p2 (Either a b) c =
        DefParam2_Either_b
        { unDefParam2_Either_b :: DefParam2 p1 p2 b c
        }
    withParam2 (DefParam2_Either_b p) = unsafeCoerce $ withParam2 p
    apWithParam2 (DefParam2_Either_b p) = unsafeCoerce $ apWithParam2 p

instance
    ( WithParam2 p1 p2 a c
    ) => WithParam2 (Meta_a p1 (Either a b)) p2 (Either a b) c 
        where
    data DefParam2 (Meta_a p1 (Either a b)) p2 (Either a b) c =
        DefParam2_Either_a
        { unDefParam2_Either_a :: DefParam2 p1 p2 a c
        }
    withParam2 (DefParam2_Either_a p) = unsafeCoerce $ withParam2 p
    apWithParam2 (DefParam2_Either_a p) = unsafeCoerce $ apWithParam2 p

-- withInnerParam2 :: forall p m n. 
--     ( WithParam2 p1 p2 m1 m2 
--     ) => DefParam2 p1 p2 m1 m2 
--       -> (=> n m) -> n m
-- withInnerParam2 = unsafeCoerce (withParam :: DefParam p m -> (ParamConstraint p m => m) -> m)

--     -- | dynamically specifies a single 'RunTime' parameter of function input
--     apWithParam2 :: DefParam p m -> (ParamConstraint p m => m -> n) -> (p m => m) -> n

-- instance
--     ( WithParam Param_len (Vector RunTime elem)
--     , Meta_b Param_len (Vector RunTime elem) (Either a (Vector RunTime elem))
--     ) => WithParam (Meta_b Param_len (Vector RunTime elem)) (Either a (Vector RunTime elem)) 
--         where
--     data DefParam (Meta_b Param_len (Vector RunTime elem)) (Either a (Vector RunTime elem)) = Def_Meta_b
--         { unDef_Meta_b :: DefParam Param_len (Vector RunTime elem)
--         }
--     type ParamConstraint (Meta_b Param_len (Vector RunTime elem)) (Either a (Vector RunTime elem)) = Param_len (Vector RunTime elem)

-- instance 
--     ( SetParam param elem1 elem2
--     ) => SetParam (Meta_elem param elem1) (Vector len elem1) (Vector len elem2)
--         where
--     
--     setParam 

poop :: (Param_len (Vector RunTime Int) => Vector (Static 1) (Vector RunTime Int)) -> (Vector (Static 1) (Vector RunTime Int))
poop = undefined

rightVec :: 
    ( Meta_b Param_len (Vector RunTime Int) (Either a (Vector RunTime Int))
    ) => (Param_len (Vector RunTime Int) => Either a (Vector RunTime Int)) 
      -> (Either a (Vector RunTime Int))
rightVec = undefined

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
