{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PolyKinds #-}
module Data.Params.Vector.Unboxed
    ( Vector 
    , module Data.Params.Vector
    )
    where

import Control.Category
import Prelude hiding ((.),id)

import Control.Monad
import Control.Monad.Primitive
import Control.DeepSeq
import Data.Primitive
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

import Debug.Trace


-------------------------------------------------------------------------------
-- immutable automatically sized vector

data family Vector (len::Config Nat) elem
mkParams ''Vector

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

instance
    ( KnownNat len
    , PseudoPrim elem
    ) => StaticToAutomatic 
        Param_len 
        (Vector (Static len) elem)
        (Vector Automatic elem)
        where
    
    staticToAutomatic _ (Vector off ppi arr) = Vector_Automatic off len ppi arr
        where
            len = fromIntegral $ natVal (Proxy::Proxy len)

    mkPseudoPrimInfoFromStatic _ (PseudoPrimInfo_VectorStatic ppi) 
        = PseudoPrimInfo_VectorAutomatic len (len*size) ppi
        where
            len = fromIntegral $ natVal (Proxy::Proxy len)
            size = pp_sizeOf ppi

instance
    ( KnownNat len 
    , StaticToAutomatic p elem elem'
    ) => StaticToAutomatic
        (Param_elem p)
        (Vector (Static len) elem)
        (Vector (Static len) elem')
        where
    
    staticToAutomatic _ (Vector off ppi arr) = Vector off ppi' arr
        where
            ppi' = mkPseudoPrimInfoFromStatic (TypeLens::TypeLens Base p) ppi

    mkPseudoPrimInfoFromStatic _ (PseudoPrimInfo_VectorStatic ppi)
        = PseudoPrimInfo_VectorStatic $ mkPseudoPrimInfoFromStatic (TypeLens :: TypeLens Base p) ppi 

instance
    ( PseudoPrim elem
    ) => RunTimeToAutomatic
        Param_len
        (Vector RunTime elem)
        (Vector Automatic elem)
        where
    
    runTimeToAutomatic lens p v = mkApWith1Param 
        (Proxy::Proxy (Vector RunTime elem))
        (Proxy::Proxy (Vector Automatic elem))
        lens 
        p
        go
        v
        where
            go v@(Vector_RunTime off ppi arr) = Vector_Automatic off len ppi arr
                where
                    len = VG.length v

    mkPseudoPrimInfoFromRuntime _ len (PseudoPrimInfo_VectorRunTime ppi) 
        = PseudoPrimInfo_VectorAutomatic len (len*pp_sizeOf ppi) ppi

instance
    ( RunTimeToAutomatic p elem elem'
    , HasDictionary p
    , ReifiableConstraint (ApplyConstraint_GetConstraint p)
    ) => RunTimeToAutomatic
        (Param_elem p)
        (Vector (Static len) elem)
        (Vector (Static len) elem')
        where

    runTimeToAutomatic lens p v = mkApWith1Param
        (Proxy::Proxy (Vector (Static len) elem))
        (Proxy::Proxy (Vector (Static len) elem'))
        lens
        p
        go
        v
        where
            go :: Vector (Static len) elem -> Vector (Static len) elem'
            go (Vector off ppi arr) = Vector off ppi' arr
                where 
                    ppi' = mkPseudoPrimInfoFromRuntime (TypeLens::TypeLens Base p) p ppi
                        :: PseudoPrimInfo elem'

    mkPseudoPrimInfoFromRuntime _ p (PseudoPrimInfo_VectorStatic ppi)
        = PseudoPrimInfo_VectorStatic $ mkPseudoPrimInfoFromRuntime (TypeLens::TypeLens Base p) p ppi

instance
    ( RunTimeToAutomatic p elem elem'
    , HasDictionary p
    , ReifiableConstraint (ApplyConstraint_GetConstraint p)
    ) => RunTimeToAutomatic
        (Param_elem p)
        (Vector RunTime elem)
        (Vector RunTime elem')
        where

    runTimeToAutomatic lens p v = mkApWith1Param
        (Proxy::Proxy (Vector RunTime elem))
        (Proxy::Proxy (Vector RunTime elem'))
        lens
        p
        go
        v
        where
            go :: Vector RunTime elem -> Vector RunTime elem'
            go (Vector_RunTime off ppi arr) = Vector_RunTime off ppi' arr
                where
                    ppi' = mkPseudoPrimInfoFromRuntime (TypeLens::TypeLens Base p) p ppi
                        :: PseudoPrimInfo elem'

    mkPseudoPrimInfoFromRuntime _ p (PseudoPrimInfo_VectorRunTime ppi)
        = PseudoPrimInfo_VectorRunTime $ mkPseudoPrimInfoFromRuntime (TypeLens::TypeLens Base p) p ppi

instance
    ( RunTimeToAutomatic p elem elem'
    , HasDictionary p
    , ReifiableConstraint (ApplyConstraint_GetConstraint p)
    ) => RunTimeToAutomatic
        (Param_elem p)
        (Vector Automatic elem)
        (Vector Automatic elem')
        where

    runTimeToAutomatic lens p v = mkApWith1Param
        (Proxy::Proxy (Vector Automatic elem))
        (Proxy::Proxy (Vector Automatic elem'))
        lens
        p
        go
        v
        where
            go :: Vector Automatic elem -> Vector Automatic elem'
            go (Vector_Automatic len off ppi arr) = Vector_Automatic len off ppi' arr
                where
                    ppi' = mkPseudoPrimInfoFromRuntime (TypeLens::TypeLens Base p) p ppi
                        :: PseudoPrimInfo elem'

    mkPseudoPrimInfoFromRuntime _ p (PseudoPrimInfo_VectorAutomatic len size ppi)
        = PseudoPrimInfo_VectorAutomatic 
            len
            size
            (mkPseudoPrimInfoFromRuntime (TypeLens::TypeLens Base p) p ppi)

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
    basicLength _ = viewParam _len (undefined::Vector (Static len) elem) 

    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice j n v = if n /= viewParam _len (undefined::Vector (Static len) elem) || j /= 0
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
    ( Prim elem
    , PseudoPrim elem
    , KnownNat len
    ) => Prim (Vector (Static len) elem)
        where
    
    {-# INLINE sizeOf# #-}
    sizeOf# _ = 
        unInt (sizeOf (undefined::elem)* (intparam (Proxy::Proxy len)))

    {-# INLINE alignment# #-}
    alignment# _ = 
        unInt (alignment (undefined :: elem))
--         unInt (sizeOf ppi * (intparam (Proxy::Proxy len)))

    {-# INLINE indexByteArray# #-}
    indexByteArray# arr# i# = 
        Vector ((I# i#)*(intparam (Proxy::Proxy len))) (emptyInfo::PseudoPrimInfo elem) (ByteArray arr#)

    {-# INLINE readByteArray# #-}
    readByteArray# marr# i# s# = 
        (# s#, Vector (I# i#) (emptyInfo::PseudoPrimInfo elem) (ByteArray (unsafeCoerce# marr#)) #)

    {-# INLINE writeByteArray# #-}
    writeByteArray# marr# i# x s# = go 0 s#
        where
            go i s = ( if i >= intparam (Proxy::Proxy len)
                then s
                else go (i+1) 
                        (writeByteArray# marr# 
                            (i# *# (unInt ( intparam (Proxy::Proxy len))) +# (unInt i)) 
--                             (x VG.! i)
                            (x `VG.unsafeIndex` i)
                            s
                        )
                    )
                where 
                    iii = I# (i# *# (sizeOf# (undefined::elem)) +# (unInt i)) 

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
--                             (x VG.! i)
                            (x `VG.unsafeIndex` i)
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


instance 
    ( PseudoPrim elem 
--     , GetParam_len (Vector RunTime elem)
--     , ViewParam GetParam_len (Vector RunTime elem)
    , ViewParam Param_len (Vector RunTime elem)
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
--     , GetParam_len (Vector RunTime elem)
    , ViewParam Param_len (Vector RunTime elem)
    ) => PseudoPrim (Vector RunTime elem)
        where

    newtype PseudoPrimInfo (Vector RunTime elem) = 
        PseudoPrimInfo_VectorRunTime (PseudoPrimInfo elem)
    
    {-# INLINE pp_sizeOf# #-}
    pp_sizeOf# (PseudoPrimInfo_VectorRunTime ppi) = 
--         unInt (pp_sizeOf ppi * (getParam_len (undefined::Vector RunTime elem)))
        unInt (pp_sizeOf ppi * (viewParam _len (undefined::Vector RunTime elem)))

    {-# INLINE pp_alignment# #-}
    pp_alignment# (PseudoPrimInfo_VectorRunTime ppi) = 
        unInt (pp_alignment ppi)
--         unInt (pp_sizeOf (undefined::elem) * (getParam_len (undefined::Vector RunTime elem)))

    {-# INLINE pp_indexByteArray# #-}
    pp_indexByteArray# (PseudoPrimInfo_VectorRunTime ppi)arr# i# = 
--         Vector_RunTime ((I# i#)*(getParam_len (undefined::Vector RunTime elem))) ppi (ByteArray arr#)
        Vector_RunTime ((I# i#)*(viewParam _len (undefined::Vector RunTime elem))) ppi (ByteArray arr#)

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
                    len = viewParam _len (undefined::Vector RunTime elem)
--                     len = getParam_len (undefined::Vector RunTime elem)
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

instance NFData (Vector Automatic elem) where
    rnf v = seq v ()

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

instance PseudoPrim elem => PseudoPrim (Vector Automatic elem) where
    data PseudoPrimInfo (Vector Automatic elem) = PseudoPrimInfo_VectorAutomatic
        {-#UNPACK#-}!(Int) -- length
        {-#UNPACK#-}!(Int) -- sizeOf
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

data family MVector (len::Config Nat) s elem

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
        then error $ "MVector (Static len) .basicUnsafeSlice not allowed to change size"
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
