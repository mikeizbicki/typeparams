{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module Data.Params.FastVector
    where

import Control.Monad
import Control.Monad.Primitive
import Control.DeepSeq
import Data.Primitive
import Data.Primitive.ByteArray
import Data.Primitive.Types
import Data.Proxy
import GHC.Ptr
import Foreign.Ptr
import Foreign.ForeignPtr hiding (unsafeForeignPtrToPtr)
import Foreign.ForeignPtr.Unsafe
import Foreign.Marshal.Array
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Primitive.Mutable as VPM

-- import Language.Haskell.TH
import Unsafe.Coerce
import Debug.Trace

import GHC.Base (Int (..))
import GHC.Int
import GHC.Prim
import GHC.TypeLits
import Data.Params

-------------------------------------------------------------------------------
-- Vector

data instance VU.Vector (Vector (Just len) elem) = VUVector 
    {-#UNPACK#-}!Int 
    {-#UNPACK#-}!Int 
    {-#UNPACK#-}!ByteArray

data instance VUM.MVector s (Vector (Just len) elem) = VUMVector 
    {-#UNPACK#-}!Int 
    {-#UNPACK#-}!Int 
    {-#UNPACK#-}!(MutableByteArray s)

-------------------------------------------------------------------------------
-- Vector

data family Vector (len::Maybe Nat) elem
data instance Vector (Just len) elem = Vector {-#UNPACK#-}!Int {-#UNPACK#-}!ByteArray
-- mkParams ''Vector
newtype instance Vector Nothing elem = Vector_Nothing (VP.Vector elem)

type instance VG.Mutable (Vector len) = MVector len

instance (Show elem, VG.Vector (Vector len) elem) => Show (Vector len elem) where
    show v = "fromList "++show (VG.toList v)

instance (Eq elem, VG.Vector (Vector len) elem) => Eq (Vector len elem) where
    a == b = (VG.toList a) == (VG.toList b)

instance (Ord elem, VG.Vector (Vector len) elem) => Ord (Vector len elem) where
    compare a b = compare (VG.toList a) (VG.toList b)

instance NFData (Vector (Just len) elem) where
    rnf a = seq a ()
instance NFData elem => NFData (Vector Nothing elem) where
    rnf (Vector_Nothing v) = rnf v

unInt :: Int -> Int#
unInt (I# i) = i

instance
    ( Prim elem
    , KnownNat len
    , Show elem
    ) => Prim (Vector (Just len) elem)
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
                    iii = I# (i# *# (sizeOf# (undefined::Vector (Just len) elem)) +# (unInt i)) 

instance Prim elem => VG.Vector (Vector Nothing) elem where 
    {-# INLINE basicUnsafeFreeze #-}
    basicUnsafeFreeze (MVector_Nothing v) = Vector_Nothing `liftM` VG.basicUnsafeFreeze v

    {-# INLINE basicUnsafeThaw #-}
    basicUnsafeThaw (Vector_Nothing v) = MVector_Nothing `liftM` VG.basicUnsafeThaw v

    {-# INLINE basicLength #-}
    basicLength (Vector_Nothing v) = VG.basicLength v

    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice i m (Vector_Nothing v) = Vector_Nothing $ VG.basicUnsafeSlice i m v

    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeIndexM (Vector_Nothing v) i = VG.basicUnsafeIndexM v i

    {-# INLINE basicUnsafeCopy #-}
    basicUnsafeCopy (MVector_Nothing mv) (Vector_Nothing v) = VG.basicUnsafeCopy mv v

    {-# INLINE elemseq #-}
    elemseq _ = seq

instance 
    ( Prim elem 
    , KnownNat len
    ) => VG.Vector (Vector (Just len)) elem 
        where
    
    {-# INLINE basicUnsafeFreeze #-}
    basicUnsafeFreeze (MVector i marr) = Vector i `liftM` unsafeFreezeByteArray marr

    {-# INLINE basicUnsafeThaw #-}
    basicUnsafeThaw (Vector i arr) = MVector i `liftM` unsafeThawByteArray arr

    {-# INLINE [2] basicLength #-}
    basicLength _ = intparam (Proxy::Proxy len)

    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice j n v = if n /= intparam (Proxy::Proxy len) || j /= 0
        then error $ "Vector.basicUnsafeSlice not allowed to change size"
        else v

    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeIndexM (Vector i arr) j = return $! indexByteArray arr (i+j)

    {-# INLINE basicUnsafeCopy #-}
    basicUnsafeCopy (MVector i dst) (Vector j src) = copyByteArray dst (i*sz) src (j*sz) (len*sz)
        where
            sz = sizeOf (undefined :: elem)
            len = intparam (Proxy::Proxy len)

    {-# INLINE elemseq #-}
    elemseq _ = seq

---------------------------------------

data family MVector (len::Maybe Nat) s elem
data instance MVector (Just len) s elem = MVector {-#UNPACK#-}!Int {-#UNPACK#-}!(MutableByteArray s)
newtype instance MVector Nothing s elem = MVector_Nothing (VPM.MVector s elem)
-- mkParams ''MVector

instance Prim elem => VGM.MVector (MVector Nothing) elem where

    {-# INLINE basicLength #-}
    basicLength (MVector_Nothing v) = VGM.basicLength v

    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice i m (MVector_Nothing v) = MVector_Nothing $ VGM.basicUnsafeSlice i m v

    {-# INLINE basicOverlaps #-}
    basicOverlaps (MVector_Nothing v1) (MVector_Nothing v2) = VGM.basicOverlaps v1 v2

    {-# INLINE basicUnsafeNew #-}
    basicUnsafeNew i = MVector_Nothing `liftM` VGM.basicUnsafeNew i

    {-# INLINE basicUnsafeRead #-}
    basicUnsafeRead (MVector_Nothing v) i = VGM.basicUnsafeRead v i

    {-# INLINE basicUnsafeWrite #-}
    basicUnsafeWrite (MVector_Nothing v) i x = VGM.basicUnsafeWrite v i x

    {-# INLINE basicUnsafeCopy #-}
    basicUnsafeCopy (MVector_Nothing v1) (MVector_Nothing v2) = VGM.basicUnsafeCopy v1 v2

    {-# INLINE basicUnsafeMove #-}
    basicUnsafeMove (MVector_Nothing v1) (MVector_Nothing v2) = VGM.basicUnsafeMove v1 v2

    {-# INLINE basicSet #-}
    basicSet (MVector_Nothing v) x = VGM.basicSet v x

instance 
    ( Prim elem
    , KnownNat len
    ) => VGM.MVector (MVector (Just len)) elem 
        where

    {-# INLINE basicLength #-}
    basicLength _ = intparam (Proxy::Proxy len) 
    
    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice i m v = if m /= intparam (Proxy::Proxy len)
        then error $ "FastMVector.basicUnsafeSlice not allowed to change size; i="++show i++"; m="++show m++"; len="++show (intparam (Proxy::Proxy len))
        else v
 
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

-------------------------------------------------------------------------------
--

type family Plus (a::Maybe Nat) (b::Nat) :: Maybe Nat
type instance Plus Nothing b = Nothing
type instance Plus (Just a) b = Just (a+b)

newtype L2Vector len elem = L2Vector (Vector (len `Plus` 1) elem)
-- mkParams ''L2Vector

-- instance (VG.Vector (Vector (len `Plus` 1)) elem, Show elem) => Show (L2Vector len elem) where
--     show (L2Vector v) = "fromList "++show (VG.toList v)


deriving instance (Show (Vector (len `Plus` 1) elem)) => Show (L2Vector len elem)
deriving instance (Eq (Vector (len `Plus` 1) elem)) => Eq (L2Vector len elem)
deriving instance (Ord (Vector (len `Plus` 1) elem)) => Ord (L2Vector len elem)
deriving instance (NFData (Vector (len `Plus` 1) elem)) => NFData (L2Vector len elem)

instance
    ( Prim (Vector (len `Plus` 1) elem) 
    ) => Prim (L2Vector len elem)
        where
    
    {-# INLINE sizeOf# #-}
    sizeOf# _ = sizeOf# (undefined::Vector (len `Plus` 1) elem)

    {-# INLINE alignment# #-}
    alignment# _ = alignment# (undefined::Vector (len `Plus` 1) elem)

    {-# INLINE indexByteArray# #-}
    indexByteArray# arr# i# = L2Vector $ indexByteArray# arr# i#

    {-# INLINE readByteArray# #-}
    readByteArray# marr# i# s# = (# s2#, L2Vector v #)
        where
            (# s2#, v #) = readByteArray# marr# i# s#

    {-# INLINE writeByteArray# #-}
    writeByteArray# marr# i# (L2Vector v) s# = writeByteArray# marr# i# v s#

type instance VG.Mutable (L2Vector len) = L2MVector len

instance 
    ( VG.Vector (Vector (len `Plus` 1)) elem
    , Num elem
    ) => VG.Vector (L2Vector len) elem
        where

    {-# INLINE basicLength #-}
    basicLength (L2Vector v) = VG.basicLength v-1

    {-# INLINE basicUnsafeFreeze #-}
    basicUnsafeFreeze (L2MVector v) = do
        totL <- forM [1..VGM.length v] $ \i -> do
            x <- VGM.unsafeRead v i
            return $ x*x
        VGM.unsafeWrite v 0 $ sum totL
        L2Vector `liftM` VG.basicUnsafeFreeze v

    {-# INLINE basicUnsafeThaw #-}
    basicUnsafeThaw (L2Vector v) = L2MVector `liftM` VG.basicUnsafeThaw v

    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice i j (L2Vector v) = L2Vector $ VG.basicUnsafeSlice i j v

    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeIndexM (L2Vector v) i = VG.basicUnsafeIndexM v (i+1)

newtype L2MVector len s elem = L2MVector (MVector (len `Plus` 1) s elem)
-- mkParams ''L2MVector

instance 
    ( VGM.MVector (MVector (len `Plus` 1)) elem 
    ) => VGM.MVector (L2MVector len) elem
        where
    
    {-# INLINE basicLength #-} 
    basicLength (L2MVector v) = (VGM.basicLength v)-1

    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice i m (L2MVector v) = L2MVector $ VGM.basicUnsafeSlice i (m+1) v

    {-# INLINE basicOverlaps #-}
    basicOverlaps (L2MVector v1) (L2MVector v2) = VGM.basicOverlaps v1 v2

    {-# INLINE basicUnsafeNew #-}
    basicUnsafeNew i = L2MVector `liftM` VGM.basicUnsafeNew (i+1)

    {-# INLINE basicUnsafeRead #-}
    basicUnsafeRead (L2MVector v) i = VGM.basicUnsafeRead v (i+1)

    {-# INLINE basicUnsafeWrite #-}
    basicUnsafeWrite (L2MVector v) i x = VGM.basicUnsafeWrite v (i+1) x

{-# INLINE l2distance4 #-}
l2distance4 :: forall len elem.
    ( Prim elem
    , KnownNat (len+1)
    , KnownNat len
    , Floating elem
    ) => L2Vector (Just len) elem -> L2Vector (Just len) elem -> elem
l2distance4 (L2Vector v1) (L2Vector v2) = sqrt $ v1 `VG.unsafeIndex` 0 + v2 `VG.unsafeIndex` 0 -2*(go 0 1)
    where
        go !tot !i = if i > intparam (Proxy::Proxy len)
            then tot
            else go (tot+(v1 `VG.unsafeIndex` i * v2 `VG.unsafeIndex` i)
                        +(v1 `VG.unsafeIndex` (i+1) * v2 `VG.unsafeIndex` (i+1))
                        +(v1 `VG.unsafeIndex` (i+2) * v2 `VG.unsafeIndex` (i+2))
                        +(v1 `VG.unsafeIndex` (i+3) * v2 `VG.unsafeIndex` (i+3))) (i+4)

{-# INLINE distance_vector_diff4 #-}
distance_vector_diff4 :: (Show f,VG.Vector v f, Floating f) => v f -> v f -> f
distance_vector_diff4 !v1 !v2 = sqrt $ go 0 (VG.length v1-1)
    where
        go tot (-1) = tot
        go tot i = trace ("go i="++show i++"; diff1="++show diff1++" v1!i="++show (v1 VG.! i)++"; v2!i="++show (v2 VG.! i)) $ go (tot+diff1*diff1
--                           +diff2*diff2
--                           +diff3*diff3
--                           +diff4*diff4
                      ) (i-1)
            where 
                diff1 = v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i
--                 diff2 = v1 `VG.unsafeIndex` (i-1)-v2 `VG.unsafeIndex` (i-1)
--                 diff3 = v1 `VG.unsafeIndex` (i-2)-v2 `VG.unsafeIndex` (i-2)
--                 diff4 = v1 `VG.unsafeIndex` (i-3)-v2 `VG.unsafeIndex` (i-3)

{-# INLINE l2distance #-}
l2distance :: forall len elem.
    ( Prim elem
    , KnownNat (len+1)
    , KnownNat len
    , Floating elem
    ) => L2Vector (Just len) elem -> L2Vector (Just len) elem -> elem
l2distance (L2Vector v1) (L2Vector v2) = sqrt $ v1 `VG.unsafeIndex` 0 + v2 `VG.unsafeIndex` 0 -2*(go 0 1)
    where
        go !tot !i = if i > intparam (Proxy::Proxy len)
            then tot
            else go (tot+(v1 `VG.unsafeIndex` i * v2 `VG.unsafeIndex` i)) (i+1)


------------------------------------------------------------------------------


{-
data family L2Vector (len::Maybe Nat) (stride::Maybe Nat) elem
newtype instance L2Vector (Just len) (Just stride) elem = 
    L2Vector (FastVector (Just (len+1)) (Just stride) elem)

type instance (+) 10 1 = 11

instance Show (FastVector (Just (len+1)) (Just stride) elem) => Show (L2Vector (Just len) (Just stride) elem) where
    show (L2Vector v) = show v

type instance VG.Mutable (L2Vector len stride) = L2MVector len stride

instance    
    ( KnownNat len
    , KnownNat (len+1)
    , Storable elem
    , Num elem
    ) => VG.Vector (L2Vector (Just len) (Just 1)) elem
        where
    {-# INLINE basicUnsafeFreeze #-}
    {-# INLINE basicUnsafeThaw #-}
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeFreeze (L2MVector v) = do
        tot <- forM [1..intparam (Proxy::Proxy len)] $ \i -> do
            x <- VGM.unsafeRead v i
            return $ x*x
        VGM.unsafeWrite v 0 $ sum tot 
        L2Vector `liftM` VG.basicUnsafeFreeze v
        where
            
    basicUnsafeThaw (L2Vector v) = L2MVector `liftM` VG.basicUnsafeThaw v
    basicLength _ = intparam (Proxy::Proxy len)
    basicUnsafeSlice i m v = if i/=0 || m /= intparam (Proxy::Proxy len)
        then error "L2Vector.basicUnsafeSlice not allowed to change size"
        else v
    basicUnsafeIndexM (L2Vector v) i = VG.basicUnsafeIndexM v (i+1)

data family L2MVector (len::Maybe Nat) (stride::Maybe Nat) s elem
newtype instance L2MVector (Just len) (Just stride) s elem = 
    L2MVector (FastMVector (Just (len+1)) (Just stride) s elem)

instance 
    ( KnownNat len
    , KnownNat (len+1)
    , Storable elem
    , Num elem
    ) => VGM.MVector (L2MVector (Just len) (Just 1)) elem 
        where
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicOverlaps #-}
    {-# INLINE basicUnsafeNew #-}
    {-# INLINE basicUnsafeRead #-}
    {-# INLINE basicUnsafeWrite #-}
    basicLength _ = intparam (Proxy::Proxy len)
    basicUnsafeSlice i m (L2MVector v) = L2MVector $ VGM.basicUnsafeSlice i (m+1) v 
    basicOverlaps (L2MVector v1) (L2MVector v2) = VGM.basicOverlaps v1 v2
    basicUnsafeNew i = L2MVector `liftM` VGM.basicUnsafeNew i
    basicUnsafeRead (L2MVector v) i = VGM.basicUnsafeRead v (i+1)
    basicUnsafeWrite (L2MVector v) i x = do
        sumold <- VGM.basicUnsafeRead v 0
        xold <- VGM.basicUnsafeRead v (i+1)
        VGM.basicUnsafeWrite v (i+1) x
        VGM.basicUnsafeWrite v 0 (sumold-xold+x*x)

-- instance 
--     ( Storable elem
--     , KnownNat len
--     ) => MetricSpace (L2Vector (Just len) (Just 1) elem) 
--         where
--     

data instance VU.Vector (L2Vector (Just len) (Just 1) elem) = 
    Vector {-#UNPACK#-}!Int {-#UNPACK#-}!Int {-#UNPACK#-}!ByteArray

-- instance (Show elem, Storable elem, KnownNat len) => Show (VU.Vector (L2Vector (Just len) (Just 1) elem)) where
--     show (Vector i m arr) = "fromList "++show  
--         [ L2Vector $ FastVector $ inlinePerformIOadvancePtr ptr ((i+j)*(1+intparam (Proxy::Proxy len)))
--         | j <- [0..m-1]
--         ]
--         where
--             ptr = Ptr addr :: Ptr elem
--             !(Addr addr) = byteArrayContents arr
        
instance
    ( Storable elem
    , Prim elem
    , KnownNat len
    , KnownNat (len+1)
    ) => VU.Unbox (L2Vector (Just len) (Just 1) elem)

instance 
    ( Storable elem
    , Prim elem
    , KnownNat len
    , KnownNat (len+1)
    ) => VG.Vector VU.Vector (L2Vector (Just len) (Just 1) elem) 
        where
    
    {-# INLINE basicUnsafeFreeze #-}
    basicUnsafeFreeze (MVector i n marr) = Vector i n `liftM` unsafeFreezeByteArray marr
    
    {-# INLINE basicUnsafeThaw #-}
    basicUnsafeThaw (Vector i n arr) = MVector i n `liftM` unsafeThawByteArray arr

    {-# INLINE basicLength #-}
    basicLength (Vector _ n _) = n

    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice j n (Vector i _ arr) = Vector (i+j) n arr

    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeIndexM (Vector i _ arr) j = do
        let fp = unsafeInlineIO $ newForeignPtr_ $ advancePtr ptr ((i+j)*(1+intparam (Proxy::Proxy len)))
        return $ L2Vector $ FastVector fp
        where
            ptr = Ptr addr :: Ptr elem
            !(Addr addr) = byteArrayContents arr


data instance VUM.MVector s (L2Vector (Just len) (Just 1) elem) = 
    MVector {-#UNPACK#-}!Int {-#UNPACK#-}!Int {-#UNPACK#-}!(MutableByteArray s)

instance 
    (Storable elem
    , Prim elem
    , KnownNat len
    , KnownNat (len+1)
    ) => VGM.MVector VUM.MVector (L2Vector (Just len) (Just 1) elem) 
        where

    {-# INLINE basicLength #-}
    basicLength (MVector _ n _) = n

    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice j m (MVector i n arr) = MVector (i+j) m arr

    {-# INLINE basicOverlaps #-}
    basicOverlaps (MVector i m arr1) (MVector j n arr2)
        = sameMutableByteArray arr1 arr2
        && (between i j (j+n) || between j i (i+m))
        where
            between x y z = x >= y && x < z

    {-# INLINE basicUnsafeNew #-}
    basicUnsafeNew n = MVector 0 n `liftM` newAlignedPinnedByteArray ((n+1) * sizeOf (undefined :: elem)) (2^16)

    {-# INLINE basicUnsafeRead #-}
    basicUnsafeRead (MVector i n arr) j = unsafePrimToPrim $ do
        fp <- newForeignPtr_ $ advancePtr ptr ((i+j)*(1+intparam (Proxy::Proxy len)))
        return $ L2Vector $ FastVector fp
        where
            ptr = Ptr addr :: Ptr elem 
            !(Addr addr) = mutableByteArrayContents arr

    {-# INLINE basicUnsafeWrite #-}
    basicUnsafeWrite (MVector i n arr) j (L2Vector v) = 
        forM_ [0..intparam (Proxy::Proxy len)] $ \k -> do
            writeByteArray arr ((i+j)*(intparam (Proxy::Proxy len)+1)+k) (v VG.! k)

{-# INLINE l2distance #-}
l2distance :: forall len elem.
    ( Storable elem
    , KnownNat (len+1)
    , KnownNat len
    , Floating elem
    ) => L2Vector (Just len) (Just 1) elem -> L2Vector (Just len) (Just 1) elem -> elem
l2distance (L2Vector fp1) (L2Vector fp2) = sqrt $ fp1 `VG.unsafeIndex` 0 + fp2 `VG.unsafeIndex` 0 -2*(go 0 1)
    where
        go !tot !i = if i > intparam (Proxy::Proxy len)
            then tot
            else go (tot+(fp1 `VG.unsafeIndex` i * fp2 `VG.unsafeIndex` i)) (i+1)

{-# INLINE l2distance8 #-}
l2distance8 :: forall len elem.
    ( Storable elem
    , KnownNat (len+1)
    , KnownNat len
    , Floating elem
    ) => L2Vector (Just len) (Just 1) elem -> L2Vector (Just len) (Just 1) elem -> elem
l2distance8 (L2Vector fp1) (L2Vector fp2) = sqrt $ fp1 `VG.unsafeIndex` 0 + fp2 `VG.unsafeIndex` 0 -2*(go 0 1)
    where
        go !tot !i = if i > intparam (Proxy::Proxy len)
            then tot
            else go (tot+(fp1 `VG.unsafeIndex` i * fp2 `VG.unsafeIndex` i)
                        +(fp1 `VG.unsafeIndex` (i+1) * fp2 `VG.unsafeIndex` (i+1))
                        +(fp1 `VG.unsafeIndex` (i+2) * fp2 `VG.unsafeIndex` (i+2))
                        +(fp1 `VG.unsafeIndex` (i+3) * fp2 `VG.unsafeIndex` (i+3))
                        +(fp1 `VG.unsafeIndex` (i+4) * fp2 `VG.unsafeIndex` (i+4))
                        +(fp1 `VG.unsafeIndex` (i+5) * fp2 `VG.unsafeIndex` (i+5))
                        +(fp1 `VG.unsafeIndex` (i+6) * fp2 `VG.unsafeIndex` (i+6))
                        +(fp1 `VG.unsafeIndex` (i+7) * fp2 `VG.unsafeIndex` (i+7))) (i+8)

-}
