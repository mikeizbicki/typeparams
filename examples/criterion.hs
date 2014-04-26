{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.DeepSeq
import Control.Monad
import Control.Monad.Random
import Control.Monad.ST
import Criterion.Config
import Criterion.Main
import Data.Params
import Data.Primitive.ByteArray 
import qualified Data.Vector.Generic as VG
import qualified Data.Params.Vector.Unboxed as VPU
import qualified Data.Params.Vector.UnboxedRaw as VPR
import qualified Data.Vector.Unboxed as VU

-------------------------------------------------------------------------------
-- criterion tests

arrlen = 12 :: Int
type Arrlen = 12

main = do
    
    let dimL1 :: [Float] = evalRand (replicateM arrlen $ getRandomR (-10000,10000)) (mkStdGen $ 1)
        dimL2 :: [Float] = evalRand (replicateM arrlen $ getRandomR (-10000,10000)) (mkStdGen $ 2)

    let vu1 = VU.fromList dimL1
        vu2 = VU.fromList dimL2

    let vpun1 = VG.fromList dimL1 :: VPU.Vector Nothing Float
        vpun2 = VG.fromList dimL2 :: VPU.Vector Nothing Float

    let vpuj1 = VG.fromList dimL1 :: VPU.Vector (Just Arrlen) Float
        vpuj2 = VG.fromList dimL2 :: VPU.Vector (Just Arrlen) Float

    let vprj1 = VG.fromList dimL1 :: VPR.Vector (Just Arrlen) Float
        vprj2 = VG.fromList dimL2 :: VPR.Vector (Just Arrlen) Float

    let ba1 = list2ByteArray dimL1
        ba2 = list2ByteArray dimL2

    deepseq vu1 $ deepseq vu2 $ return ()
    deepseq vpun1 $ deepseq vpun2 $ return ()
    deepseq vpuj1 $ deepseq vpuj2 $ return ()
    deepseq vprj1 $ deepseq vprj2 $ return ()
    seq ba1 $ seq ba2 $ return ()

    defaultMainWith critConfig (return ())
        [ bgroup "diff1"
            [ bench "VU.Vector"                 $ nf (distance_Vector_diff1 vu1) vu2
            , bench "VPU.Vector Nothing"        $ nf (distance_Vector_diff1 vpun1) vpun2
            , bench "VPU.Vector (Just Arrlen)"  $ nf (distance_Vector_diff1 vpuj1) vpuj2
            , bench "VPR.Vector (Just Arrlen)"  $ nf (distance_Vector_diff1 vprj1) vprj2
            , bench "ByteArray"                 $ nf (distance_ByteArray_diff1 ba1) ba2
            ]
        , bgroup "diff4"
            [ bench "VU.Vector"                 $ nf (distance_Vector_diff4 vu1) vu2
            , bench "VPU.Vector Nothing"        $ nf (distance_Vector_diff4 vpun1) vpun2
            , bench "VPU.Vector (Just Arrlen)"  $ nf (distance_Vector_diff4 vpuj1) vpuj2
            , bench "VPR.Vector (Just Arrlen)"  $ nf (distance_Vector_diff4 vprj1) vprj2
            , bench "ByteArray"                 $ nf (distance_ByteArray_diff4 ba1) ba2
            ]
        ]

critConfig = defaultConfig 
        { cfgPerformGC   = ljust True
        , cfgSamples     = ljust 1000
        }

-------------------------------------------------------------------------------
-- test functions 

{-# INLINE distance_Vector_diff1 #-}
distance_Vector_diff1 :: (Show f,VG.Vector v f, Floating f) => v f -> v f -> f
distance_Vector_diff1 !v1 !v2 = sqrt $ go 0 (VG.length v1-1)
    where
        go tot (-1) = tot
        go tot i = go (tot+diff1*diff1
                      ) (i-1)
            where 
                diff1 = v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i

{-# INLINE distance_Vector_diff4 #-}
distance_Vector_diff4 :: (Show f,VG.Vector v f, Floating f) => v f -> v f -> f
distance_Vector_diff4 !v1 !v2 = sqrt $ go 0 (VG.length v1-1)
    where
        go tot (-1) = tot
        go tot i = go (tot+diff1*diff1
                          +diff2*diff2
                          +diff3*diff3
                          +diff4*diff4
                      ) (i-4)
            where 
                diff1 = v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i
                diff2 = v1 `VG.unsafeIndex` (i-1)-v2 `VG.unsafeIndex` (i-1)
                diff3 = v1 `VG.unsafeIndex` (i-2)-v2 `VG.unsafeIndex` (i-2)
                diff4 = v1 `VG.unsafeIndex` (i-3)-v2 `VG.unsafeIndex` (i-3)

---------------------------------------

list2ByteArray xs = runST $ do
    arr <- newAlignedPinnedByteArray (2^16) (arrlen*4)
    forM (zip [0..] xs) $ \(i,x) -> do
        writeByteArray arr i x
    unsafeFreezeByteArray arr

{-# INLINE distance_ByteArray_diff1 #-}
distance_ByteArray_diff1 :: ByteArray -> ByteArray -> Float
distance_ByteArray_diff1 !a1 !a2 = sqrt $ go 0 (arrlen-1)
    where
        go tot (-1) = tot
        go tot i = go (tot+diff1*diff1
                      ) (i-1)
            where
                diff1 = (a1 `indexByteArray` i)-(a2 `indexByteArray` i)

{-# INLINE distance_ByteArray_diff4 #-}
distance_ByteArray_diff4 :: ByteArray -> ByteArray -> Float
distance_ByteArray_diff4 !a1 !a2 = sqrt $ go 0 (arrlen-1)
    where
        go tot (-1) = tot
        go tot i = go (tot+diff1*diff1
                          +diff2*diff2
                          +diff3*diff3
                          +diff4*diff4
                      ) (i-4)
            where
                diff1 = (a1 `indexByteArray` i)-(a2 `indexByteArray` i)
                diff2 = (a1 `indexByteArray` (i-1))-(a2 `indexByteArray` (i-1))
                diff3 = (a1 `indexByteArray` (i-3))-(a2 `indexByteArray` (i-3))
                diff4 = (a1 `indexByteArray` (i-4))-(a2 `indexByteArray` (i-4))
