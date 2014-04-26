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
import qualified Data.Params.Vector.Storable as VPS
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V

-------------------------------------------------------------------------------
-- criterion tests

critConfig = defaultConfig 
        { cfgPerformGC   = ljust True
        , cfgSamples     = ljust 100
        }

---------------------------------------
-- vector size constants

arrlen = 20 :: Int
type Arrlen = 20

numvec = 10000 :: Int
type Numvec = 10000

---------------------------------------
-- main function
main = do
    
    -- single vectors

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

    let vpsj1 = VG.fromList dimL1 :: VPR.Vector (Just Arrlen) Float
        vpsj2 = VG.fromList dimL2 :: VPR.Vector (Just Arrlen) Float

    let ba1 = list2ByteArray dimL1
        ba2 = list2ByteArray dimL2

    deepseq vu1 $ deepseq vu2 $ return ()
    deepseq vpun1 $ deepseq vpun2 $ return ()
    deepseq vpuj1 $ deepseq vpuj2 $ return ()
    deepseq vprj1 $ deepseq vprj2 $ return ()
    deepseq vpsj1 $ deepseq vpsj2 $ return ()
    seq ba1 $ seq ba2 $ return ()

    -- vectors of vectors

    let dimLL1 :: [[Float]] = 
            [ evalRand (replicateM arrlen $ getRandomR (-10000,10000)) (mkStdGen i) | i <- [1..numvec]]

    let dimLL2 :: [[Float]] = 
            [ evalRand (replicateM arrlen $ getRandomR (-10000,10000)) (mkStdGen i) | i <- [2..numvec+1]]

    let vpunvpuj1 = VG.fromList $ map VG.fromList dimLL1 
            :: VPU.Vector Nothing (VPU.Vector (Just Arrlen) Float)
    let vpunvpuj2 = VG.fromList $ map VG.fromList dimLL2 
            :: VPU.Vector Nothing (VPU.Vector (Just Arrlen) Float)

    let vpujvpuj1 = VG.fromList $ map VG.fromList dimLL1 
            :: VPU.Vector (Just Numvec) (VPU.Vector (Just Arrlen) Float)
    let vpujvpuj2 = VG.fromList $ map VG.fromList dimLL2 
            :: VPU.Vector (Just Numvec) (VPU.Vector (Just Arrlen) Float)

    let vprjvpuj1 = VG.fromList $ map VG.fromList dimLL1 
            :: VPR.Vector (Just Numvec) (VPU.Vector (Just Arrlen) Float)
    let vprjvpuj2 = VG.fromList $ map VG.fromList dimLL2 
            :: VPR.Vector (Just Numvec) (VPU.Vector (Just Arrlen) Float)

    let vpsjvpsj1 = VG.fromList $ map VG.fromList dimLL1 
            :: VPS.Vector (Just Numvec) (VPS.Vector (Just Arrlen) Float)
    let vpsjvpsj2 = VG.fromList $ map VG.fromList dimLL2 
            :: VPS.Vector (Just Numvec) (VPS.Vector (Just Arrlen) Float)

    let vvpuj1 = VG.fromList $ map VG.fromList dimLL1 
            :: V.Vector (VPU.Vector (Just Arrlen) Float)
    let vvpuj2 = VG.fromList $ map VG.fromList dimLL2 
            :: V.Vector (VPU.Vector (Just Arrlen) Float)

    deepseq vpunvpuj1 $ deepseq vpunvpuj2 $ return ()
    deepseq vpujvpuj1 $ deepseq vpujvpuj2 $ return ()
    deepseq vprjvpuj1 $ deepseq vprjvpuj2 $ return ()
    deepseq vpsjvpsj1 $ deepseq vpsjvpsj2 $ return ()
    deepseq vvpuj1 $ deepseq vvpuj2 $ return ()

    -- tests

    defaultMainWith critConfig (return ())
        [ bgroup "1d"
            [ bgroup "diff1"
                [ bench "VU.Vector"                 $ nf (distance_Vector_diff1 vu1) vu2
                , bench "VPU.Vector Nothing"        $ nf (distance_Vector_diff1 vpun1) vpun2
                , bench "VPU.Vector (Just Arrlen)"  $ nf (distance_Vector_diff1 vpuj1) vpuj2
                , bench "VPR.Vector (Just Arrlen)"  $ nf (distance_Vector_diff1 vprj1) vprj2
                , bench "VPS.Vector (Just Arrlen)"  $ nf (distance_Vector_diff1 vpsj1) vpsj2
                , bench "ByteArray"                 $ nf (distance_ByteArray_diff1 ba1) ba2
                ]
            , bgroup "diff4"
                [ bench "VU.Vector"                 $ nf (distance_Vector_diff4 vu1) vu2
                , bench "VPU.Vector Nothing"        $ nf (distance_Vector_diff4 vpun1) vpun2
                , bench "VPU.Vector (Just Arrlen)"  $ nf (distance_Vector_diff4 vpuj1) vpuj2
                , bench "VPR.Vector (Just Arrlen)"  $ nf (distance_Vector_diff4 vprj1) vprj2
                , bench "VPS.Vector (Just Arrlen)"  $ nf (distance_Vector_diff4 vpsj1) vpsj2
                , bench "ByteArray"                 $ nf (distance_ByteArray_diff4 ba1) ba2
                ]
            ]
        , bgroup "2d"
            [ bgroup "diff4"
                [ bench "VPU.Vector Nothing (VPU.Vector (Just Arrlen))" 
                    $ nf (distance_VectorVector distance_Vector_diff4 vpunvpuj1) vpunvpuj2
                , bench "VPU.Vector (Just Numvec) (VPU.Vector (Just Arrlen))" 
                    $ nf (distance_VectorVector distance_Vector_diff4 vpujvpuj1) vpujvpuj2
                , bench "VPR.Vector (Just Numvec) (VPU.Vector (Just Arrlen))" 
                    $ nf (distance_VectorVector distance_Vector_diff4 vprjvpuj1) vprjvpuj2
                , bench "VPS.Vector (Just Numvec) (VPS.Vector (Just Arrlen))" 
                    $ nf (distance_VectorVector distance_Vector_diff4 vpsjvpsj1) vpsjvpsj2
                , bench "V.Vector (VPU.Vector (Just Arrlen))" 
                    $ nf (distance_VectorVector distance_Vector_diff4 vvpuj1) vvpuj2
                ]
            ]
        ]

-------------------------------------------------------------------------------
-- test functions 

{-# INLINE distance_VectorVector #-}
distance_VectorVector :: 
    ( VG.Vector v1 (v2 f)
    , VG.Vector v2 f
    , Floating f
    ) => (v2 f -> v2 f -> f) -> v1 (v2 f) -> v1 (v2 f) -> f
distance_VectorVector dist vv1 vv2 = go 0 (VG.length vv1-1)
    where
        go tot 0 = tot
        go tot i = dist (vv1 VG.! i) (vv2 VG.! i)
                 + go tot (i-1)

---------------------------------------

{-# INLINE distance_Vector_diff1 #-}
distance_Vector_diff1 :: (VG.Vector v f, Floating f) => v f -> v f -> f
distance_Vector_diff1 !v1 !v2 = sqrt $ go 0 (VG.length v1-1)
    where
        go tot (-1) = tot
        go tot i = go (tot+diff1*diff1
                      ) (i-1)
            where 
                diff1 = v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i

{-# INLINE distance_Vector_diff4 #-}
distance_Vector_diff4 :: (VG.Vector v f, Floating f) => v f -> v f -> f
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
