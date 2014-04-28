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

-- size of each vector to test; must be divisible by 4
veclen = 20 :: Int
type Veclen = 20

-- number of vectors in 2d tests
numvec = 10000 :: Int
type Numvec = 10000

-- criterion configuration parameters
critConfig = defaultConfig 
    { cfgPerformGC   = ljust True
    , cfgSamples     = ljust 100
    , cfgSummaryFile = ljust $ "results/summary-"++show veclen++"-"++show numvec++".csv"
--     , cfgReport      = ljust "report.html"
    }

---------------------------------------
-- main function
main = do
    
    -- single vectors

    let dimL1 :: [Float] = evalRand (replicateM veclen $ getRandomR (-10000,10000)) (mkStdGen $ 1)
        dimL2 :: [Float] = evalRand (replicateM veclen $ getRandomR (-10000,10000)) (mkStdGen $ 2)

    let vu1 = VU.fromList dimL1
        vu2 = VU.fromList dimL2

    let vpua1 = VG.fromList dimL1 :: VPU.Vector Automatic Float
        vpua2 = VG.fromList dimL2 :: VPU.Vector Automatic Float

    let vpus1 = VG.fromList dimL1 :: VPU.Vector (Static Veclen) Float
        vpus2 = VG.fromList dimL2 :: VPU.Vector (Static Veclen) Float

--     let vpur1 = withParam (VPU.len veclen) $ VG.fromList dimL1 :: VPU.Vector Automatic Float
--         vpur2 = withParam (VPU.len veclen) $ VG.fromList dimL2 :: VPU.Vector Automatic Float

    let vprs1 = VG.fromList dimL1 :: VPR.Vector (Static Veclen) Float
        vprs2 = VG.fromList dimL2 :: VPR.Vector (Static Veclen) Float

    let vpss1 = VG.fromList dimL1 :: VPR.Vector (Static Veclen) Float
        vpss2 = VG.fromList dimL2 :: VPR.Vector (Static Veclen) Float

    let ba1 = list2ByteArray dimL1
        ba2 = list2ByteArray dimL2

    deepseq vu1 $ deepseq vu2 $ return ()
    deepseq vpua1 $ deepseq vpua2 $ return ()
    deepseq vpus1 $ deepseq vpus2 $ return ()
    deepseq vprs1 $ deepseq vprs2 $ return ()
    deepseq vpss1 $ deepseq vpss2 $ return ()
    seq ba1 $ seq ba2 $ return ()

    -- vectors of vectors

    let dimLL1 :: [[Float]] = 
            [ evalRand (replicateM veclen $ getRandomR (-10000,10000)) (mkStdGen i) | i <- [1..numvec]]

    let dimLL2 :: [[Float]] = 
            [ evalRand (replicateM veclen $ getRandomR (-10000,10000)) (mkStdGen i) | i <- [2..numvec+1]]

    let vpuavpus1 = VG.fromList $ map VG.fromList dimLL1 
            :: VPU.Vector Automatic (VPU.Vector (Static Veclen) Float)
    let vpuavpus2 = VG.fromList $ map VG.fromList dimLL2 
            :: VPU.Vector Automatic (VPU.Vector (Static Veclen) Float)

    let vpusvpus1 = VG.fromList $ map VG.fromList dimLL1 
            :: VPU.Vector (Static Numvec) (VPU.Vector (Static Veclen) Float)
    let vpusvpus2 = VG.fromList $ map VG.fromList dimLL2 
            :: VPU.Vector (Static Numvec) (VPU.Vector (Static Veclen) Float)

    let vprsvpus1 = VG.fromList $ map VG.fromList dimLL1 
            :: VPR.Vector (Static Numvec) (VPU.Vector (Static Veclen) Float)
    let vprsvpus2 = VG.fromList $ map VG.fromList dimLL2 
            :: VPR.Vector (Static Numvec) (VPU.Vector (Static Veclen) Float)

    let vpssvpss1 = VG.fromList $ map VG.fromList dimLL1 
            :: VPS.Vector (Static Numvec) (VPS.Vector (Static Veclen) Float)
    let vpssvpss2 = VG.fromList $ map VG.fromList dimLL2 
            :: VPS.Vector (Static Numvec) (VPS.Vector (Static Veclen) Float)

    let vvpus1 = VG.fromList $ map VG.fromList dimLL1 
            :: V.Vector (VPU.Vector (Static Veclen) Float)
    let vvpus2 = VG.fromList $ map VG.fromList dimLL2 
            :: V.Vector (VPU.Vector (Static Veclen) Float)

    deepseq vpuavpus1 $ deepseq vpuavpus2 $ return ()
    deepseq vpusvpus1 $ deepseq vpusvpus2 $ return ()
    deepseq vprsvpus1 $ deepseq vprsvpus2 $ return ()
    deepseq vpssvpss1 $ deepseq vpssvpss2 $ return ()
    deepseq vvpus1 $ deepseq vvpus2 $ return ()

    -- tests

    defaultMainWith critConfig (return ())
        [ bgroup "1d"
            [ bgroup "diff1"
                [ bench "VU.Vector"                 $ nf (distance_Vector_diff1 vu1) vu2
                , bench "VPU.Vector Automatic"        $ nf (distance_Vector_diff1 vpua1) vpua2
                , bench "VPU.Vector (Static Veclen)"  $ nf (distance_Vector_diff1 vpus1) vpus2
                , bench "VPR.Vector (Static Veclen)"  $ nf (distance_Vector_diff1 vprs1) vprs2
                , bench "VPS.Vector (Static Veclen)"  $ nf (distance_Vector_diff1 vpss1) vpss2
                , bench "ByteArray"                 $ nf (distance_ByteArray_diff1 ba1) ba2
                ]
            , bgroup "diff4"
                [ bench "VU.Vector"                 $ nf (distance_Vector_diff4 vu1) vu2
                , bench "VPU.Vector Automatic"        $ nf (distance_Vector_diff4 vpua1) vpua2
                , bench "VPU.Vector (Static Veclen)"  $ nf (distance_Vector_diff4 vpus1) vpus2
                , bench "VPR.Vector (Static Veclen)"  $ nf (distance_Vector_diff4 vprs1) vprs2
                , bench "VPS.Vector (Static Veclen)"  $ nf (distance_Vector_diff4 vpss1) vpss2
                , bench "ByteArray"                 $ nf (distance_ByteArray_diff4 ba1) ba2
                ]
            ]
        , bgroup "2d"
            [ bgroup "diff4"
                [ bench "VPU.Vector Automatic (VPU.Vector (Static Veclen))" 
                    $ nf (distance_VectorVector distance_Vector_diff4 vpuavpus1) vpuavpus2
                , bench "VPU.Vector (Static Numvec) (VPU.Vector (Static Veclen))" 
                    $ nf (distance_VectorVector distance_Vector_diff4 vpusvpus1) vpusvpus2
                , bench "VPR.Vector (Static Numvec) (VPU.Vector (Static Veclen))" 
                    $ nf (distance_VectorVector distance_Vector_diff4 vprsvpus1) vprsvpus2
                , bench "VPS.Vector (Static Numvec) (VPS.Vector (Static Veclen))" 
                    $ nf (distance_VectorVector distance_Vector_diff4 vpssvpss1) vpssvpss2
                , bench "V.Vector (VPU.Vector (Static Veclen))" 
                    $ nf (distance_VectorVector distance_Vector_diff4 vvpus1) vvpus2
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
    arr <- newAlignedPinnedByteArray (2^16) (veclen*4)
    forM (zip [0..] xs) $ \(i,x) -> do
        writeByteArray arr i x
    unsafeFreezeByteArray arr

{-# INLINE distance_ByteArray_diff1 #-}
distance_ByteArray_diff1 :: ByteArray -> ByteArray -> Float
distance_ByteArray_diff1 !a1 !a2 = sqrt $ go 0 (veclen-1)
    where
        go tot (-1) = tot
        go tot i = go (tot+diff1*diff1
                      ) (i-1)
            where
                diff1 = (a1 `indexByteArray` i)-(a2 `indexByteArray` i)

{-# INLINE distance_ByteArray_diff4 #-}
distance_ByteArray_diff4 :: ByteArray -> ByteArray -> Float
distance_ByteArray_diff4 !a1 !a2 = sqrt $ go 0 (veclen-1)
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
