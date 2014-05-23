{-# OPTION_GHC -O2 -fllvm #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE FlexibleContexts #-}

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
import qualified Data.Params.Vector.UnboxedRaw as VPUR
import qualified Data.Params.Vector.Storable as VPS
import qualified Data.Params.Vector.Storable as VPSR
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V

import GHC.Float
import GHC.Int
import GHC.Base (Int (..))
import GHC.Prim

-------------------------------------------------------------------------------
-- criterion tests

-- | size of each vector to test; must be divisible by 4
type Veclen = 400
veclen = fromIntegral $ natVal (Proxy::Proxy Veclen)

-- | number of vectors in 2d tests
type Numvec = 100
numvec = fromIntegral $ natVal (Proxy::Proxy Numvec)

-- | numeric type to test against
type NumType = Float

-- | criterion configuration parameters
critConfig = defaultConfig 
    { cfgPerformGC   = ljust True
    , cfgSamples     = ljust 1000
--     , cfgSummaryFile = ljust $ "results/summary-"++show veclen++"-"++show numvec++".csv"
--     , cfgReport      = ljust "report.html"
    }

-------------------------------------------------------------------------------
-- main function

main = do
    
    -----------------------------------
    -- initialize single vectors

    putStrLn "constructing single vectors"

    let dimL1 :: [NumType] = evalRand (replicateM veclen $ getRandomR (-10000,10000)) (mkStdGen $ 1)
        dimL2 :: [NumType] = evalRand (replicateM veclen $ getRandomR (-10000,10000)) (mkStdGen $ 2)

    let vu1 = VU.fromList dimL1
        vu2 = VU.fromList dimL2

    let vpua1 = VG.fromList dimL1 :: VPU.Vector Automatic NumType
        vpua2 = VG.fromList dimL2 :: VPU.Vector Automatic NumType

    let vpus1 = VG.fromList dimL1 :: VPU.Vector (Static Veclen) NumType
        vpus2 = VG.fromList dimL2 :: VPU.Vector (Static Veclen) NumType

--     let vpur1 = withParam (VPU.len veclen) $ VG.fromList dimL1 :: VPU.Vector Automatic NumType
--         vpur2 = withParam (VPU.len veclen) $ VG.fromList dimL2 :: VPU.Vector Automatic NumType

    let vprs1 = VG.fromList dimL1 :: VPUR.Vector (Static Veclen) NumType
        vprs2 = VG.fromList dimL2 :: VPUR.Vector (Static Veclen) NumType

    let vpss1 = VG.fromList dimL1 :: VPS.Vector (Static Veclen) NumType
        vpss2 = VG.fromList dimL2 :: VPS.Vector (Static Veclen) NumType

    let vpsr1 = VG.fromList dimL1 :: VPSR.Vector (Static Veclen) NumType
        vpsr2 = VG.fromList dimL2 :: VPSR.Vector (Static Veclen) NumType

    let ba1 = list2ByteArray dimL1
        ba2 = list2ByteArray dimL2

    deepseq vu1 $ deepseq vu2 $ return ()
    deepseq vpua1 $ deepseq vpua2 $ return ()
    deepseq vpus1 $ deepseq vpus2 $ return ()
    deepseq vprs1 $ deepseq vprs2 $ return ()
    deepseq vpss1 $ deepseq vpss2 $ return ()
    deepseq vpsr1 $ deepseq vpsr2 $ return ()
    seq ba1 $ seq ba2 $ return ()

    -----------------------------------
    -- initialize 2d vectors

    putStrLn "constructing 2d vectors of vectors"

    let dimLL1 :: [[NumType]] = 
            [ evalRand (replicateM veclen $ getRandomR (-10000,10000)) (mkStdGen i) | i <- [1..numvec]]

    let dimLL2 :: [[NumType]] = 
            [ evalRand (replicateM veclen $ getRandomR (-10000,10000)) (mkStdGen i) | i <- [2..numvec+1]]

    putStrLn "  generating random numbers"
    deepseq dimLL1 $ deepseq dimLL2 $ return ()

    let vpuavpus1 = VG.fromList $ map VG.fromList dimLL1 
            :: VPU.Vector Automatic (VPU.Vector (Static Veclen) NumType)
    let vpuavpus2 = VG.fromList $ map VG.fromList dimLL2 
            :: VPU.Vector Automatic (VPU.Vector (Static Veclen) NumType)

    let vpuavpur1 = VPU.withParam3 (VPU._elem.VPU._len $ veclen) $ VG.fromList $ map VG.fromList dimLL1
            :: VPU.Vector Automatic (VPU.Vector RunTime NumType)
    let vpuavpur2 = VPU.withParam3 (VPU._elem.VPU._len $ veclen) $ VG.fromList $ map VG.fromList dimLL2
            :: VPU.Vector Automatic (VPU.Vector RunTime NumType)

    let vpusvpus1 = VG.fromList $ map VG.fromList dimLL1 
            :: VPU.Vector (Static Numvec) (VPU.Vector (Static Veclen) NumType)
    let vpusvpus2 = VG.fromList $ map VG.fromList dimLL2 
            :: VPU.Vector (Static Numvec) (VPU.Vector (Static Veclen) NumType)

    let vprsvpus1 = VG.fromList $ map VG.fromList dimLL1 
            :: VPUR.Vector (Static Numvec) (VPU.Vector (Static Veclen) NumType)
    let vprsvpus2 = VG.fromList $ map VG.fromList dimLL2 
            :: VPUR.Vector (Static Numvec) (VPU.Vector (Static Veclen) NumType)

    let vpssvpss1 = VG.fromList $ map VG.fromList dimLL1 
            :: VPS.Vector (Static Numvec) (VPS.Vector (Static Veclen) NumType)
    let vpssvpss2 = VG.fromList $ map VG.fromList dimLL2 
            :: VPS.Vector (Static Numvec) (VPS.Vector (Static Veclen) NumType)

    let vpsrvpsr1 = VG.fromList $ map VG.fromList dimLL1 
            :: VPSR.Vector (Static Numvec) (VPSR.Vector (Static Veclen) NumType)
    let vpsrvpsr2 = VG.fromList $ map VG.fromList dimLL2 
            :: VPSR.Vector (Static Numvec) (VPSR.Vector (Static Veclen) NumType)

    let vpusvpua1 = VPU.ss2sa $ (VG.fromList $ map VG.fromList dimLL1
            :: VPU.Vector (Static Numvec) (VPU.Vector (Static Veclen) NumType))
        vpusvpua2 = VPU.ss2sa $ (VG.fromList $ map VG.fromList dimLL2
            :: VPU.Vector (Static Numvec) (VPU.Vector (Static Veclen) NumType))

    let vpuavpua1 = VPU.ss2aa $ (VG.fromList $ map VG.fromList dimLL1
            :: VPU.Vector (Static Numvec) (VPU.Vector (Static Veclen) NumType))
        vpuavpua2 = VPU.ss2aa $ (VG.fromList $ map VG.fromList dimLL2
            :: VPU.Vector (Static Numvec) (VPU.Vector (Static Veclen) NumType))

    let vvpus1 = VG.fromList $ map VG.fromList dimLL1 
            :: V.Vector (VPU.Vector (Static Veclen) NumType)
    let vvpus2 = VG.fromList $ map VG.fromList dimLL2 
            :: V.Vector (VPU.Vector (Static Veclen) NumType)

    let vvu1 = VG.fromList $ map VG.fromList dimLL1
            :: V.Vector (VU.Vector NumType)
        vvu2 = VG.fromList $ map VG.fromList dimLL2
            :: V.Vector (VU.Vector NumType)

    putStrLn "  vpuavpus"; deepseq vpuavpus1 $ deepseq vpuavpus2 $ return ()
    putStrLn "  vpusvpus"; deepseq vpusvpus1 $ deepseq vpusvpus2 $ return ()
    putStrLn "  vpssvpss"; deepseq vpssvpss1 $ deepseq vpssvpss2 $ return ()
    putStrLn "  vpsrvpsr"; deepseq vpsrvpsr1 $ deepseq vpsrvpsr2 $ return ()
    putStrLn "  vpusvpua"; deepseq vpusvpua1 $ deepseq vpusvpua2 $ return ()
    putStrLn "  vpuavpua"; deepseq vpuavpua1 $ deepseq vpuavpua2 $ return ()
    putStrLn "  vprsvpus"; deepseq vprsvpus1 $ deepseq vprsvpus2 $ return ()
    putStrLn "  vpuavpus"; deepseq vpssvpss1 $ deepseq vpssvpss2 $ return ()
    putStrLn "  vvpus"; deepseq vvpus1 $ deepseq vvpus2 $ return ()
    putStrLn "  vvu"; deepseq vvu1 $ deepseq vvu2 $ return ()

    -----------------------------------
    -- tests

    putStrLn "starting criterion"

    defaultMainWith critConfig (return ())
        [ bgroup "1d"
            [ bgroup "diff1"
                [ bench "VU.Vector"                 $ nf (distance_Vector_diff1 vu1) vu2
                , bench "VPU.Vector Automatic"        $ nf (distance_Vector_diff1 vpua1) vpua2
                , bench "VPU.Vector (Static Veclen)"  $ nf (distance_Vector_diff1 vpus1) vpus2
                , bench "VPUR.Vector (Static Veclen)"  $ nf (distance_Vector_diff1 vprs1) vprs2
                , bench "VPS.Vector (Static Veclen)"  $ nf (distance_Vector_diff1 vpss1) vpss2
                , bench "VPSR.Vector (Static Veclen)"  $ nf (distance_Vector_diff1 vpsr1) vpsr2
                , bench "ByteArray"                 $ nf (distance_ByteArray_diff1 ba1) ba2
                ]
            , bgroup "diff4"
                [ bench "VU.Vector"                 $ nf (distance_Vector_diff4 vu1) vu2
                , bench "VPU.Vector Automatic"        $ nf (distance_Vector_diff4 vpua1) vpua2
                , bench "VPU.Vector (Static Veclen)"  $ nf (distance_Vector_diff4 vpus1) vpus2
                , bench "VPUR.Vector (Static Veclen)"  $ nf (distance_Vector_diff4 vprs1) vprs2
                , bench "VPS.Vector (Static Veclen)"  $ nf (distance_Vector_diff4 vpss1) vpss2
                , bench "VPSR.Vector (Static Veclen)"  $ nf (distance_Vector_diff4 vpsr1) vpsr2
                , bench "ByteArray"                 $ nf (distance_ByteArray_diff4 ba1) ba2
                ]
            , bgroup "simd"
                [ bgroup "ByteArray"
                    [ bench "diff1-ByteArray"                 $ nf (distance_ByteArray_diff1 ba1) ba2
                    , bench "simd4-ByteArray"                 $ nf (distance_ByteArray_simd4 ba1) ba2
                    , bench "simd8-ByteArray"                 $ nf (distance_ByteArray_simd8 ba1) ba2
                    , bench "simd8'-ByteArray"                 $ nf (distance_ByteArray_simd8' ba1) ba2
                    , bench "simd16-ByteArray"                 $ nf (distance_ByteArray_simd16 ba1) ba2
                    , bench "simd16'-ByteArray"                 $ nf (distance_ByteArray_simd16' ba1) ba2
                    ]
                , bgroup "VU.Vector"
                    [ bench "diff4-VU.Vector"                 $ nf (distance_Vector_diff4 vu1) vu2
                    , bench "simd4-VU.Vector"                 $ nf (distance_Vector_simd4 vu1) vu2
                    , bench "simd8-VU.Vector"                 $ nf (distance_Vector_simd8 vu1) vu2
                    , bench "simd16-VU.Vector"                $ nf (distance_Vector_simd16 vu1) vu2
                    ]
                ]
            ]
        , bgroup "pairwise"
            [ bgroup "diff4"
                [ bench "VPU.Vector Automatic (VPU.Vector (Static Veclen))" 
                    $ nf (distance_pairwise distance_Vector_diff4 vpuavpus1) vpuavpus2
                , bench "VPU.Vector (Static Numvec) (VPU.Vector (Static Veclen))" 
                    $ nf (distance_pairwise distance_Vector_diff4 vpusvpus1) vpusvpus2
                , bench "VPU.Vector (Static Numvec) (VPU.Vector Automatic)" 
                    $ nf (distance_pairwise distance_Vector_diff4 vpusvpua1) vpusvpua2
                , bench "VPU.Vector Automatic (VPU.Vector Automatic)" 
                    $ nf (distance_pairwise distance_Vector_diff4 vpuavpua1) vpuavpua2
                , bench "VPUR.Vector (Static Numvec) (VPU.Vector (Static Veclen))" 
                    $ nf (distance_pairwise distance_Vector_diff4 vprsvpus1) vprsvpus2
                , bench "VPS.Vector (Static Numvec) (VPS.Vector (Static Veclen))" 
                    $ nf (distance_pairwise distance_Vector_diff4 vpssvpss1) vpssvpss2
                , bench "VPSR.Vector (Static Numvec) (VPSR.Vector (Static Veclen))" 
                    $ nf (distance_pairwise distance_Vector_diff4 vpsrvpsr1) vpsrvpsr2
                , bench "V.Vector (VPU.Vector (Static Veclen))" 
                    $ nf (distance_pairwise distance_Vector_diff4 vvpus1) vvpus2
                , bench "V.Vector VU.Vector" 
                    $ nf (distance_pairwise distance_Vector_diff4 vvu1) vvu2
                ]
            ]
        , bgroup "allToAll"
            [ bgroup "diff4"
                [ bench "VPU.Vector Automatic (VPU.Vector (Static Veclen))" 
                    $ nf (distance_allToAll distance_Vector_diff4 vpuavpus1) vpuavpus2
                , bench "VPU.Vector (Static Numvec) (VPU.Vector (Static Veclen))" 
                    $ nf (distance_allToAll distance_Vector_diff4 vpusvpus1) vpusvpus2
                , bench "VPU.Vector (Static Numvec) (VPU.Vector Automatic)" 
                    $ nf (distance_allToAll distance_Vector_diff4 vpusvpua1) vpusvpua2
                , bench "VPU.Vector Automatic (VPU.Vector Automatic)" 
                    $ nf (distance_allToAll distance_Vector_diff4 vpuavpua1) vpuavpua2
--                 , bench "VPUR.Vector (Static Numvec) (VPU.Vector (Static Veclen))" 
--                     $ nf (distance_allToAll distance_Vector_diff4 vprsvpus1) vprsvpus2
                , bench "VPS.Vector (Static Numvec) (VPS.Vector (Static Veclen))" 
                    $ nf (distance_allToAll distance_Vector_diff4 vpssvpss1) vpssvpss2
                , bench "VPSR.Vector (Static Numvec) (VPSR.Vector (Static Veclen))" 
                    $ nf (distance_allToAll distance_Vector_diff4 vpsrvpsr1) vpsrvpsr2
                , bench "V.Vector (VPU.Vector (Static Veclen))" 
                    $ nf (distance_allToAll distance_Vector_diff4 vvpus1) vvpus2
                , bench "V.Vector VU.Vector" 
                    $ nf (distance_allToAll distance_Vector_diff4 vvu1) vvu2
                ]
            ]
        ]

-------------------------------------------------------------------------------
-- test functions 

-- | sums the pairwise distance between elements in the two vectors in time O(n)
distance_pairwise :: 
    ( VG.Vector v1 (v2 f)
    , VG.Vector v2 f
    , Floating f
    ) => (v2 f -> v2 f -> f) -> v1 (v2 f) -> v1 (v2 f) -> f
distance_pairwise dist vv1 vv2 = go 0 (VG.length vv1-1)
    where
        go tot (-1) = tot
        go tot i = dist (vv1 `VG.unsafeIndex` i) (vv2 `VG.unsafeIndex` i)
                 + go tot (i-1)

-- | sums the distance between a point and every point in a vector in time O(n)
distance_oneToAll ::
    ( VG.Vector v1 (v2 f)
    , VG.Vector v2 f
    , Floating f
    ) => (v2 f -> v2 f -> f) -> v2 f -> v1 (v2 f) -> f
distance_oneToAll dist v vv = go 0 (VG.length vv-1)
    where
        go tot (-1) = tot
        go tot i = go tot' (i-1)
            where
                tot' = tot + dist v (vv `VG.unsafeIndex` i)

-- | sums the distance between every point in vv1 and every point in vv2 in time O(n^2)
distance_allToAll ::
    ( VG.Vector v1 (v2 f)
    , VG.Vector v2 f
    , Floating f
    ) => (v2 f -> v2 f -> f) -> v1 (v2 f) -> v1 (v2 f) -> f
distance_allToAll dist vv1 vv2 = go 0 (VG.length vv1-1)
    where
        go tot (-1) = tot
        go tot i = go tot' (i-1)
            where
                tot' = tot + distance_oneToAll dist (vv1 `VG.unsafeIndex` i) vv2

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

{-# INLINE distance_Vector_simd4 #-}
distance_Vector_simd4 :: (VG.Vector v Float) => v Float -> v Float -> Float
distance_Vector_simd4 !v1 !v2 = sqrt $ sum4 (go zeros (VG.length v1-1))
    where
        zeros = broadcastFloatX4# (unFloat 0)

        go tot (-1) = tot
        go tot i = go tot' (i-4)
            where 
                tot' = plusFloatX4# tot sqrarr
                sqrarr = timesFloatX4# minarr minarr
                minarr = minusFloatX4# arr1 arr2

                arr1 = packFloatX4# (# e1_1, e1_2, e1_3, e1_4 #)
                arr2 = packFloatX4# (# e2_1, e2_2, e2_3, e2_4 #)

                e1_1 = unFloat $ v1 `VG.unsafeIndex` i
                e1_2 = unFloat $ v1 `VG.unsafeIndex` (i-1)
                e1_3 = unFloat $ v1 `VG.unsafeIndex` (i-2)
                e1_4 = unFloat $ v1 `VG.unsafeIndex` (i-3)
                e2_1 = unFloat $ v1 `VG.unsafeIndex` i
                e2_2 = unFloat $ v1 `VG.unsafeIndex` (i-1)
                e2_3 = unFloat $ v1 `VG.unsafeIndex` (i-2)
                e2_4 = unFloat $ v1 `VG.unsafeIndex` (i-3)

{-# INLINE distance_Vector_simd8 #-}
distance_Vector_simd8 :: (VG.Vector v Float) => v Float -> v Float -> Float
distance_Vector_simd8 !v1 !v2 = sqrt $ sum8 (go zeros (VG.length v1-1))
    where
        zeros = broadcastFloatX8# (unFloat 0)

        go tot (-1) = tot
        go tot i = go tot' (i-8)
            where 
                tot' = plusFloatX8# tot sqrarr
                sqrarr = timesFloatX8# minarr minarr
                minarr = minusFloatX8# arr1 arr2

                arr1 = packFloatX8# (# e1_1, e1_2, e1_3, e1_4 
                                    ,  e1_5, e1_6, e1_7, e1_8 #)
                arr2 = packFloatX8# (# e2_1, e2_2, e2_3, e2_4
                                    ,  e2_5, e2_6, e2_7, e2_8 #)

                e1_1 = unFloat $ v1 `VG.unsafeIndex` i
                e1_2 = unFloat $ v1 `VG.unsafeIndex` (i-1)
                e1_3 = unFloat $ v1 `VG.unsafeIndex` (i-2)
                e1_4 = unFloat $ v1 `VG.unsafeIndex` (i-3)
                e1_5 = unFloat $ v1 `VG.unsafeIndex` (i-4)
                e1_6 = unFloat $ v1 `VG.unsafeIndex` (i-5)
                e1_7 = unFloat $ v1 `VG.unsafeIndex` (i-6)
                e1_8 = unFloat $ v1 `VG.unsafeIndex` (i-7)

                e2_1 = unFloat $ v2 `VG.unsafeIndex` i
                e2_2 = unFloat $ v2 `VG.unsafeIndex` (i-1)
                e2_3 = unFloat $ v2 `VG.unsafeIndex` (i-2)
                e2_4 = unFloat $ v2 `VG.unsafeIndex` (i-3)
                e2_5 = unFloat $ v2 `VG.unsafeIndex` (i-4)
                e2_6 = unFloat $ v2 `VG.unsafeIndex` (i-5)
                e2_7 = unFloat $ v2 `VG.unsafeIndex` (i-6)
                e2_8 = unFloat $ v2 `VG.unsafeIndex` (i-7)

{-# INLINE distance_Vector_simd16 #-}
distance_Vector_simd16 :: (VG.Vector v Float) => v Float -> v Float -> Float
distance_Vector_simd16 !v1 !v2 = sqrt $ sum16 (go zeros (VG.length v1-1))
    where
        zeros = broadcastFloatX16# (unFloat 0)

        go tot (-1) = tot
        go tot i = go tot' (i-16)
            where 
                tot' = plusFloatX16# tot sqrarr
                sqrarr = timesFloatX16# minarr minarr
                minarr = minusFloatX16# arr1 arr2

                arr1 = packFloatX16# (# e1_1,  e1_2,  e1_3,  e1_4 
                                     ,  e1_5,  e1_6,  e1_7,  e1_8 
                                     ,  e1_9,  e1_10, e1_11, e1_12
                                     ,  e1_13, e1_14, e1_15, e1_16 #)
                arr2 = packFloatX16# (# e2_1,  e2_2,  e2_3,  e2_4
                                     ,  e2_5,  e2_6,  e2_7,  e2_8 
                                     ,  e2_9,  e2_10, e2_11, e2_12
                                     ,  e2_13, e2_14, e2_15, e2_16 #)

                e1_1 = unFloat $ v1 `VG.unsafeIndex` i
                e1_2 = unFloat $ v1 `VG.unsafeIndex` (i-1)
                e1_3 = unFloat $ v1 `VG.unsafeIndex` (i-2)
                e1_4 = unFloat $ v1 `VG.unsafeIndex` (i-3)
                e1_5 = unFloat $ v1 `VG.unsafeIndex` (i-4)
                e1_6 = unFloat $ v1 `VG.unsafeIndex` (i-5)
                e1_7 = unFloat $ v1 `VG.unsafeIndex` (i-6)
                e1_8 = unFloat $ v1 `VG.unsafeIndex` (i-7)
                e1_9 = unFloat $ v1 `VG.unsafeIndex` (i-8)
                e1_10 = unFloat $ v1 `VG.unsafeIndex` (i-9)
                e1_11 = unFloat $ v1 `VG.unsafeIndex` (i-10)
                e1_12 = unFloat $ v1 `VG.unsafeIndex` (i-11)
                e1_13 = unFloat $ v1 `VG.unsafeIndex` (i-12)
                e1_14 = unFloat $ v1 `VG.unsafeIndex` (i-13)
                e1_15 = unFloat $ v1 `VG.unsafeIndex` (i-14)
                e1_16 = unFloat $ v1 `VG.unsafeIndex` (i-15)

                e2_1 = unFloat $ v2 `VG.unsafeIndex` i
                e2_2 = unFloat $ v2 `VG.unsafeIndex` (i-1)
                e2_3 = unFloat $ v2 `VG.unsafeIndex` (i-2)
                e2_4 = unFloat $ v2 `VG.unsafeIndex` (i-3)
                e2_5 = unFloat $ v2 `VG.unsafeIndex` (i-4)
                e2_6 = unFloat $ v2 `VG.unsafeIndex` (i-5)
                e2_7 = unFloat $ v2 `VG.unsafeIndex` (i-6)
                e2_8 = unFloat $ v2 `VG.unsafeIndex` (i-7)
                e2_9 = unFloat $ v2 `VG.unsafeIndex` (i-8)
                e2_10 = unFloat $ v2 `VG.unsafeIndex` (i-9)
                e2_11 = unFloat $ v2 `VG.unsafeIndex` (i-10)
                e2_12 = unFloat $ v2 `VG.unsafeIndex` (i-11)
                e2_13 = unFloat $ v2 `VG.unsafeIndex` (i-12)
                e2_14 = unFloat $ v2 `VG.unsafeIndex` (i-13)
                e2_15 = unFloat $ v2 `VG.unsafeIndex` (i-14)
                e2_16 = unFloat $ v2 `VG.unsafeIndex` (i-15)

---------------------------------------

list2ByteArray xs = runST $ do
    arr <- newAlignedPinnedByteArray (2^(16::Int)) (veclen*4)
    forM (zip [0..] xs) $ \(i,x) -> do
        writeByteArray arr i x
    unsafeFreezeByteArray arr

{-# INLINE distance_ByteArray_diff1 #-}
distance_ByteArray_diff1 :: ByteArray -> ByteArray -> NumType
distance_ByteArray_diff1 !a1 !a2 = sqrt $ go 0 (veclen-1)
    where
        go tot (-1) = tot
        go tot i = go (tot+diff1*diff1
                      ) (i-1)
            where
                diff1 = (a1 `indexByteArray` i)-(a2 `indexByteArray` i)

{-# INLINE distance_ByteArray_diff4 #-}
distance_ByteArray_diff4 :: ByteArray -> ByteArray -> NumType
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

{-# INLINE distance_ByteArray_simd4 #-}
distance_ByteArray_simd4 :: ByteArray -> ByteArray -> Float
distance_ByteArray_simd4 !(ByteArray ba1#) !(ByteArray ba2#) = sqrt $ sum4 (go zeros (veclen-1))
    where
        zeros = broadcastFloatX4# (unFloat 0)

        go tot (-1) = tot
        go tot i = go tot' (i-4)
            where 
                tot' = plusFloatX4# tot sqrarr
                sqrarr = timesFloatX4# minarr minarr
                minarr = minusFloatX4# arr1 arr2

                arr1 = indexFloatArrayAsFloatX4# ba1# (unInt i)
                arr2 = indexFloatArrayAsFloatX4# ba2# (unInt i)

{-# INLINE distance_ByteArray_simd8 #-}
distance_ByteArray_simd8 :: ByteArray -> ByteArray -> Float
distance_ByteArray_simd8 !(ByteArray ba1#) !(ByteArray ba2#) = sqrt $ sum8 (go zeros (veclen-1))
    where
        zeros = broadcastFloatX8# (unFloat 0)

        go tot (-1) = tot
        go tot i = go tot' (i-8)
            where 
                tot' = plusFloatX8# tot sqrarr 
                sqrarr = timesFloatX8# minarr minarr
                minarr = minusFloatX8# arr1 arr2

                arr1 = indexFloatArrayAsFloatX8# ba1# (unInt i)
                arr2 = indexFloatArrayAsFloatX8# ba2# (unInt i)

{-# INLINE distance_ByteArray_simd8' #-}
distance_ByteArray_simd8' :: ByteArray -> ByteArray -> Float
distance_ByteArray_simd8' !(ByteArray ba1#) !(ByteArray ba2#) = sqrt $ sum8' (go zeros (veclen-1))
    where
        zeros = broadcastFloatX8# (unFloat 0)

        go tot (-1) = tot
        go tot i = go tot' (i-8)
            where 
                tot' = plusFloatX8# tot sqrarr 
                sqrarr = timesFloatX8# minarr minarr
                minarr = minusFloatX8# arr1 arr2

                arr1 = indexFloatArrayAsFloatX8# ba1# (unInt i)
                arr2 = indexFloatArrayAsFloatX8# ba2# (unInt i)

{-# INLINE distance_ByteArray_simd16 #-}
distance_ByteArray_simd16 :: ByteArray -> ByteArray -> Float
distance_ByteArray_simd16 !(ByteArray ba1#) !(ByteArray ba2#) = sqrt $ sum16 (go zeros (veclen-1))
    where
        zeros = broadcastFloatX16# (unFloat 0)

        go tot (-1) = tot
        go tot i = go tot' (i-16)
            where 
                tot' = plusFloatX16# tot sqrarr 
                sqrarr = timesFloatX16# minarr minarr
                minarr = minusFloatX16# arr1 arr2

                arr1 = indexFloatArrayAsFloatX16# ba1# (unInt i)
                arr2 = indexFloatArrayAsFloatX16# ba2# (unInt i)

{-# INLINE distance_ByteArray_simd16' #-}
distance_ByteArray_simd16' :: ByteArray -> ByteArray -> Float
distance_ByteArray_simd16' !(ByteArray ba1#) !(ByteArray ba2#) = sqrt $ sum16' (go zeros (veclen-1))
    where
        zeros = broadcastFloatX16# (unFloat 0)

        go tot (-1) = tot
        go tot i = go tot' (i-16)
            where 
                tot' = plusFloatX16# tot sqrarr 
                sqrarr = timesFloatX16# minarr minarr
                minarr = minusFloatX16# arr1 arr2

                arr1 = indexFloatArrayAsFloatX16# ba1# (unInt i)
                arr2 = indexFloatArrayAsFloatX16# ba2# (unInt i)

-------

{-# INLINE unFloat #-}
unFloat :: Float -> Float#
unFloat (F# f) = f

{-# INLINE unInt #-}
unInt :: Int -> Int#
unInt (I# i) = i

{-# INLINE sum4 #-}
sum4 :: FloatX4# -> Float
sum4 arr = F# r1 + F# r2 + F# r3 + F# r4
    where
        (# r1, r2, r3, r4 #) = unpackFloatX4# arr

{-# INLINE sum8 #-}
sum8 :: FloatX8# -> Float
sum8 arr = F# r1 + F# r2 + F# r3 + F# r4
         + F# r5 + F# r6 + F# r7 + F# r8
    where
        (# r1, r2, r3, r4, r5, r6, r7, r8 #) = unpackFloatX8# arr

{-# INLINE sum8' #-}
sum8' :: FloatX8# -> Float
sum8' arr = sum4 a1 + sum4 a2
    where
        a1 = packFloatX4# (# r1, r2, r3, r4 #)
        a2 = packFloatX4# (# r5, r6, r7, r8 #) 
        (# r1, r2, r3, r4, r5, r6, r7, r8 #) = unpackFloatX8# arr

{-# INLINE sum16 #-}
sum16 :: FloatX16# -> Float
sum16 arr = F# r1 + F# r2 + F# r3 + F# r4
          + F# r5 + F# r6 + F# r7 + F# r8
          + F# r9 + F# r10+ F# r11+ F# r12
          + F# r13+ F# r14+ F# r15+ F# r16
    where
        (# r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16 #) = 
                unpackFloatX16# arr

{-# INLINE sum16' #-}
sum16' :: FloatX16# -> Float
sum16' arr = sum8' a1 + sum8' a2
    where
        a1 = packFloatX8# (# r1, r2, r3, r4, r5, r6, r7, r8 #)
        a2 = packFloatX8# (# r9, r10, r11, r12, r13, r14, r15, r16 #)
        (# r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16 #) = 
                unpackFloatX16# arr
