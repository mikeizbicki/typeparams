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
import qualified Data.Params.Vector.UnboxedRaw as VPUR
import qualified Data.Params.Vector.Storable as VPS
import qualified Data.Params.Vector.Storable as VPSR
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V

-------------------------------------------------------------------------------
-- criterion tests

-- | size of each vector to test; must be divisible by 4
type Veclen = 40
veclen = fromIntegral $ natVal (Proxy::Proxy Veclen)

-- | number of vectors in 2d tests
type Numvec = 100
numvec = fromIntegral $ natVal (Proxy::Proxy Numvec)

-- | numeric type to test against
type NumType = Double

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
