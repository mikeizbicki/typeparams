{-# OPTIONS_GHC -O2 -fllvm -mavx512f -optlo -O3 -optlo -enable-unsafe-fp-math #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This example performs a number of criterion benchmarks calculating various
-- Lp (Lebesgue) metrics.  The variable p is specified at the type level.
module Main
    where

import Control.Category
import Control.DeepSeq
import Control.Monad
import Control.Monad.Random
import qualified Criterion.Config as Criterion
import Criterion.Main
import Data.Monoid
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Params.Vector.Unboxed as VPU
import qualified Data.Params.Vector.UnboxedRaw as VPUR
import qualified Data.Params.Vector.Storable as VPS
import qualified Data.Params.Vector.Storable as VPSR
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import Prelude hiding ((.),id)

import GHC.Base (Int(..))
import Language.Haskell.TH hiding (reify)
import Language.Haskell.TH.Syntax hiding (reify)
import qualified Language.Haskell.TH as TH

import Data.Params
import Data.Params.Frac
import Data.Params.PseudoPrim
import Data.Params.Vector

-------------------------------------------------------------------------------

newtype Lebesgue (n::Config Frac) (vec :: * -> *) elem = Lebesgue (vec elem)
    deriving (Read,Show,Eq,Ord)

mkParamClass_Config "n" (ConT $ mkName "Rational" )
mkReifiableConstraint "n" 
mkTypeLens_Config "n"
mkViewParam_Config "n" ''Lebesgue
mkApplyConstraint_Config "n" ''Lebesgue
mkHasDictionary_Config "n" (ConT $ mkName "Rational" )
mkParamInstance "n" (ConT $ mkName "Rational" ) ''Lebesgue

mkTypeLens_Star "elem"
mkViewParam_Star "elem" ''Lebesgue
mkApplyConstraint_Star "elem" ''Lebesgue
mkHasDictionary_Star "elem"
mkParamClass_Star "elem"

mkParamClass_Star "vec"
mkTypeLens_Star "vec"
mkHasDictionary_Star "vec"

instance 
    ( ViewParam p (vec elem) 
    ) => ViewParam (Param_vec p) (Lebesgue n vec elem)
        where
    viewParam _ _ = viewParam (undefined::TypeLens Base p) (undefined :: vec elem)

type instance ApplyConstraint_GetConstraint (Param_vec p) 
   = ApplyConstraint_GetConstraint p 

type instance ApplyConstraint_GetType (Param_vec p) (Lebesgue n vec elem)
   = ApplyConstraint_GetType p (Lebesgue n vec elem)

v = VG.fromList [1..10] :: Lebesgue (Static (2/1)) (VPU.Vector Automatic) Float
v' = VG.fromList [1..10] :: Lebesgue (Static (2/1)) (VPU.Vector (Static 10)) Float

-------------------

instance PseudoPrim (vec elem) => PseudoPrim (Lebesgue n vec elem) where
    newtype PseudoPrimInfo (Lebesgue n vec elem)
        = PseudoPrimInfo_ManyParams (PseudoPrimInfo (vec elem))
    pp_sizeOf# (PseudoPrimInfo_ManyParams ppi) = pp_sizeOf# ppi
    pp_alignment# (PseudoPrimInfo_ManyParams ppi) = pp_alignment# ppi
    pp_indexByteArray# (PseudoPrimInfo_ManyParams ppi) arr# i# 
        = Lebesgue $ pp_indexByteArray# ppi arr# i#
    pp_readByteArray# (PseudoPrimInfo_ManyParams ppi) marr# i# s# 
        = case pp_readByteArray# ppi marr# i# s# of
            (# s, d #) -> (# s, Lebesgue d #)
    pp_writeByteArray# (PseudoPrimInfo_ManyParams ppi) marr# i# (Lebesgue d) s#
        = pp_writeByteArray# ppi marr# i# d s#
    seqInfo (Lebesgue d) = seqInfo d
    emptyInfo = PseudoPrimInfo_ManyParams $ emptyInfo
    {-# INLINE pp_sizeOf# #-}
    {-# INLINE pp_alignment# #-}
    {-# INLINE pp_indexByteArray# #-}
    {-# INLINE pp_readByteArray# #-}
    {-# INLINE pp_writeByteArray# #-}
    {-# INLINE seqInfo #-}
    {-# INLINE emptyInfo #-}

---------------------------------------
-- Vector instances

instance VG.Vector vec elem => VG.Vector (Lebesgue p vec) elem where
    {-# INLINE basicUnsafeFreeze #-}
    {-# INLINE basicUnsafeThaw #-}
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicUnsafeIndexM #-}
    {-# INLINE basicUnsafeCopy #-}
    {-# INLINE elemseq #-}
    basicUnsafeFreeze (MLebesgue v) = liftM Lebesgue $ VG.basicUnsafeFreeze v
    basicUnsafeThaw (Lebesgue v) = liftM MLebesgue $ VG.basicUnsafeThaw v
    basicLength (Lebesgue v) = VG.basicLength v
    basicUnsafeSlice s t (Lebesgue v) = Lebesgue $ VG.basicUnsafeSlice s t v
    basicUnsafeIndexM (Lebesgue v) i = VG.basicUnsafeIndexM v i
    basicUnsafeCopy (MLebesgue vm) (Lebesgue v) = VG.basicUnsafeCopy vm v
    elemseq (Lebesgue v) a b = VG.elemseq v a b

newtype MLebesgue (n::Config Frac) v s a = MLebesgue ( v s a )

instance VGM.MVector v a => VGM.MVector (MLebesgue n v) a where
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicOverlaps #-}
    {-# INLINE basicUnsafeNew #-}
    {-# INLINE basicUnsafeReplicate #-}
    {-# INLINE basicUnsafeRead #-}
    {-# INLINE basicUnsafeWrite #-}
    basicLength (MLebesgue v) = VGM.basicLength v
    basicUnsafeSlice s t (MLebesgue v) = MLebesgue $ VGM.basicUnsafeSlice s t v
    basicOverlaps (MLebesgue v1) (MLebesgue v2) = VGM.basicOverlaps v1 v2
    basicUnsafeNew n = liftM MLebesgue $ VGM.basicUnsafeNew n
    basicUnsafeReplicate i a = liftM MLebesgue $ VGM.basicUnsafeReplicate i a
    basicUnsafeRead (MLebesgue v) i = VGM.basicUnsafeRead v i
    basicUnsafeWrite (MLebesgue v) i a = VGM.basicUnsafeWrite v i a

type instance VG.Mutable (Lebesgue n v) = MLebesgue n (VG.Mutable v)

---------------------------------------
-- the Lebesgue distance

lp_distance :: forall n vec elem.
    ( VG.Vector vec elem
    , Floating elem
    , ViewParam Param_n (Lebesgue n vec elem)
    ) => Lebesgue n vec elem -> Lebesgue n vec elem -> elem
lp_distance !v1 !v2 = (go 0 (VG.length v1-1))**(1/n)
    where
        n = fromRational $ viewParam _n v1

        go tot (-1) = tot
        go tot i = go (tot+diff1**n) (i-1)
            where 
                diff1 = abs $ v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i

-------------------------------------------------------------------------------
-- criterion tests

-- | size of each vector to test; must be divisible by 4
type Veclen = 16
veclen = intparam (Proxy::Proxy Veclen)

-- | number of vectors in 2d tests
type Numvec = 160
numvec = intparam (Proxy::Proxy Numvec)

-- | numeric type to test against
type NumType = Float

-- | criterion configuration parameters
critConfig = Criterion.defaultConfig 
    { Criterion.cfgPerformGC   = Criterion.ljust True
    , Criterion.cfgSamples     = Criterion.ljust 100
--     , cfgSummaryFile = ljust $ "results/summary-"++show veclen++"-"++show numvec++".csv"
--     , cfgReport      = ljust "report.html"
    }

-------------------------------------------------------------------------------
-- main

main = do
    
    -----------------------------------
    -- initialize single vectors

    putStrLn "constructing single vectors"

    let dimL1 :: [NumType] = evalRand (replicateM veclen $ getRandomR (-10000,10000)) (mkStdGen $ 1)
        dimL2 :: [NumType] = evalRand (replicateM veclen $ getRandomR (-10000,10000)) (mkStdGen $ 2)

    let vu1 = VU.fromList dimL1
        vu2 = VU.fromList dimL2

    -----------------------------------
    -- initialize 2d vectors

    putStrLn "constructing 2d vectors of vectors"

    let dimLL1 :: [[NumType]] = 
            [ evalRand (replicateM veclen $ getRandomR (-10000,10000)) (mkStdGen i) | i <- [1..numvec]]
        dimLL2 :: [[NumType]] = 
            [ evalRand (replicateM veclen $ getRandomR (-10000,10000)) (mkStdGen i) | i <- [2..numvec+1]]

    let vvl1a = VG.fromList $ map VG.fromList dimLL1 
            :: VPU.Vector (Static Numvec) (Lebesgue (Static (1/1)) (VPU.Vector (Static Veclen)) NumType)
        vvl1b = VG.fromList $ map VG.fromList dimLL2 
            :: VPU.Vector (Static Numvec) (Lebesgue (Static (1/1)) (VPU.Vector (Static Veclen)) NumType)

    let vvl2b = VG.fromList $ map VG.fromList dimLL1 
            :: VPU.Vector (Static Numvec) (Lebesgue (Static (2/1)) (VPU.Vector (Static Veclen)) NumType)
        vvl2a = VG.fromList $ map VG.fromList dimLL2 
            :: VPU.Vector (Static Numvec) (Lebesgue (Static (2/1)) (VPU.Vector (Static Veclen)) NumType)

    let vvl3b = VG.fromList $ map VG.fromList dimLL1 
            :: VPU.Vector (Static Numvec) (Lebesgue (Static (2/1)) (VPU.Vector (Static Veclen)) NumType)
        vvl3a = VG.fromList $ map VG.fromList dimLL2 
            :: VPU.Vector (Static Numvec) (Lebesgue (Static (2/1)) (VPU.Vector (Static Veclen)) NumType)

    -----------------------------------
    -- tests

    putStrLn "starting criterion"

    defaultMainWith critConfig (return ())
        [ bgroup "allToAll"
            [ bench "Static (1/1)" $ nf (distance_allToAll lp_distance vvl1a) vvl1b
            , bench "Static (2/1)" $ nf (distance_allToAll lp_distance vvl2a) vvl2b
            , bench "Static (3/1)" $ nf (distance_allToAll lp_distance vvl3a) vvl3b
            , bench "standard" $ nf (distance_allToAll distance_Vector_diff1 vvl3a) vvl3b
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
