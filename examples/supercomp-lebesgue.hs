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

import GHC.Prim
import GHC.Float
import GHC.Base (Int(..))
import Language.Haskell.TH hiding (reify)
import Language.Haskell.TH.Syntax hiding (reify)
import qualified Language.Haskell.TH as TH

import Data.Params
import Data.Params.Frac
import Data.Params.PseudoPrim
import Data.Params.Vector
import Numeric.FastMath

-------------------------------------------------------------------------------

newtype Lebesgue (p::Config Frac) (vec :: * -> *) elem = Lebesgue (vec elem)
    deriving (Read,Show,Eq,Ord)

mkParamClass_Config "p" (ConT $ mkName "Float" )
mkHasDictionary_Config "p" (ConT $ mkName "Float" )
mkParamInstance "p" (ConT $ mkName "Float" ) ''Lebesgue
mkReifiableConstraint "p" 
mkTypeLens_Config "p"
mkViewParam_Config "p" ''Lebesgue
mkApplyConstraint_Config "p" ''Lebesgue

mkTypeLens_Star "elem"
mkViewParam_Star "elem" ''Lebesgue
mkApplyConstraint_Star "elem" ''Lebesgue
mkHasDictionary_Star "elem"
mkParamClass_Star "elem"

mkParamClass_Star "vec"
mkTypeLens_Star "vec"
mkHasDictionary_Star "vec"

instance 
    ( ViewParam p1 (vec elem) 
    ) => ViewParam (Param_vec p1) (Lebesgue p vec elem)
        where
    viewParam _ _ = viewParam (undefined::TypeLens Base p1) (undefined :: vec elem)

type instance ApplyConstraint_GetConstraint (Param_vec p) 
   = ApplyConstraint_GetConstraint p 

type instance ApplyConstraint_GetType (Param_vec p1) (Lebesgue p vec elem)
   = ApplyConstraint_GetType p1 (Lebesgue p vec elem)

v = VG.fromList [1..10] :: Lebesgue (Static (2/1)) (VPU.Vector Automatic) Float
v' = VG.fromList [1..10] :: Lebesgue (Static (2/1)) (VPU.Vector (Static 10)) Float

-------------------

instance NFData (vec elem) => NFData (Lebesgue p vec elem) where
    rnf (Lebesgue v) = rnf v

instance PseudoPrim (vec elem) => PseudoPrim (Lebesgue p vec elem) where
    newtype PseudoPrimInfo (Lebesgue p vec elem)
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

newtype MLebesgue (p::Config Frac) v s a = MLebesgue ( v s a )

instance VGM.MVector v a => VGM.MVector (MLebesgue p v) a where
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
    basicUnsafeNew p = liftM MLebesgue $ VGM.basicUnsafeNew p
    basicUnsafeReplicate i a = liftM MLebesgue $ VGM.basicUnsafeReplicate i a
    basicUnsafeRead (MLebesgue v) i = VGM.basicUnsafeRead v i
    basicUnsafeWrite (MLebesgue v) i a = VGM.basicUnsafeWrite v i a

type instance VG.Mutable (Lebesgue p v) = MLebesgue p (VG.Mutable v)

---------------------------------------
-- the Lebesgue distance

lp_distance :: 
    ( VG.Vector vec elem
    , Floating elem
    , elem ~ Float
    , ViewParam Param_p (Lebesgue p vec elem)
    ) => Lebesgue p vec elem -> Lebesgue p vec elem -> elem
lp_distance !v1 !v2 = (go 0 (VG.length v1-1))**(1/p)
    where
        p = viewParam _p v1

        go tot (-1) = tot
        go tot i = go (tot+diff1**p) (i-1)
            where 
                diff1 = abs $ v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i

l1_distance :: (VG.Vector v f, Floating f) => v f -> v f -> f
l1_distance !v1 !v2 = go 0 (VG.length v1-1)
    where
        go tot (-1) = tot
        go tot i = go (tot+diff1) (i-1)
            where 
                diff1 = abs $ v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i

l1_distance' :: (VG.Vector v f, Floating f) => v f -> v f -> f
l1_distance' !v1 !v2 = go 0 (VG.length v1-1)
    where
        go tot (-1) = tot
        go tot i = go (tot+(sqrt $ diff1*diff1)) (i-1)
            where 
                diff1 = v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i

l2_distance :: (VG.Vector v f, Floating f) => v f -> v f -> f
l2_distance !v1 !v2 = sqrt $ go 0 (VG.length v1-1)
    where
        go tot (-1) = tot
        go tot i = go (tot+diff1*diff1) (i-1)
            where 
                diff1 = v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i

l3_distance :: (VG.Vector v f, Floating f) => v f -> v f -> f
l3_distance !v1 !v2 = (go 0 (VG.length v1-1))**(1/3)
    where
        go tot (-1) = tot
        go tot i = go (tot+diff1*diff1*diff1) (i-1)
            where 
                diff1 = abs $ v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i

l4_distance :: (VG.Vector v f, Floating f) => v f -> v f -> f
l4_distance !v1 !v2 = sqrt $ sqrt $ go 0 (VG.length v1-1)
    where
        go tot (-1) = tot
        go tot i = go (tot+diff1*diff1*diff1*diff1) (i-1)
            where 
                diff1 = abs $ v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i
l77_distance :: (VG.Vector v f, Floating f) => v f -> v f -> f
l77_distance !v1 !v2 = (go 0 (VG.length v1-1))**(2/77)
    where
        go tot (-1) = tot
        go tot i = go (tot+diff1**(77/2)) (i-1)
            where 
                diff1 = abs $ v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i

l79_distance :: (VG.Vector v f, Floating f) => v f -> v f -> f
l79_distance !v1 !v2 = (go 0 (VG.length v1-1))**(2/79)
    where
        go tot (-1) = tot
        go tot i = go (tot+diff1**(1651110466/1024)) (i-1)
            where 
                diff1 = abs $ v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i

-------------------------------------------------------------------------------
-- criterion tests

-- | size of each vector to test; must be divisible by 4
type Veclen = 16 
veclen = intparam (Proxy::Proxy Veclen)

-- | number of vectors in 2d tests
type Numvec = 1000
numvec = intparam (Proxy::Proxy Numvec)

-- | criterion configuration parameters
critConfig = Criterion.defaultConfig 
    { Criterion.cfgPerformGC   = Criterion.ljust True
    , Criterion.cfgSamples     = Criterion.ljust 30
--     , cfgSummaryFile = ljust $ "results/summary-"++show veclen++"-"++show numvec++".csv"
--     , cfgReport      = ljust "report.html"
    }

mkRuleFrac 1
mkRuleFrac 2
mkRuleFrac 3
mkRuleFrac 4
mkRuleFrac (77/2)

-------------------------------------------------------------------------------
-- main

main = do
    
    -----------------------------------
    -- initialize single vectors

    putStrLn "constructing single vectors"

    let dimL1 :: [Float] = evalRand (replicateM veclen $ getRandomR (-10000,10000)) (mkStdGen $ 1)
        dimL2 :: [Float] = evalRand (replicateM veclen $ getRandomR (-10000,10000)) (mkStdGen $ 2)

    -----------------------------------
    -- initialize 2d vectors

    putStrLn "constructing 2d vectors of vectors"

    let dimLL1 :: [[Float]] = 
            [ evalRand (replicateM veclen $ getRandomR (-10000,10000)) (mkStdGen i) | i <- [1..numvec]]
        dimLL2 :: [[Float]] = 
            [ evalRand (replicateM veclen $ getRandomR (-10000,10000)) (mkStdGen i) | i <- [2..numvec+1]]

    let vvl1a = VG.fromList $ map VG.fromList dimLL1 
            :: VPU.Vector (Static Numvec) (Lebesgue (Static (1/1)) (VPU.Vector (Static Veclen)) Float)
        vvl1b = VG.fromList $ map VG.fromList dimLL2 
            :: VPU.Vector (Static Numvec) (Lebesgue (Static (1/1)) (VPU.Vector (Static Veclen)) Float)

    let vvl2b = VG.fromList $ map VG.fromList dimLL1 
            :: VPU.Vector (Static Numvec) (Lebesgue (Static (2/1)) (VPU.Vector (Static Veclen)) Float)
        vvl2a = VG.fromList $ map VG.fromList dimLL2 
            :: VPU.Vector (Static Numvec) (Lebesgue (Static (2/1)) (VPU.Vector (Static Veclen)) Float)

    let vvl3b = VG.fromList $ map VG.fromList dimLL1 
            :: VPU.Vector (Static Numvec) (Lebesgue (Static (3/1)) (VPU.Vector (Static Veclen)) Float)
        vvl3a = VG.fromList $ map VG.fromList dimLL2 
            :: VPU.Vector (Static Numvec) (Lebesgue (Static (3/1)) (VPU.Vector (Static Veclen)) Float)

    let vvl4b = VG.fromList $ map VG.fromList dimLL1 
            :: VPU.Vector (Static Numvec) (Lebesgue (Static (4/1)) (VPU.Vector (Static Veclen)) Float)
        vvl4a = VG.fromList $ map VG.fromList dimLL2 
            :: VPU.Vector (Static Numvec) (Lebesgue (Static (4/1)) (VPU.Vector (Static Veclen)) Float)

    let vvl77b = VG.fromList $ map VG.fromList dimLL1 
            :: VPU.Vector (Static Numvec) (Lebesgue (Static (77/2)) (VPU.Vector (Static Veclen)) Float)
        vvl77a = VG.fromList $ map VG.fromList dimLL2 
            :: VPU.Vector (Static Numvec) (Lebesgue (Static (77/2)) (VPU.Vector (Static Veclen)) Float)

    let vvl79b = VG.fromList $ map VG.fromList dimLL1 
            :: VPU.Vector (Static Numvec) (Lebesgue (Static (1651110466/1024)) (VPU.Vector (Static Veclen)) Float)
        vvl79a = VG.fromList $ map VG.fromList dimLL2 
            :: VPU.Vector (Static Numvec) (Lebesgue (Static (1651110466/1024)) (VPU.Vector (Static Veclen)) Float)

    let vvlna = VG.fromList $ map VG.fromList dimLL1 
            :: VPU.Vector (Static Numvec) (Lebesgue RunTime (VPU.Vector (Static Veclen)) Float)
        vvlnb = VG.fromList $ map VG.fromList dimLL2 
            :: VPU.Vector (Static Numvec) (Lebesgue RunTime (VPU.Vector (Static Veclen)) Float)

    deepseq vvl1a $ deepseq vvl1b $ return ()
    deepseq vvl2a $ deepseq vvl2b $ return ()
    deepseq vvl3a $ deepseq vvl3b $ return ()
    deepseq vvl4a $ deepseq vvl4b $ return ()
    deepseq vvl77a $ deepseq vvl77b $ return ()
    deepseq vvl79a $ deepseq vvl79b $ return ()
    deepseq vvlna $ deepseq vvlnb $ return ()

    -----------------------------------
    -- tests

    putStrLn "starting criterion"

    let test i = mkApWith1Param
            (Proxy::Proxy (VPU.Vector 
                (Static Numvec) 
                (Lebesgue RunTime (VPU.Vector (Static Veclen)) Float)))
            (Proxy::Proxy Float)
            (_elem._p)
            i
            (distance_allToAll lp_distance vvlna)

    defaultMainWith critConfig (return ())
        [ bgroup "Static"
            [ bench "(1/1)" $ nf (distance_allToAll lp_distance vvl1a) vvl1b
            , bench "(2/1)" $ nf (distance_allToAll lp_distance vvl2a) vvl2b
            , bench "(3/1)" $ nf (distance_allToAll lp_distance vvl3a) vvl3b
            , bench "(4/1)" $ nf (distance_allToAll lp_distance vvl4a) vvl4b
            , bench "(77/2)" $ nf (distance_allToAll lp_distance vvl77a) vvl77b
            , bench "(1651110466/1024)" $ nf (distance_allToAll lp_distance vvl79a) vvl79b
            ]
        , bgroup "RunTime"
            [ bench "(1/1)" $ nfWith1Constraint (test 1) vvlnb
            , bench "(2/1)" $ nfWith1Constraint (test 2) vvlnb
            , bench "(3/1)" $ nfWith1Constraint (test 3) vvlnb
            , bench "(4/1)" $ nfWith1Constraint (test 4) vvlnb
            , bench "(77/2)" $ nfWith1Constraint (test $ 77/2) vvlnb
            , bench "(1651110466/1024)" $ nfWith1Constraint (test $ 1651110466/1024) vvlnb
            ]
        , bgroup "HandOpt"
            [ bench "(1/1)" $ nf (distance_allToAll l1_distance vvlna) vvlnb
            , bench "(1/1)'" $ nf (distance_allToAll l1_distance' vvlna) vvlnb
            , bench "(2/1)" $ nf (distance_allToAll l2_distance vvlna) vvlnb
            , bench "(3/1)" $ nf (distance_allToAll l3_distance vvlna) vvlnb
            , bench "(4/1)" $ nf (distance_allToAll l4_distance vvlna) vvlnb
            , bench "(77/2)" $ nf (distance_allToAll l77_distance vvlna) vvlnb
            , bench "(1651110466/1024)" $ nf (distance_allToAll l79_distance vvlna) vvlnb
            ]
        ]

nfWith1Constraint :: NFData b => ((p => a) -> b) -> (p => a) -> Pure
nfWith1Constraint = nf

-------------------------------------------------------------------------------
-- test functions 

-- | sums the distance between a point and every point in a vector in time O(p)
distance_oneToAll ::
    ( VG.Vector v1 (v2 f)
    , VG.Vector v2 f
    , Floating f
    ) => (v2 f -> v2 f -> f) -> v2 f -> v1 (v2 f) -> f
distance_oneToAll !dist !v !vv =  go 0 (VG.length vv-1)
    where
        go !tot (-1) = tot
        go !tot !i = go tot' (i-1)
            where
                tot' = tot + dist v (vv `VG.unsafeIndex` i)

-- | sums the distance between every point in vv1 and every point in vv2 in time O(p^2)
distance_allToAll ::
    ( VG.Vector v1 (v2 f)
    , VG.Vector v2 f
    , Floating f
    ) => (v2 f -> v2 f -> f) -> v1 (v2 f) -> v1 (v2 f) -> f
distance_allToAll !dist !vv1 !vv2 = go 0 (VG.length vv1-1)
    where
        go !tot (-1) = tot
        go !tot !i = go tot' (i-1)
            where
                tot' = tot + distance_oneToAll dist (vv1 `VG.unsafeIndex` i) vv2
