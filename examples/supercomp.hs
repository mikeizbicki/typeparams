{-# OPTIONS_GHC -O2 -fllvm -mavx512f #-}
{-# LANGUAGE DataKinds #-}
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

-- | This file contains a single data type called "ManyParams" that has many
-- type level parameters.  Criterion benchmarks show that specifying these
-- parameters at compile time results in approximately a 25% performance improvement
-- vs specifying at run time.
module Main
    where

import Control.Category
import Control.DeepSeq
import Control.Monad
import Control.Monad.Random
import Criterion.Config
import Criterion.Main
import Data.Monoid
import qualified Data.Vector.Generic as VG
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
-- test functions 

-- {-# NOINLINE sumVector #-}
sumVector :: (Monoid e, VG.Vector v e) => v e -> e
sumVector v = VG.foldl1' mappend v

newtype ManyParams
    (p1 :: Param Nat)
    (p2 :: Param Nat)
    (p3 :: Param Nat)
    (p4 :: Param Nat)
    (p5 :: Param Nat)
    = ManyParams Int
    deriving (Read,Show,Eq,Ord)
mkParams ''ManyParams

instance NFData (ManyParams p1 p2 p3 p4 p5) where
    rnf (ManyParams x) = rnf x

instance PseudoPrim (ManyParams p1 p2 p3 p4 p5) where
    newtype PseudoPrimInfo (ManyParams p1 p2 p3 p4 p5)
        = PseudoPrimInfo_ManyParams (PseudoPrimInfo Int)
    pp_sizeOf# (PseudoPrimInfo_ManyParams ppi) = pp_sizeOf# ppi
    pp_alignment# (PseudoPrimInfo_ManyParams ppi) = pp_alignment# ppi
    pp_indexByteArray# (PseudoPrimInfo_ManyParams ppi) arr# i# 
        = ManyParams $ pp_indexByteArray# ppi arr# i#
    pp_readByteArray# (PseudoPrimInfo_ManyParams ppi) marr# i# s# 
        = case pp_readByteArray# ppi marr# i# s# of
            (# s, d #) -> (# s, ManyParams d #)
    pp_writeByteArray# (PseudoPrimInfo_ManyParams ppi) marr# i# (ManyParams d) s#
        = pp_writeByteArray# ppi marr# i# d s#
    seqInfo (ManyParams d) = seqInfo d
    emptyInfo = PseudoPrimInfo_ManyParams $ emptyInfo
    {-# INLINE pp_sizeOf# #-}
    {-# INLINE pp_alignment# #-}
    {-# INLINE pp_indexByteArray# #-}
    {-# INLINE pp_readByteArray# #-}
    {-# INLINE pp_writeByteArray# #-}
    {-# INLINE seqInfo #-}
    {-# INLINE emptyInfo #-}

instance
    ( ViewParam Param_p1 t
    , ViewParam Param_p2 t
    , ViewParam Param_p3 t
    , ViewParam Param_p4 t
    , ViewParam Param_p5 t
    , t ~ ManyParams p1 p2 p3 p4 p5
    ) => Monoid (ManyParams p1 p2 p3 p4 p5)
        where

    {-# INLINE mempty #-}
    mempty = ManyParams 0

    {-# INLINE mappend #-}
    mappend m@(ManyParams m1) (ManyParams m2) = ManyParams 
        $ m1+m2
        + viewParam _p1 m
        + viewParam _p2 m
        + viewParam _p3 m
        + viewParam _p4 m
        + viewParam _p5 m

-------------------------------------------------------------------------------
-- criterion tests

-- | criterion configuration parameters
critConfig = defaultConfig 
    { cfgPerformGC   = ljust True
    , cfgSamples     = ljust 10
--     , cfgSummaryFile = ljust $ "results/summary-"++show veclen++"-"++show numvec++".csv"
--     , cfgReport      = ljust "report.html"
    }

veclen = 10000000

type P1 = Static 1211
type P2 = Static 1222
type P3 = Static 1233
type P4 = Static 1244
type P5 = Static 1215

-- type P1 = Static (1211/1)
-- type P2 = Static (1222/1)
-- type P3 = Static (1233/1)
-- type P4 = Static (1244/1)
-- type P5 = Static (1215/1)

type MP5 = ManyParams P1 P2 P3 P4 P5
type MP4 = ManyParams P1 P2 P3 P4 RunTime
type MP3 = ManyParams P1 P2 P3 RunTime RunTime
type MP2 = ManyParams P1 P2 RunTime RunTime RunTime
type MP1 = ManyParams P1 RunTime RunTime RunTime RunTime
type MP0 = ManyParams RunTime RunTime RunTime RunTime RunTime

-------------------------------------------------------------------------------
-- main function

nfWith1Constraint :: NFData b => ((p => a) -> b) -> (p => a) -> Pure
nfWith1Constraint = nf

nfWith2Constraint :: NFData b => (((p1,p2) => a) -> b) -> ((p1,p2) => a) -> Pure
nfWith2Constraint = nf

nfWith3Constraint :: NFData b => (((p1,p2,p3) => a) -> b) -> ((p1,p2,p3) => a) -> Pure
nfWith3Constraint = nf

-- {-# RULES "intparam111" intparam (Proxy::Proxy 111) = (111::Int) #-}
-- {-# RULES "intparam222" intparam (Proxy::Proxy 222) = (222::Int) #-}

-- return $ 
--     [ PragmaD $ RuleP 
--         ("intparam "++show i)
--         [ ]
--         ( AppE 
--             ( VarE $ mkName "intparam" )
--             ( SigE
--                 ( ConE $ mkName "Proxy" )
--                 ( AppT
--                     ( ConT $ mkName "Proxy" )
--                     ( LitT ( NumTyLit i ) )
--                 )
--             )
--         )
--         ( AppE ( ConE $ mkName "I#" ) (LitE $ IntPrimL i ) )
--         AllPhases
--     | i <- [0..10000]
--     ]

main = do
    
    -----------------------------------
    -- initializing tests

    putStrLn "initializing tests"

--     let randL = evalRand (replicateM veclen $ getRandomR (-10000,10000)) (mkStdGen $ 1)
    let randL = replicate veclen 1

    let vs5 = VG.fromList (map ManyParams randL) :: VPU.Vector Automatic MP5
        vs4 = VG.fromList (map ManyParams randL) :: VPU.Vector Automatic MP4
        vs3 = VG.fromList (map ManyParams randL) :: VPU.Vector Automatic MP3
        vs2 = VG.fromList (map ManyParams randL) :: VPU.Vector Automatic MP2
        vs1 = VG.fromList (map ManyParams randL) :: VPU.Vector Automatic MP1
        vs0 = VG.fromList (map ManyParams randL) :: VPU.Vector Automatic MP0

    deepseq vs5 $ return ()
    deepseq vs4 $ return ()
    deepseq vs3 $ return ()
    deepseq vs2 $ return ()
    deepseq vs1 $ return ()
    deepseq vs0 $ return ()

    let test4 = mkApWith1Param 
                (Proxy::Proxy (VPU.Vector Automatic MP4))
                (Proxy::Proxy MP4)
                (_elem._p5) 2222
                sumVector 

    let test3 = mkApWith2Param 
                (Proxy::Proxy (VPU.Vector Automatic MP3))
                (Proxy::Proxy MP3)
                (_elem._p5) 2222
                (_elem._p4) 2223 
                sumVector 

    let test2 = mkApWith3Param 
                (Proxy::Proxy (VPU.Vector Automatic MP2))
                (Proxy::Proxy MP2)
                (_elem._p5) 2222 
                (_elem._p4) 2223 
                (_elem._p3) 2224 
                sumVector 

    -----------------------------------
    -- run tests

    putStrLn "starting criterion"

    defaultMainWith critConfig (return ())
        [ bench "vs5" $ nf sumVector vs5
        , bench "vs4" $ nfWith1Constraint test4 vs4
        , bench "vs3" $ nfWith2Constraint test3 vs3
        , bench "vs2" $ nfWith3Constraint test2 vs2
        ]



