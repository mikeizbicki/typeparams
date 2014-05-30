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

-- | This module is intentionally simple so that the core and assembly will 
-- be easy to inspect.  This demonstrates that the generated machine code
-- is slightly nicer when parameters are known at compile time.
module Main
    where

import GHC.Float
import GHC.Prim

import Data.Params

data Test (p::Config Nat) = Test Int
mkParams ''Test

{-# INLINE doSomething #-}
doSomething :: forall p.
    ( ViewParam Param_p (Test p)
    ) => Test p -> Int
doSomething t@(Test i) = i*viewParam _p t

{-# INLINE doStatic #-}
doStatic :: Int -> Int
doStatic i = doSomething (Test i::Test (Static 123456))
-- resulting core:
--
-- doStatic :: Int -> Int
-- doStatic =
--   \ (i :: Int) -> case i of _ [Occ=Dead] { I# x -> I# (*# x 10) }

{-# INLINE doRuntime #-}
doRuntime :: Int -> Int
doRuntime i = mkApWith1Param
    (Proxy::Proxy (Test RunTime))
    (Proxy::Proxy Int)
    _p
    123455
    doSomething
    (Test i)
-- resulting core:
--
-- doRuntime :: Int -> Int
-- doRuntime =
--   doRuntime1
--   `cast` (<Int>_R -> UnivCo representational Any Int
--           :: (Int -> Any) ~# (Int -> Int))
-- 


main = do
    print $ doStatic 1234
    print $ doRuntime 1233

    print (rationalToFloat 2 1)


{-# RULES 
 
"rationalToFloat 2 1"  rationalToFloat 2 1 = 2 :: Float

  #-}

{-# NOINLINE x #-}
x=2::Rational

