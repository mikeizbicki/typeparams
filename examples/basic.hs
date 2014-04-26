{-# LANGUAGE TemplateHaskell #-}

import Data.Monoid
import Data.Params

data ReflectionTest (a::Maybe Nat) (b::Maybe Nat) = ReflectionTest Int Int Int
    deriving (Read,Show,Eq,Ord)
mkParams ''ReflectionTest

instance 
    ( Param_a (ReflectionTest a b)
    , Param_b (ReflectionTest a b)
    ) => Monoid (ReflectionTest a b) 
        where

    mempty = ReflectionTest a b $ a+b
        where
            a = param_a (undefined::ReflectionTest a b)
            b = param_b (undefined::ReflectionTest a b)

    mappend a b = a

