module Data.Params.ModInt
    where

import Data.Params

-------------------------------------------------------------------------------
-- general

data family ModIntegral (modulus :: Param Nat) i

type ModInt     modulus = ModIntegral modulus Int
type ModInteger modulus = ModIntegral modulus Integer

-------------------------------------------------------------------------------
-- Static 

newtype instance ModIntegral (Static n) i = ModIntegral_Static i
    deriving (Read,Show,Eq,Ord)

instance (KnownNat n, Integral i) => Num (ModIntegral (Static n) i) where
    (ModIntegral_Static a)+(ModIntegral_Static b) = ModIntegral_Static $ (a+b) `mod` n
        where n = fromIntegral $ natVal (Proxy::Proxy n)
    
    (ModIntegral_Static a)*(ModIntegral_Static b) = ModIntegral_Static $ (a*b) `mod` n
        where n = fromIntegral $ natVal (Proxy::Proxy n)
    
    abs = id
    
    signum = id

    fromInteger a = ModIntegral_Static $ (fromIntegral $ a `mod` n)
        where n = fromIntegral $ natVal (Proxy::Proxy n)

    negate (ModIntegral_Static a) = ModIntegral_Static $ (a-n) `mod` n
        where n = fromIntegral $ natVal (Proxy::Proxy n)

-------------------------------------------------------------------------------
-- RunTime

newtype instance ModIntegral RunTime i = ModIntegral_RunTime i
    deriving (Read,Show,Eq,Ord)
mkParams ''ModIntegral

instance 
    ( Param_modulus (ModIntegral RunTime i)
    , Integral i
    ) => Num (ModIntegral RunTime i) 
        where
    (ModIntegral_RunTime a)+(ModIntegral_RunTime b) = ModIntegral_RunTime $ (a+b) `mod` n
        where n = fromIntegral $ param_modulus (undefined:: (ModIntegral RunTime i))
    
    (ModIntegral_RunTime a)*(ModIntegral_RunTime b) = ModIntegral_RunTime $ (a*b) `mod` n
        where n = fromIntegral $ param_modulus (undefined:: (ModIntegral RunTime i))
    
    abs = id
    
    signum = id

    fromInteger a = ModIntegral_RunTime $ (fromIntegral $ a `mod` n)
        where n = fromIntegral $ param_modulus (undefined:: (ModIntegral RunTime i))

    negate (ModIntegral_RunTime a) = ModIntegral_RunTime $ a-n `mod` n
        where n = fromIntegral $ param_modulus (undefined:: (ModIntegral RunTime i))
