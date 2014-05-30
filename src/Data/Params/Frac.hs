{-# LANGUAGE DataKinds #-}

-- | Provides type level fractions based on type nats
module Data.Params.Frac
    ( Frac (..)
    , KnownFrac (..)
    , fracVal
    )
    where

import GHC.TypeLits
import Data.Proxy

-- | (Kind) This is the kind of type-level fractions.  
-- It is not built in to GHC, but instead defined in terms of 'Nat'
data Frac = (/) Nat Nat

-- | This class gives the 'Rational' associated with a type-level fraction.
class KnownFrac (n :: Frac) where
    fracSing :: SFrac n 

instance (KnownNat a, KnownNat b) => KnownFrac (a/b) where
    fracSing = SFrac 
        (fromIntegral $ natVal (Proxy::Proxy a)) 
        (fromIntegral $ natVal (Proxy::Proxy b))

-- | get the value from a type frac
-- fracVal :: forall n proxy. (KnownFrac n) => proxy n -> Rational
fracVal :: forall f n proxy. (KnownFrac n, Fractional f) => proxy n -> f 
fracVal _ = case fracSing :: SFrac n of
    SFrac a b -> fromIntegral a / fromIntegral b 

data SFrac (n :: Frac) = SFrac 
    {-#UNPACK#-}!Int
    {-#UNPACK#-}!Int
-- data SFrac (n :: Frac) = SFrac Integer Integer 
