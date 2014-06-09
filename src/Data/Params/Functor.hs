{-# LANGUAGE TemplateHaskell #-}

module Data.Params.Functor
    where

import Control.Category
import Prelude hiding ((.), id, Functor(..), Applicative(..))

import Data.Maybe
import Data.Params

-------------------------------------------------------------------------------

class Functor lens tb where
  fmap' :: ( b ~ GetParam lens tb, ta ~ SetParam lens a tb )  
      => TypeLens p lens 
      -> (a -> b) -> ta -> tb

fmap :: 
  ( Functor lens tb
  , b ~ GetParam lens tb
  , ta ~ SetParam lens a tb
  ) => TypeLens Base lens
    -> (a -> b) -> ta -> tb
fmap lens = fmap' (lens._base)

-------------------
-- Either

mkParams ''Either

instance Functor p b => Functor (Param_b p) (Either a b) where
  fmap' lens f (Left a) = Left a
  fmap' lens f (Right b) = Right $ fmap' (zoom lens) f b

instance Functor p a => Functor (Param_a p) (Either a b) where
  fmap' lens f (Left a) = Left $ fmap' (zoom lens) f a
  fmap' lens f (Right b) = Right b

instance Functor Base t where
  fmap' _ f a = f a

-------------------
-- Maybe

mkParams ''Maybe
instance Functor p a => Functor (Param_a p) (Maybe a) where
  fmap' lens f Nothing = Nothing
  fmap' lens f (Just a) = Just $ fmap' (zoom lens) f a
