{-# LANGUAGE TemplateHaskell #-}

module Data.Params.Functor
    where

import Control.Category
import Prelude hiding ((.), id, Functor(..), Applicative(..))

import Data.Maybe
import Data.Params

import GHC.Exts

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

-- type instance Objective (Param_a p) = Objective_Param_a (Param_a p)
-- type family Objective_Param_a (lens :: * -> Constraint) :: * -> Constraint where
--   Objective_Param_a (Param_a Base) = Param_a Base
--   Objective_Param_a (Param_a p) = Objective p
-- 
-- type instance Objective (Param_b p) = Objective_Param_b (Param_b p)
-- type family Objective_Param_b (lens :: * -> Constraint) :: * -> Constraint where
--   Objective_Param_b (Param_b Base) = Param_b Base
--   Objective_Param_b (Param_b p) = Objective p


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
