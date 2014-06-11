module Data.Params.Applicative
    where

import Control.Category
import Prelude hiding ( (.), id, Functor(..), Applicative(..) )

import Data.Params
import Data.Params.Functor

-------------------------------------------------------------------------------
-- Applicative class

class Functor lens tb => Applicative lens tb where

  pure :: GetParam lens tb -> TypeLens Base lens -> tb

  ap :: 
      ( tf ~ SetParam lens (a -> b) tb
      , ta ~ SetParam lens a tb
      , a ~ GetParam lens ta
      , b ~ GetParam lens tb
      )
     => TypeLens Base lens 
     -> tf
     -> ta
     -> tb

---------------------------------------
-- instances

instance Applicative Base t where
  pure a _ = a
  ap _ f = f

-------------------

instance Applicative p a => Applicative (Param_a p) (Maybe a) where
  pure a lens = Just $ pure a (zoom lens)
  ap lens Nothing _ = Nothing
  ap lens (Just f) Nothing = Nothing
  ap lens (Just f) (Just b) = Just $ ap (zoom lens) f b

-------------------

instance Applicative p a => Applicative (Param_a p) (Either a b) where
  pure a lens = Left $ pure a (zoom lens)
  ap lens (Right a) _ = Right a
  ap lens (Left f) (Right a) = Right a
  ap lens (Left f) (Left b) = Left $ ap (zoom lens) f b

instance Applicative p b => Applicative (Param_b p) (Either a b) where
  pure b lens = Right $ pure b (zoom lens)
  ap lens (Left a) _ = Left a
  ap lens (Right f) (Left a) = Left a
  ap lens (Right f) (Right b) = Right $ ap (zoom lens) f b

-------------------------------------------------------------------------------
-- combinators

infixl 4 <$>
(<$>) :: 
  ( Functor lens tb
  , b ~ GetParam lens tb
  , ta ~ SetParam lens a tb
  ) => (a -> b) 
    -> ta
    -> TypeLens Base lens
    -> tb
(f <$> t) lens = fmap lens f t

infixr 0 @@
(@@) :: (TypeLens p q -> b) -> TypeLens p q -> b
(@@) = id

at :: TypeLens q p -> (TypeLens q p -> t) -> t
at lens f = f lens

infixl 4 <*>
(<*>) ::
  ( Applicative lens tb
  , tf ~ SetParam lens (a -> b) tb
  , ta ~ SetParam lens a tb
  , a ~ GetParam lens ta
  , b ~ GetParam lens tb
  ) => (TypeLens Base lens -> tf)
    -> ta
    -> (TypeLens Base lens -> tb)
(<*>) tf ta lens = ap lens (tf lens) ta

infixl 4 <*>-
(tf <*>- ta) lens = (tf <*> ta lens) lens

infixl 4 <*
(u <* v) lens = pure const <*> u <*> v @@ lens

infixl 4 *>
(u *> v) lens = pure (const id) <*> u <*> v @@ lens

infixl 4  <*-
infixl 4 -<*-
infixl 4 -<*
(u  <*- v) lens = ( u      <* v lens ) lens
(u -<*- v) lens = ( u lens <* v lens ) lens
(u -<*  v) lens = ( u lens <* v      ) lens

infixl 4  *>-
infixl 4 -*>-
infixl 4 -*>
(u  *>- v) lens = ( u      *> v lens ) lens
(u -*>- v) lens = ( u lens *> v lens ) lens
(u -*>  v) lens = ( u lens *> v      ) lens

