module Data.Params.Monad
    where

import Control.Category
import Prelude hiding ( (.), id, Functor(..), Applicative(..), Monad(..) )
import qualified Prelude as P
import GHC.Exts

import Data.Params hiding ( (\\) )
import Data.Params.Applicative
import Data.Params.Functor

-------------------------------------------------------------------------------
-- Monad class

class Applicative lens tfb => Monad lens tfb where
    join :: tffb ~ CoJoin lens tfb
         => TypeLens Base lens -> tffb -> tfb

type family CoJoin (lens :: * -> Constraint) t
type instance CoJoin lens t 
  = SetParam' 
      lens 
      ( SetParam' 
          ( Objective lens ) 
          ( GetParam lens t ) 
          ( GetParam (RemoveObjective lens) t )
      ) 
      t

-------------------------------------------------------------------------------
-- functions

return :: Monad lens t
  => GetParam lens t 
  -> TypeLens Base lens
  -> t
return = pure 

infixl 1 \\=
(m \\= f) lens = join lens $ fmap lens f m

infixl 1  \\=-
infixl 1 -\\=-
infixl 1 -\\=
(m  \\=- f) lens = ( m      \\= \a -> f a $ objective lens ) lens
(m -\\=- f) lens = ( m lens \\= \a -> f a $ objective lens ) lens
(m -\\=  f) lens = ( m lens \\= \a -> f a                  ) lens

atM lens m = at (removeObjective lens) $ do
    return $ at (objective lens) $ m

-------------------


-- > (\\) :: forall a b ma mb lens.
-- >   ( b ~ GetParam lens mb 
-- >   , ma ~ SetParam lens a mb
-- >   , ma ~ SetParam lens (GetParam lens ma) mb
-- >   , a ~ GetParam lens ma
-- >   ) => ma -> mb -> TypeLens Base lens -> mb

(m \\ f) lens = (m \\= \ (_::String) -> f) lens

-- FIXME: The compiler can't figure out the type of (\\) without the String annotation;  GHC bug?

infixl 1 \\

infixl 1  \\-
infixl 1 -\\-
infixl 1 -\\
( m  \\- f ) lens = (      m \\ f (objective lens) ) lens
( m -\\- f ) lens = ( m lens \\ f (objective lens) ) lens
( m -\\  f ) lens = ( m lens \\ f                  ) lens

-------------------
-- do notation

infixl 1 >>=
(m >>= f) lens = (m -\\=- f) lens

infixl 1 >>
m >> f = m -\\- f

fail = error 

ifThenElse False _ f = f
ifThenElse True t _ = t


-------------------------------------------------------------------------------
-- instances

instance Monad (Param_a Base) (Either a b) where
    join lens (Left (Left a)) = Left a
    join lens (Left (Right b)) = Right b
    join lens (Right b) = Right b

instance Monad (Param_b Base) (Either a b) where
    join lens (Right (Right b)) = Right b
    join lens (Right (Left a)) = Left a
    join lens (Left a) = Left a

instance 
    ( Monad p a
    , Either (CoJoin p a) b ~ CoJoin (Param_a p) (Either a b) -- follows from the lens laws
    ) => Monad (Param_a p) (Either a b) 
        where

    join lens (Left a) = Left $ join (zoom lens) a
    join lens (Right b) = Right b

instance 
    ( Monad p b
    , Either a (CoJoin p b) ~ CoJoin (Param_b p) (Either a b) -- follows from the lens laws
    ) => Monad (Param_b p) (Either a b) 
        where

    join lens (Left a) = Left a
    join lens (Right b) = Right $ join (zoom lens) b

---------------------------------------

instance Monad (Param_a Base) (Maybe a) where
    join lens Nothing = Nothing 
    join lens (Just Nothing) = Nothing
    join lens (Just (Just a)) = Just a

instance 
    ( Monad p a
    , Maybe (CoJoin p a) ~ CoJoin (Param_a p) (Maybe a) -- follows from the lens laws
    ) => Monad (Param_a p) (Maybe a) 
      where

    join lens Nothing = Nothing
    join lens (Just a) = Just $ join (zoom lens) a

