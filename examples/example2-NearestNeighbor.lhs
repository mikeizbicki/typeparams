-------------------------------------------------------------------------------
Example 2: nearest neighbor searches
-------------------------------------------------------------------------------

In this example, we will consider nearest neighbor search.
(Making neighbor search in HLearn as efficient as possible is actually what 
inspired this whole library!)

First, we set our extensions and imports:

> {-# LANGUAGE TemplateHaskell,DataKinds #-}
>
> module Main
>   where
>
> import Data.Monoid
> import Data.Params
> import System.IO

> class MetricSpace v where
>   distance :: v -> v -> Double
>
> instance MetricSpace (Double,Double) where
>   distance (x1,y1) (x2,y2) = sqrt $ (x1-x2)*(x1-x2)+(y1-y2)*(y1-y2)

> data RangeSearch 
>   (root :: Param Symbol) 
>   (k :: Param Nat) 
>   (mindist :: Param Frac) 
>   (maxdist :: Param Frac) 
>   elem
>   = RangeSearch [elem]
>
> mkParams ''RangeSearch
>
> addElem :: 
>   ( Param_mindist (RangeSearch root k mindist maxdist elem)
>   , Param_maxdist (RangeSearch root k mindist maxdist elem)
>   , MetricSpace elem
>   ) => elem -> RangeSearch root k mindist maxdist elem
> addElem e = undefined

-- > instance 
-- >   ( Param_k       (RangeSearch root k mindist maxdist elem)
-- >   , Param_mindist (RangeSearch root k mindist maxdist elem)
-- >   , Param_maxdist (RangeSearch root k mindist maxdist elem)
-- >   , MetricSpace elem
-- >   ) => Monoid (RangeSearch root k mindist maxdist elem)
-- >       where
-- >   
-- >   mempty = RangeSearch []
-- >
-- >   mappend (RangeSearch xs) (RangeSearch ys) = RangeSearch $ take k (xs++ys)
-- >       where
-- >           k = param_k (undefined::RangeSearch root k mindist maxdist elem)
-- >           mind = param_mindist (undefined::RangeSearch root k mindist maxdist elem)
-- >           maxd = param_maxdist (undefined::RangeSearch root k mindist maxdist elem)
-- >

