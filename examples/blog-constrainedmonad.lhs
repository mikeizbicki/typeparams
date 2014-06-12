> {-# LANGUAGE TemplateHaskell #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE KindSignatures #-}
> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE UndecidableInstances #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE RankNTypes #-}

> {-# LANGUAGE RebindableSyntax #-}

> import Control.Category
> import Prelude hiding ((.), id, Functor(..), Applicative(..), Monad(..) )
> import qualified Data.Map as Map
> import Data.Monoid

> import Data.Params
> import Data.Params.Applicative
> import Data.Params.Functor
> import Data.Params.Monad

> import Debug.Trace

-------------------
-- define the Map 

> mkParams ''Map.Map
> instance (Ord k, Functor p k) => Functor (Param_k p) (Map.Map k a) where
>   fmap' lens f m = Map.mapKeys (fmap' (zoom lens) f) m

> instance (Ord k, Applicative p k, Monoid a) => Applicative (Param_k p) (Map.Map k a) where
>   pure a lens = Map.singleton (pure a $ zoom lens) mempty 
>   
>   -- ap function does not make sense because there is no Ord instance for (a -> b)
>   -- can someone explain to me categorically why this happens?

> instance (Ord k, Monoid a) => Monad (Param_k Base) (Map.Map k a) where
>   join lens m 
>       = foldr (Map.unionWith (<>)) mempty 
>       $ map (\(k,v) -> Map.map (<>v) k)
>       $ Map.toList m 

> instance Functor p a => Functor (Param_a p) (Map.Map k a) where
>   fmap' lens f m = Map.map (fmap' (zoom lens) f) $ m

> instance (Applicative p a, Monoid k) => Applicative (Param_a p) (Map.Map k a) where
>   pure a lens = Map.singleton mempty $ pure a $ zoom lens

-- > instance Monoid k => Monad (Param_a Base) (Map.Map k a) where
-- >   join lens m = _ $ Map.toList m

-- > q _ m = Map.fromList m

-------------------
-- example

> languages :: Map.Map String String
> languages = Map.fromList 
>   [ ( "agda"    , "crazier than this blog?" )
>   , ( "bash"    , "quite handy"             )
>   , ( "c++"     , "better than c?"          )
>   , ( "c"       , "better than c++?"        )
>   , ( "haskell" , "awesome"                 )
>   , ( "python"  , "better than c++"         )
>   ]

> key2value :: String -> Map.Map String String
> key2value str = Map.singleton str str

> stringToValue :: Ord k => String -> k -> Map.Map k String
> stringToValue str k = Map.singleton k str

> sentences :: Map.Map String String
> sentences = at _k $ do
>   k <- const languages
>   const $ stringToValue " is " k
>   const $ key2value k
>   return $ k

ghci> sentences
fromList [("agda","agda is crazier than this blog?")
         ,("bash","bash is quite handy")
         ,("c++","c++ is better than c?")
         ,("c","c is better than c++?")
         ,("haskell","hakell is awesome")
         ,("python","python is better than c++")
         ]

