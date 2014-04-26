> {-# LANGUAGE TemplateHaskell #-}
>
> module Main
>   where
>
> import Data.Monoid
> import Data.Params


 
> data FunkyMonoid (a::Maybe Nat) (b::Maybe Nat) baseType
>   = FunkyMonoid baseType baseType baseType
>     deriving (Read,Show,Eq,Ord)
>
> mkParams ''FunkyMonoid

Now, let's create a monoid instance that depends on these parameters.
 
> instance 
>     ( Param_a (FunkyMonoid a b baseType)
>     , Param_b (FunkyMonoid a b baseType)
>     ) => Monoid (FunkyMonoid a b baseType) 
>         where
> 
>     mempty = FunkyMonoid a b $ a+b
>         where
>             a = param_a (undefined::FunkyMonoid a b baseType)
>             b = param_b (undefined::FunkyMonoid a b baseType)
> 
>     mappend a b = a



> main = do
>   let static = mempty :: FunkyMonoid (Just 10) (Just 11) Int
>       dynamic = setParam (SetParam_FunkyMonoid_a 10) mempty :: FunkyMonoid Nothing (Just 5) Int
>       dynamic2 = setParam (a 10) mempty :: FunkyMonoid Nothing (Just 5) Int
>   
>   putStrLn $ "static  = "++show static
>   putStrLn $ "dynamic = "++show dynamic
>   putStrLn "That was simple!"
