-------------------------------------------------------------------------------
Example 1: statically sized lists
-------------------------------------------------------------------------------

The classic example for using type naturals is to create a list whose size is 
represented in the type.  
In this example, we will create a type called StaticList that implements an
alternative Monoid instance for lists.

First, we set our extensions and imports:

> {-# LANGUAGE TemplateHaskell,DataKinds #-}
>
> module Main
>   where
>
> import Data.Monoid
> import Data.Params
> import System.IO

Next, we define the StaticList type as:

> newtype StaticList (len::Param Nat) a = StaticList [a]
>     deriving (Read,Show,Eq,Ord)
>
> mkParams ''StaticList

The mkParams function creates a number of helpers classes and instances.
It inspects the type parameters whose kind is a Param something, and ignores
the rest.
In this case, we create a type class called Param_len.
Our StaticList will be an instance of this type class whenever the len parameter
is valid.
More details on this in the next section.
For now, we'll see how to use the length information by creating a Monoid 
instance.
Notice that the param_len function is used to extract the type level length.

> instance 
>   ( Monoid a
>   , Param_len (StaticList len a)
>   ) => Monoid (StaticList len a) 
>       where
>
>       mempty = StaticList $ replicate n mempty
>           where 
>               n = param_len (undefined::StaticList len a)
>
>       mappend (StaticList a) (StaticList b) = StaticList $ zipWith mappend a b

Question: 
Compare the mappend function in this instance to the standard list's mappend 
function (i.e. ++).
Why do we *need* type level length information in order to write mempty so that 
we have a valid Monoid instance?

So that's how we use the type level information.
Now we need to see how to set the information.
In the following simple main function, we create two variables.
The length of the static variable is determined at compile time, whereas the length
of the dynamic variable is read from stdin.

> main = do
>       putStr "Enter a size for the dynamic list: "
>       size <- readLn
>       let static  =                     mempty :: StaticList (Static 5) (Maybe [Int])
>       let dynamic = setParam (len size) mempty :: StaticList RunTime    (Maybe [Int])
>       putStrLn $ "static  = " ++ show static
>       putStrLn $ "dynamic = " ++ show dynamic

That's it!
Check out the next example for an interesting twist using nearest neighbor searches.
