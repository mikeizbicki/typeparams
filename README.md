typeparams
----------

This library provides a lens-like interface for working with type parameters.  In the example below,

```
data Example p1 (p2::Config Nat) (p3::Constraint) = Example
```

`p1`, `p2`, and `p3` are the type parameters.  This README first describes this interface, then shows two concrete uses cases:

1. A type safe way to unbox your unboxed vectors.  In the example below, we will see a **25% speed improvement** on nearest neighbor queries.  The standard `Vector` class provided in `Data.Vector.Generic` can be used, so we retain all the stream fusion goodness.
 
2. A simple interface for supercompilation.  In the example below, we get a **40x speed improvement** when calculating the Lp distance of vectors. 

Further [documentation can be found on hackage], and more examples can be found in the examples folder.  This README is a literate haskell file.  You can follow along by first installing the library:

```
cabal update
cabal install typeparams
```

Type Lenses #################################################################

Consider the `Example` data type above.  We can generate a number of useful classes and functions via the template haskell code:

```
mkParams ''Example
```

The first thing generated is a class for each of the type parameters.  These classes have the form `Param_` then the name of the parameter.  So the command above generated three classes called `Param_p1`, `Param_p2`, and `Param_p3`.  These classes are used to identify which parameter we want to manipulate.

Next, we get instances of the `GetParam` and `SetParam` type families.  These act as type level getters and setters.  For example, in the following code:

```
type IntType = Example Int p2 Ord
x = 10 :: GetParam Param_p1 BigType
```

`x` will have type `Int`.  Similarly, 

```
y = Example :: SetParam Param_p1_ Float IntType 
```

gives `y` type `Example Float p2 Ord`.

Unboxed unboxed vectors ####################################################

We'll begin by looking at how to create unboxed vectors of unboxed vectors.

> import Control.Category
> import Data.Params
> import Data.Params.Vector.Unboxed as VPU
> import Data.Params.Vector
> import qualified Data.Vector.Generic as VG
> import Prelude hiding ((.),id)

The `Data.Params.Vector.Unboxed` module contains the following definition:

```
data family Vector (len::Config Nat) elem 
```

The `len` type param lets us statically enforce the size of the vector as follows:

> v1 = VG.fromList [1..10] :: VPU.Vector (Static 10) Float

Here, `Static` means that the parameter is known statically at compile time.  If we don't know in advance the size of our vectors, however, we can set `len` to `Automatic`:

> v2 = VG.fromList [1..10] :: VPU.Vector Automatic Float

`v2` will behave exactly like the unboxed vectors in the `vector` package. 

The type of a `Config` param can also be set to `RunTime`.  Before we go into the detils of `Automatic` and `RunTime`, however, let's look at what we can do with statically known configurations.

Statically sized vectors ###################################################

Accessing the parameters ##################################################

We can get access to the value of the `len` parameter using the function:

```
viewParam :: ViewParam p t => TypeLens Base p -> t -> ParamType p
```

Here, a `TypeLens Base p` is a singleton type that identifies which parameter we are viewing in type `t`.  (The significance of `Base` will be explained shortly.)  The type lens we want is `_len :: TypeLens Base Param_len`.  All together, we use it as:

```
ghci> viewParam _len v1
10
```

The `viewParam` function does not `seq` its arguments, so we could also call the function as:

```
ghci> viewParam _len (undefined::Vector (Static 10) Float)
10
```

We cannot use `ViewParam` if the length is being managed automatically.  `Vector Automatic Float` is not an instance of the `ViewParam` type class, so the type checker enforces this restriction automatically.

Unboxing the vector #######################################################

If we know a vector's size at compile time, then the compiler has all the information it needs to unbox the vector.  Therefore, we can construct a 2d unboxed vector by:

> vv1 :: VPU.Vector (Static 2) (VPU.Vector (Static 10) Float)
> vv1 = VG.fromList [VG.fromList [1..10], VG.fromList [1..10]] 

or even a 3d vector by:

> vvv1 :: VPU.Vector (Static 20) (VPU.Vector (Static 2) (VPU.Vector (Static 10) Float))
> vvv1 = VG.replicate 20 vv1

In general, there are no limits to the depth the vectors can be nested.

Nested access to the parameters ###########################################

Now, what if I want to access the length of an inner nested vector?  The value `_elem :: TypeLens p (Param_elem p)` gives us this capability.  It composes with `_len` to give the type:

```
_elem._len :: TypeLens Base (Param_elem Param_len)
```

The type signature tells us that the lens corresponds to the `len` param of whatever is sitting in the `elem` param.  For example:

```
ghci> viewParam _len vv1
1

ghci> viewParam (_elem._len) vv1
10

ghci> viewParam _len vvv1
20

ghci> viewParam (_elem._len) vvv1
2

ghci> viewParam (_elem._elem._len) vvv1
10
```

We can use type lenses in this fashion to extract parameters from truly monstrous types.  For example, given the type:

> type Monster a = Either
>   (Maybe (VPU.Vector (Static 34) Float))
>   (Either 
>       a
>       (Either 
>           (VPU.Vector (Static 2) (VPU.Vector (Static 10) Double))
>           (VPU.Vector (Static 1) Int)
>       )
>   )

We can do

```
ghci> viewParam (_left._just._len) (undefined::Monster Int)
34

ghci> viewParam (_right._right._left._elem._len) (undefined::Monster Float)
10
```

No matter how large the type is, there is a `TypeLens` that lets us access every parameter.  (The `_left`, `_right`, and `_just` type lenses are defined in `Data.Params.Instances`.)

RunTime and Automatic params ###############################################

That's cool, but it's not super useful if we have to know the values of all our types at compile time.  We can use the magic of the `reflection` package to get around this.   `reflection` is pretty difficult to use, however, so `typeparams` wraps the magic into the `RunTime` `Config` type.  (This code is based off of Austin Seipp's excellent [https://www.fpcomplete.com/user/thoughtpolice/using-reflection reflection tutorial].)

Whenever we need to specify a `RunTime` param, we use the function 

```
with1Param :: 
    ( ParamIndex p
    ) => TypeLens Base p -> ParamType p -> ((ApplyConstraint p m) => m) -> m
```

For example, we can specify the length of the innermost vector as follows:

> vvv2 :: Vector (Static 1) (Vector (Static 1) (Vector RunTime Float))
> vvv2 = with1Param (_elem._elem._len) 10 $ VG.singleton $ VG.singleton $ VG.fromList [1..10] 

Or we can specify the length of all vectors:

> vvv3 :: Vector RunTime (Vector RunTime (Vector RunTime Float))
> vvv3 = with1Param (_elem._elem._len) 10 
>      $ with1Param (_elem._len) 1 
>      $ with1Param _len 1 
>      $ VG.singleton $ VG.singleton $ VG.fromList [1..10] 

But wait!  If we try to `show` these variables, we get an error message:

```
ghci> show vvv2
<interactive>:19:1:
    No instance for (Param_len (Vector 'RunTime Float))
      arising from a use of ‘print’
    In a stmt of an interactive GHCi command: print it
```

That's because `RunTime` configurations don't remember what value they were set to.  If we want to use these variables, we must manually respecify the value using the function:

```
apWith1Param ::
  ( ValidIndex p
  ) => TypeLens Base p
    -> ParamType p
    -> ((ApplyConstraint p m) => m -> n)
    -> ((ApplyConstraint p m) => m)
    -> n
```

It gets used like:

```
ghci> apWith1Param (_elem._elem._len) 10 show vvv2
"fromList [fromList [fromList [1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0]]]"

ghci> apWith3Param (_elem._elem._len) 10 (_elem._len) 1 _len 1 show vvv3
"fromList [fromList [fromList [1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0]]]"
```

Why do I have to manually specify the length of a vector?  Can't it be inferred?

The answer is yes in this specific example, but not in general.  The unique thing about a `RunTime` parameter is that it retains no information about the value of the parameter after it is used.  So in this example, the `Vector`s will immediately forget how big they are.  If we want the `Vector` to remember, then we will need to use the `Automatic` parameter.

Lying to the RunTime ########################################################

We can specify any number we want to the vector's len parameter.  This will change the vector's shape with no runtime overhead.  For example

```
ghci> apWith3Param (_elem._elem._len) 2 (_elem._len) 5 _len 1 show vvv3
fromList [fromList [fromList [1.0,2.0]
                   ,fromList [3.0,4.0]
                   ,fromList [5.0,6.0]
                   ,fromList [7.0,8.0]
                   ,fromList [9.0,10.0]
                   ]]
```

Of course, we must be careful.  If we specify lengths that cause the size of the result to exceed the allocated `ByteArray`, then we will get undefined results:

```
ghci> apWith3Param (_elem._elem._len) 2 (_elem._len) 5 _len 1 show vvv3
fromList [fromList [fromList [1.0,2.0]
                   ,fromList [3.0,4.0]
                   ,fromList [5.0,6.0]
                   ,fromList [7.0,8.0]
                   ,fromList [9.0,10.0]
                   ]]
         ,fromList [fromList [-1.7796708,4.5566e-41]
                   ,fromList [-1.46142,4.5566e-41]
                   ,fromList [-1.5570038e-7,4.5566e-41]
                   ,fromList [-1.701097e-5,4.5566e-41]
                   ,fromList [1.23e-43,0.0]]
         ]
```

(I've manually reformatted the output of show to make it easier to read.)

Making it Automatic ###########################################################

Let's recap... `Static` configurations are easy to work with but less flexible, whereas `RunTime` configurations are a flexible pain in the butt.  We get the best of both worlds with `Automatic` configurations.

With one dimensional vectors, making the length automatic is as easy as specifying the type signature:

> v3 = VG.fromList [1..5] :: Vector Automatic Float

With multiple dimensions, we must explicitly specify the dimension like so:

> vvv4 :: Vector Automatic (Vector Automatic (Vector Automatic Float))
> vvv4 = with1ParamAutomatic (_elem._elem._len) 5
>      $ with1ParamAutomatic (_elem._len) 2
>      $ VG.singleton $ VG.replicate 2 $ VG.fromList [1..5]

Now, we can use `vvv4` just like any other vector:

```
ghci> show vvv4
fromList [fromList [fromList [1.0,2.0,3.0,4.0,5.0],fromList [1.0,2.0,3.0,4.0,5.0]]]

ghci> vvv4 VG.! 0 VG.! 1 VG.! 3
4.0

ghci> VG.foldl1' (VG.zipWith (+)) $ vvv4 VG.! 0
fromList [2.0,4.0,6.0,8.0,10.0]
```

So how much faster?! ########################################################

In the examples folder is a file called `criterion.hs`.  This file performs a number of performance tests.In particular, it uses the naive O(n^2) algorithm to perform nearest neighbor searches.  

Turning up the magic to over 9000 ############################################

If look at the code of a `Static` vector versus a `RunTime` one, we see some magic happening.  The compiler will inline any constants specified in a `Config` into the raw assembly as what's called an "immediate" instruction.  These instructions don't have to do any

```
newtype Lebesgue (n::Config Frac) (vec :: * -> *) elem = Lebesgue (vec elem)

instance VG.Vector vec elem => VG.Vector (Lebesgue p vec) elem where
    {- ... -}

lp_distance :: 
    ( VG.Vector vec elem
    , Floating elem
    , ViewParam Param_n (Lebesgue n vec elem)
    ) => Lebesgue n vec elem -> Lebesgue n vec elem -> elem
lp_distance !v1 !v2 = (go 0 (VG.length v1-1))**(1/n)
    where
        n = viewParam _n v1

        go tot (-1) = tot
        go tot i = go (tot+diff1**n) (i-1)
            where 
                diff1 = abs $ v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i
```

Combined with the `fast-math` library, GHC does a butt-ton of optimizations for us automatically.  

TODO #########################################################################

There's still a couple of warts that need to be ironed out.

1.  The classes in `vector` were never meant to be abused in this way, and so there are a small number of edge cases where this framework does not work.  For example, you cannot call `slice` on a `Static` length vector.  This throws a run time error.  Fixing this would require rewriting the vector library, which is a MAJOR undertaking.  I doubt it is worth the effort.

2.  I'm still not 100% satisfied with the way the types look.  They could probably be made cleaner.

