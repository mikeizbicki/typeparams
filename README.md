# The typeparams library

This library provides a lens-like interface for working with type parameters.  In the code:

```
data Example p1 (p2::Config Nat) (p3::Constraint) = Example
```

`p1`, `p2`, and `p3` are the type parameters.  The tutorial below uses unboxed vectors to demonstrate some of the library's capabilities.  In particular, we'll see:

1. A type safe way to unbox your unboxed vectors.  We will see a **25% speed improvement** on nearest neighbor queries.  The standard `Vector` class provided in `Data.Vector.Generic` can be used, so we retain all the stream fusion goodness.
 
2. A simple interface for [supercompilation](http://stackoverflow.com/questions/9067545/what-is-supercompilation).  In the example below, we get up to a **40x speed improvement** when calculating the Lp distance between vectors. 

Further [documentation can be found on hackage], and examples over other data types can be found in the [examples folder]().  You can download the library from github directly, or via cabal:

```
cabal update
cabal install typeparams
```

## Tutorial: unnbox your unboxed vectors!

The remainder of this README is a literate haskell file.  Please follow along yourself!

```
> import Control.Category
> import Data.Params
> import Data.Params.Vector.Unboxed 
> import Data.Params.Vector
> import qualified Data.Vector.Generic as VG
> import Prelude hiding ((.),id)
```

The `Data.Params.Vector.Unboxed` module contains the following definition for our vectors:

```
data family Vector (len::Config Nat) elem 
mkParams ''Vector
```

`mkParams` is a template haskell function that generates a number of useful functions and classes that will be described below.  The `len` type param lets us statically enforce the size of a vector as follows:

```
> v1 = VG.fromList [1..10] :: Vector (Static 10) Float
```

Here, `Static` means that the parameter is known statically at compile time.  If we don't know in advance the size of our vectors, however, we can set `len` to `Automatic`:

```
> v2 = VG.fromList [1..10] :: Vector Automatic Float
```

`v2` will behave exactly like the unboxed vectors in the `vector` package. 

The `Config` param generalizes the concept of implicit configurations introduced by [this functional pearl](http://www.cs.rutgers.edu/~ccshan/prepose/prepose.pdf) by Oleg Kiselyov and Chung-chieh Shan.  It can take on types of `Static x`, `Automatic`, or `RunTime`.  We begin by working through the capabilities of the `Static` configurations.

### From type params to values


We can get access to the value of the `len` parameter using the function:

```
viewParam :: ViewParam p t => TypeLens Base p -> t -> ParamType p
```

The singleton type `TypeLens Base p` identifies which parameter we are viewing in type `t`.  The type lens we want is `_len :: TypeLens Base Param_len`.  The value `_len` and type `Param_len` were created by the `mkParams` function above.  The significance of `Base` will be explained in a subsequent section.  

All together, we use it as:

```
ghci> viewParam _len v1
10
```

The `viewParam` function does not evaluate its arguments, so we could also call the function as:

```
ghci> viewParam _len (undefined::Vector (Static 10) Float)
10
```

We cannot use `ViewParam` if the length is being managed automatically.  `Vector Automatic Float` is not an instance of the `ViewParam` type class, so the type checker enforces this restriction automatically.

### Unboxing the vector 

If we know a vector's size at compile time, then the compiler has all the information it needs to unbox the vector.  Therefore, we can construct a 2d unboxed vector by:

```
> vv1 :: Vector (Static 2) (Vector (Static 10) Float)
> vv1 = VG.fromList [VG.fromList [1..10], VG.fromList [1..10]] 
```

or even a 3d vector by:

```
> vvv1 :: Vector (Static 20) (Vector (Static 2) (Vector (Static 10) Float))
> vvv1 = VG.replicate 20 vv1
```

In general, there are no limits to the depth the vectors can be nested.

###Viewing nested parameters

What if I want to view the length of a nested inner vector?  The value `_elem :: TypeLens p (Param_elem p)` gives us this capability.  It composes with `_len` to give the type:

```
_elem._len :: TypeLens Base (Param_elem Param_len)
```

`_elem` and `Param_elem` were also created by `mkParams`.  In general, `mkParams` will generate similar values and types for every type param of its argument.  If the type param p1 has kind `*`, then the type lens will have type `_p1 :: TypeLens p (Param_p1 p)` and the class will have kind `Param_p1 :: (* -> Constraint) -> * -> Constraint`.  If the type param has any other kind (e.g. `Config Nat`), then `mkParams` will generate `_p1 :: TypeLens Base Param_p1` and `Param_p1 :: * -> Constraint`.

The type of `_elem` allows us to combine it with `_len` to view the inner parameters of a type.  Using the vectors we created above, we can view their parameters with:

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

```
> type Monster a = Either
>   (Maybe (Vector (Static 34) Float))
>   (Either 
>       a
>       (Either 
>           (Vector (Static 2) (Vector (Static 10) Double))
>           (Vector (Static 1) Int)
>       )
>   )
```

We can do

```
ghci> viewParam (_left._just._len) (undefined::Monster Int)
34

ghci> viewParam (_right._right._left._elem._len) (undefined::Monster Float)
10
```

No matter how large the type is, we can compose `TypeLens`es to access any parameter.  

The `_left`, `_right`, and `_just` identifiers are defined in `Data.Params.Instances`.  This file contains identifiers for all the types in base.  If you want to use the library on a type not in base, all you have to do is call `mkParams` on the type constructor.

###From values back to type params

That's cool, but it's not super useful if we have to know the values of all our types at compile time.  We can use the magic of the `reflection` package to get around this.   `reflection` is awkward to use directly, so `typeparams` wraps the magic into the `RunTime` `Config` type.  (This code is based off of Austin Seipp's excellent [reflection tutorial](https://www.fpcomplete.com/user/thoughtpolice/using-reflection).)

Whenever we need to specify a `RunTime` param, we use the function:

```
with1Param :: 
    ( ParamIndex p
    ) => TypeLens Base p -> ParamType p -> ((ApplyConstraint p m) => m) -> m
```

For example, we can specify the length of the innermost vector as follows:

```
> vvv2 :: Vector (Static 1) (Vector (Static 1) (Vector RunTime Float))
> vvv2 = with1Param (_elem._elem._len) 10 $ VG.singleton $ VG.singleton $ VG.fromList [1..10] 
```

Or we can specify the length of all vectors:

```
> vvv3 :: Vector RunTime (Vector RunTime (Vector RunTime Float))
> vvv3 = with1Param (_elem._elem._len) 10 
>      $ with1Param (_elem._len) 1 
>      $ with1Param _len 1 
>      $ VG.singleton $ VG.singleton $ VG.fromList [1..10] 
```

But wait!  If we try to `show` either of these variables, we get an error message:

```
ghci> show vvv2
<interactive>:19:1:
    No instance for (Param_len (Vector 'RunTime Float))
      arising from a use of ‘print’
    In a stmt of an interactive GHCi command: print it
```

This is because `RunTime` configurations don't remember what value they were set to.  Every time we use a variable with a `RunTime` configuration, we must manually specify the value.  

A useful family of functions is the `apWith` functions.  For example:

```
apWith1Param ::
  ( ValidIndex p
  ) => TypeLens Base p
    -> ParamType p
    -> ((ApplyConstraint p m) => m -> n)
    -> ((ApplyConstraint p m) => m)
    -> n
```

These functions let us specify configurations to the arguments of a function.  So if we want to show our vectors, we could call:

```
ghci> apWith1Param (_elem._elem._len) 10 show vvv2
"fromList [fromList [fromList [1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0]]]"

ghci> apWith3Param (_elem._elem._len) 10 (_elem._len) 1 _len 1 show vvv3
"fromList [fromList [fromList [1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0]]]"
```

### Lying to the RunTime 

We can specify any number we want to a `RunTime` configuration, and can change the value throughout the course of the program.  For our `Vector` type, this will change the shape with no runtime overhead.  For example:

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

### Making it Automatic

Let's recap... `Static` configurations are easy to work with but less flexible, whereas `RunTime` configurations are a flexible pain in the butt.  We get the best of both worlds with `Automatic` configurations.

With one dimensional vectors, making the length automatic is as easy as specifying the type signature:

```
> v3 :: Vector Automatic Float
> v3 = VG.fromList [1..5]
```

Now, we can use `v3` just like we would use any vector from the `vector` package.

With multiple dimensions, we must explicitly specify the inner dimensions like so:

```
> vvv4 :: Vector Automatic (Vector Automatic (Vector Automatic Float))
> vvv4 = with1ParamAutomatic (_elem._elem._len) 5
>      $ with1ParamAutomatic (_elem._len) 2
>      $ VG.singleton $ VG.replicate 2 $ VG.fromList [1..5]
```

This is required so that the vectors can enforce that every inner vector at the same level has the correct size.  For example, the following code will give a run time error:

```
> vvv5 :: Vector Automatic (Vector Automatic (Vector Automatic Float))
> vvv5 = with1ParamAutomatic (_elem._elem._len) 5
>      $ with1ParamAutomatic (_elem._len) 2
>      $ VG.singleton $ VG.fromList [VG.fromList [1..5], VG.fromList [1..4]]
```

Using `vvv4` is as convenient as using any vectors from the `vector` package that can be nested.  For example:

```
ghci> show vvv4
fromList [fromList [fromList [1.0,2.0,3.0,4.0,5.0],fromList [1.0,2.0,3.0,4.0,5.0]]]

ghci> vvv4 VG.! 0 VG.! 1 VG.! 3
4.0

ghci> VG.foldl1' (VG.zipWith (+)) $ vvv4 VG.! 0
fromList [2.0,4.0,6.0,8.0,10.0]
```

Whew!  That's a lot to take in!  

### So how much faster?! 

The file [examples/criterion.hs](https://github.com/mikeizbicki/typeparams/blob/master/examples/criterion.hs) contains some run time experiments that show just how fast the unboxed unboxed vectors are.  In one test, it uses the naive O(n^2) algorithm to perform nearest neighbor searches.  The results are shown below:

<p align="center">
<img src="http://izbicki.me/public/cs/github/unboxed-vs-boxed.png"/>
</p>

The green line uses vectors provided in the `Data.Params.Vector.Unboxed` module of type `Vector Automatic (Vector Automatic Float)`; and the red line uses standard vectors from the `vector` package of type `Data.Vector.Vector (Data.Vector.Unboxed.Vector Float)`.  In both cases, the number of dimensions of the data points was 400.

Switching to unboxed unboxed vectors yields a nice performance boost with almost no conceptual cost to the programmer!

###Lebesgue or not to beg, that is the supercompilation 

There is a family of normed vector spaces called [Lebesgue (or Lp) spaces](https://en.wikipedia.org/wiki/Lp_space).  For a given value `p`, the Lp norm is defined as:


<p align="center">
<img src="https://upload.wikimedia.org/math/b/7/0/b7022490915ed7618b8265e45cea1df1.png"/>
</p>

In haskell code we can create a newtype that will encode the value of `p` by:

```
newtype Lebesgue (n::Config Frac) (vec :: * -> *) elem = Lebesgue (vec elem)

instance VG.Vector vec elem => VG.Vector (Lebesgue p vec) elem where
    {- ... -}

```

(See the file [examples/supercomp-lebesgue.hs](https://github.com/mikeizbicki/typeparams/blob/master/examples/supercomp-lebesgue.hs) for implementation details.)

We can then define a generic distance function over _any_ Lp space as:

```
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

The value of `p` can now be set at compile time or at run time using the `typeparams` machinery.  If we know the value at compile time, however, GHC can perform a number of optimizations:  

1.  The most important optimization is that the value of `p` never has to be stored in memory or even in a register.  The resulting assembly uses what is called [immediate instructions](http://programmedlessons.org/AssemblyTutorial/Chapter-11/ass11_2.html).  These assembly instructions are very fast in inner loops, and make the code run about 2x faster no matter what the value of `p` is.  (The example [examples/coretest.hs](https://github.com/mikeizbicki/typeparams/blob/master/examples/coretest.hs) provides a minimal code sample that facilitates inspecting the effect of different parameters on the core code and resulting assembly.)

2.  For specific values of `p`, we can optimize the formula of the Lp distance considerably.  For example, exponentiation is very slow on x86 CPUs.  Instead of evaluating `x**2`, it is much cheaper to evaluate `x*x`.  Similarly, instead of evaluating `x**(1/2)`, it is cheaper to evaluate `sqrt x`.  These optimizations are not safe for floating point numbers (small amounts of precision can be lost), so GHC doesn't perform them by default.  But we can make GHC perform them by importing the [fast-math]() library, resulting in huge speedups.  

The plot below shows the resulting run times.  The green values are the run times of the `lp_distance` function where `p` is specified using `Static`; the red for when `p` is specified using `RunTime`; and the blue for hand-optimized routines.

<p align="center">
<img src="http://izbicki.me/public/cs/github/supercomp-lebesgue.png"/>
</p>

Notice that for the L2 distnce, the compiler gives us a 40x speed boost when we switch from `RunTime` to `Static` configurations.  

By using the generic `lp_distance` function, we get all the speed advantages of hand-optimized code, but we still have the flexibility of having users enter whatever `p` value they want to compute.  We also avoid the need to manually write many hand-tuned distance functions.

##Conclusion 

The tradition use for dependent types is to make programs safer... but maybe they can make our programs faster too.  The goal of `typeparams` is to do both.  

There's still a couple of warts in the library:

1.  The classes in `vector` were never meant to be abused in this way, and so there are a small number of edge cases where this framework does not work.  For example, you cannot call `slice` on a `Static` length vector.  This throws a run time error.  Fixing this would require rewriting the vector library, which is a MAJOR undertaking.  I doubt it is worth the effort.

2.  I'm still not 100% satisfied with the way the types look.  They could probably be made much easier to use.

Please report any bugs or feature requests!
