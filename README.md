# The typeparams library

This library provides a lens-like interface for working with type parameters.  In the code:

```
data Example p1 (p2::Config Nat) (p3::Constraint) = Example
```

`p1`, `p2`, and `p3` are the type parameters.  The tutorial below uses unboxed vectors to demonstrate some of the library's capabilities.  In particular, we'll see:

1. A type safe way to unbox your unboxed vectors.   This technique gives a **25% speed improvement** on nearest neighbor queries.  The standard `Vector` class provided in `Data.Vector.Generic` can be used, so we retain all the stream fusion goodness.
 
2. A simple interface for [supercompilation](http://stackoverflow.com/questions/9067545/what-is-supercompilation).  In the example below, we combine this library and the [fast-math](https://github.com/liyang/fast-math) library to get up to a **40x speed improvement** when calculating the Lp distance between vectors.   

Further [documentation can be found on hackage](http://hackage.haskell.org/package/typeparams), and examples with non-vector data types can be found in the [examples folder](https://github.com/mikeizbicki/typeparams/tree/master/examples).  You can download the library from github directly, or via cabal:

```
cabal update
cabal install typeparams
```

## Tutorial: unbox your unboxed vectors!

The remainder of this README is a literate haskell file.  Please follow along yourself!

```
> import Control.Category
> import Data.Params
> import Data.Params.Vector.Unboxed 
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

The `Config` param generalizes the concept of implicit configurations introduced by [this functional pearl](http://www.cs.rutgers.edu/~ccshan/prepose/prepose.pdf) by Oleg Kiselyov and Chung-chieh Shan.  (See also the [ImplicitParams GHC extension](http://www.haskell.org/ghc/docs/latest/html/users_guide/other-type-extensions.html#implicit-parameters).)  It can take on types of `Static x`, `Automatic`, or `RunTime`.  This tutorial will begin by working through the capabilities of the `Static` configurations before discussing the other options.

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
> vv1 = VG.fromList [VG.fromList [1..10], VG.fromList [21..30]] 
```

or even a 3d vector by:

```
> vvv1 :: Vector (Static 20) (Vector (Static 2) (Vector (Static 10) Float))
> vvv1 = VG.replicate 20 vv1
```

In general, there are no limits to the depth the vectors can be nested.

###Viewing nested parameters

What if we want to view the length of a nested inner vector?  The value `_elem :: TypeLens p (Param_elem p)` gives us this capability.  It composes with `_len` to give the type:

```
_elem._len :: TypeLens Base (Param_elem Param_len)
```

`_elem` and `Param_elem` were also created by `mkParams`.  In general, `mkParams` will generate these type lenses for every type param of its argument.  If the type param `p1` has kind `*`, then the type lens will have type `_p1 :: TypeLens p (Param_p1 p)` and the class will have kind `Param_p1 :: (* -> Constraint) -> * -> Constraint`.  If the type param has any other kind (e.g. `Config Nat`), then `mkParams` will generate `_p1 :: TypeLens Base Param_p1` and `Param_p1 :: * -> Constraint`.

The type of `_elem` allows us to combine it with `_len` to view the inner parameters of a type.  Using the vectors we created above, we can view their parameters with:

```
ghci> viewParam _len vv1
2

ghci> viewParam (_elem._len) vv1
10

ghci> viewParam _len vvv1
20

ghci> viewParam (_elem._len) vvv1
2

ghci> viewParam (_elem._elem._len) vvv1
10
```

###Lensing into giant types

What if instead of having a `Vector` of `Vector`s, we have some other data type of `Vectors`?  For example, what if we have a `Maybe (Vector len elem)`.  Now, how can we get access to the length of the vector?

Consider the definition of `Maybe`:

```
data Maybe a = Nothing | Just a
```

If we run the following template haskell:

```
> mkParams ''Maybe
```

then we will generate the type lens `_a :: TypeLens p (Param_a p)` which will give us the desired capability:

```
ghci> viewParam (_a._len) (undefined :: Maybe (Vector (Static 10) Int))
10
```

We can do the same process for any data type, even if the names of their type params overlap.  For example, we can run:

```
> mkparams ''Either
```

This will reuse the already created `_a` type lens (which corresponds to the left component of `Either`) and generate the type lens `_b :: TypeLens p (Param_b p)` (which corresponds to the right component).

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

We can do:

```
ghci> viewParam (_a._a._len) (undefined::Monster Int)
34

ghci> viewParam (_b._b._a._elem._len) (undefined::Monster Float)
10
```

No matter how large the type is, we can compose `TypeLens`es to access any configuration parameter.  

It would be nice if the type lenses for these built in data types had more meaningful names (like `_just`,`_left`, and `_right`), but this would require a change to base.

###From values back to type params

That's cool, but it's not super useful if we have to know the values of all our configurations at compile time.  The `RunTime` and `Automatic` `Config` values give us more flexibility.  We will see that the `RunTime` method is powerful but cumbersome, and the `Automatic` method will provide a much simpler interface that wraps the `RunTime` method.

(The `RunTime` configurations use the magic of the [reflection](http://hackage.haskell.org/package/reflection) package.   The internal code is based off of Austin Seipp's excellent [reflection tutorial](https://www.fpcomplete.com/user/thoughtpolice/using-reflection).)

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

The `with1Param` function is only useful when we pass parameters to the output of whatever function we are calling.  In the example of `show`, however, we need to pass parameters to the input of the function.  We do this using the function:

```
apWith1Param ::
  ( ValidIndex p
  ) => TypeLens Base p
    -> ParamType p
    -> ((ApplyConstraint p m) => m -> n)
    -> ((ApplyConstraint p m) => m)
    -> n
```

Similar functions exist for passing more than one parameter.  These functions let us specify configurations to the arguments of a function.  So if we want to show our vectors, we could call:

```
ghci> apWith1Param (_elem._elem._len) 10 show vvv2
"fromList [fromList [fromList [1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0]]]"

ghci> apWith3Param (_elem._elem._len) 10 (_elem._len) 1 _len 1 show vvv3
"fromList [fromList [fromList [1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0]]]"
```

#### A bug in GHC!

Unfortunately, due to a [bug in GHC 7.8.2's typechecker](https://ghc.haskell.org/trac/ghc/ticket/9090), the above code doesn't typecheck.  We must explicitly specify the specialized type of `apWithNParam` for it to work.  This is syntactically very awkward.  As a temporary workaround, the library provides the function:

```
apWith1Param' :: m -> (
    ( ParamIndex p
    )  => TypeLens Base p
       -> ParamType p
       -> (ApplyConstraint p m => m -> n)
       -> (ApplyConstraint p m => m)
       -> n
       )
```

The only difference is that the unconstrained type `m` is passed as the first argument, which causes the `apWith1Param'` function's type signature to be specialized for us correctly.  We can use this function like:

```
ghci> apWith1Param' vvv2 (_elem._elem._len) 10 show vvv2 :: String
"fromList [fromList [fromList [1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0]]]"

ghci> apWith3Param vvv3 (_elem._elem._len) 10 (_elem._len) 1 _len 1 show vvv3 :: String
"fromList [fromList [fromList [1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0]]]"
```

Notice that we can use the same variable as both the first and last parameter.  This gives us a useable workaround in the presence of the GHC bug.

### Lying to the `RunTime` 

We can specify any value we want to a `RunTime` configuration.  We can even change the value throughout the course of the program.  For our `Vector` type, this will change the shape with no runtime overhead.  For example:

```
ghci> apWith3Param' vvv3 (_elem._elem._len) 2 (_elem._len) 5 _len 1 show vvv3 :: String
fromList [fromList [fromList [1.0,2.0]
                   ,fromList [3.0,4.0]
                   ,fromList [5.0,6.0]
                   ,fromList [7.0,8.0]
                   ,fromList [9.0,10.0]
                   ]]
```

Of course, we must be careful.  If we specify lengths that cause the size of the result to exceed the allocated `ByteArray`, then we will get undefined results:

```
ghci> apWith3Param vvv3 (_elem._elem._len) 2 (_elem._len) 5 _len 2 show vvv3 :: String
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

### Making it `Automatic`

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

When using `Automatic` parameters, there is no need for the `apWithNParam` family of functions.  Internally, the type will store the value of the configuration.  Whenever the value is needed, `apWith1Param` is called for us automatically.

### So how much faster?! 

The file [examples/criterion.hs](https://github.com/mikeizbicki/typeparams/blob/master/examples/criterion.hs) contains some run time experiments that show just how fast the unboxed unboxed vectors are.  In one test, it uses the naive O(n^2) algorithm to perform nearest neighbor searches.  The results are shown below:

<p align="center">
<img src="http://izbicki.me/public/cs/github/unboxed-vs-boxed.png"/>
</p>

The green line uses vectors provided in the `Data.Params.Vector.Unboxed` module of type `Vector Automatic (Vector Automatic Float)`; and the red line uses standard vectors from the `vector` package of type `Data.Vector.Vector (Data.Vector.Unboxed.Vector Float)`.  In both cases, the number of dimensions of the data points was 400.

Switching to unboxed unboxed vectors yields a nice performance boost of about 25%.  The best part is that we barely have to change existing code at all.  The only difference between the interface for a boxed unboxed vector and an unboxed unboxed vector is the initial construction.  **If you have code that creates boxed unboxed vectors, you should get a similar performance gain switching over to this library.**

###Lebesgue or not to beg, that is the supercompilation 

If we combine this typeparams package with the [fast-math](https://github.com/liyang/fast-math) package, we get a very simple form of supercompilation.  To demonstrate how this works, we will use the example of distance calculations in arbitrary [Lebesgue (Lp) spaces](https://en.wikipedia.org/wiki/Lp_space).  For a given value `p`, the Lp norm is defined as:

<p align="center">
<img src="https://upload.wikimedia.org/math/b/7/0/b7022490915ed7618b8265e45cea1df1.png"/>
</p>

In haskell code we can create a `newtype` that will encode the value of `p` by:

```
newtype Lebesgue (p::Config Frac) (vec :: * -> *) elem = Lebesgue (vec elem)

instance VG.Vector vec elem => VG.Vector (Lebesgue p vec) elem where
    {- ... -}

```

The `Frac` kind is similar to the `Nat` kind, except it represents any positive fraction at the type level.  The file [src/Data/Params/Frac.hs](https://github.com/mikeizbicki/typeparams/blob/master/src/Data/Params/Frac.hs) contains the implementation of `Frac`.  The file [examples/supercomp-lebesgue.hs](https://github.com/mikeizbicki/typeparams/blob/master/examples/supercomp-lebesgue.hs) for contains the implementation details of the `Lebesgue` example.

We can then define a generic distance function over _any_ Lp space as:

```
lp_distance :: 
    ( VG.Vector vec elem
    , Floating elem
    , ViewParam Param_p (Lebesgue p vec elem)
    ) => Lebesgue p vec elem -> Lebesgue p vec elem -> elem
lp_distance !v1 !v2 = (go 0 (VG.length v1-1))**(1/p)
    where
        p = viewParam _p v1

        go tot (-1) = tot
        go tot i = go (tot+diff1**p) (i-1)
            where 
                diff1 = abs $ v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i
```

The value of `p` can now be set at compile time or at run time using the typeparams machinery.  If we know the value at compile time, however, GHC can perform a number of optimizations:  

1.  The most important optimization is that the value of `p` never has to be stored in memory or even in a register.  The resulting assembly uses what is called [immediate instructions](http://programmedlessons.org/AssemblyTutorial/Chapter-11/ass11_2.html).  These assembly instructions are very fast in inner loops, and make the code run about 2x faster no matter what the value of `p` is.  (The example [examples/coretest.hs](https://github.com/mikeizbicki/typeparams/blob/master/examples/coretest.hs) provides a minimal code sample that facilitates inspecting the effect of different parameters on the core code and resulting assembly.)

2.  For specific values of `p`, we can optimize the formula of the Lp distance considerably.  For example, exponentiation is very slow on x86 CPUs.  Instead of evaluating `x**2`, it is much cheaper to evaluate `x*x`.  Similarly, instead of evaluating `x**(1/2)`, it is cheaper to evaluate `sqrt x`.  These optimizations are not safe for floating point numbers (small amounts of precision can be lost), so GHC doesn't perform them by default.  The [fast-math](https://github.com/liyang/fast-math) library is needed to cause these optimizations. 

The plot below shows the resulting run times:   

<p align="center">
<img src="http://izbicki.me/public/cs/github/supercomp-lebesgue2.png"/>
</p>

The green values are the run times of the `lp_distance` function where `p` is specified using `Static`; the red for when `p` is specified using `RunTime`; and the blue for hand-optimized routines.  Hashed columns indicate the test was run with the `Numeric.FastMath` import.  All code was compiled using llvm and the optimization flags: `-optlo -O3 -optlo -enable-unsafe-fp-math`.  Notice that there are some cases where the fast-math library is able to perform optimizations that llvm's `-enable-unsafe-fp-math` flag cannot.

By using the generic `lp_distance` function, we get all the speed advantages of hand-optimized code, but we still have the flexibility of having users enter whatever `p` value they want to compute.  We also avoid the need to manually write many hand-tuned distance functions.

##Thoughts for the road

It is popular to think of these type level configurations as "lightweight dependent types."  The traditional use for dependent types is to make programs safer... but maybe they can make our programs faster too!?  Exploring both of these possibilities is the goal of `typeparams` library.

There's still a couple of warts in the library:

1.  The classes in the vector library were never meant to be abused in this way, and so there are a small number of edge cases where this framework does not work.  For example, you cannot call `slice` on a `Static` length vector.  This throws a run time error.  Fixing this would require rewriting the vector library, which is a MAJOR undertaking.  

2.  The `mkParams` template haskell function currently only makes the necessary instances for `Static` and `RunTime` configurations.  The infrastructure for `Automatic` configurations must be done manually.  It is possible to automatically produce the required infrastructure for `Automatic` configurations as well, but I haven't figured out a way to do it without introducing overhead that's usually unnecessary.

3.  For simplicity, this package only currently implements unboxed vectors in this framework.  There is no reason, however, that boxed vectors and storable vectors could not be implemented as well.  This would allow storable storable vectors using all the same techniques as above.

**Please report any bugs/issues/feature requests!**
