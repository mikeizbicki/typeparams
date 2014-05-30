# typeparams

This library makes it easier to work with type parameters.  In the example below,

> data Example p1 (p2::Config Nat) (p3::Constraint) = Example

`p1`, `p2`, and `p3` are the type parameters.  The typeparams package provides a new, lens-like interface for manipulating these parameters.  This README first describes this interface, then shows two concrete uses of the interface:

1. A type safe way to unbox your unboxed vectors.  In the example below, we will see a **25% speed improvement** on nearest neighbor queries.

2. A simple interface for supercompilation.  In the example below, we get a **40x speed improvement** when calculating the Lp distance of vectors. 

The library can be installed from hackage by running:

> cabal update
> cabal install typeparams

## Type Lenses

Consider the `Example` data type above.  We can generate a number of useful classes and functions via the template haskell code:

> mkParams ''Example

The first thing generated is a class for each of the type parameters.  These classes have the form `Param_` then the name of the parameter.  So the command above generated three classes called `Param_p1`, `Param_p2`, and `Param_p3`.  These classes are used to identify which parameter we want to manipulate.

Next, we get instances of the `GetParam` and `SetParam` type families.  These act as type level getters and setters.  For example, in the following code:

> type IntType = Example Int p2 Ord
> x = 10 :: GetParam Param_p1 BigType   -_

`x` will have type `Int`.  Similarly, 

> y = Example :: SetParam Param_p1_ Float IntType 

gives `y` type `Example Float p2 Ord`.

## Unboxed unboxed vectors

> import Data.Params.Vector.Unboxed as VPU
> import Data.Vector.Generic as VG

The `Data.Params.Vector.Unboxed` module contains the following definition:

> data family Vector (len::Config Nat) elem 

The `len` type param lets us statically enforce the size of the vector as follows:

> v1 = VG.fromList [1..10] :: VPU.Vector (Static 10) Float

Here, `Static` means that the parameter is known statically at compile time.  If we don't know in advance the size of our vectors, however, we can set `len` to `Automatic`:

> v2 = VG.fromList [1..10] :: VPU.Vector Automatic Float

`v2` will behave exactly like the vectors in the `vector` package.  What's important is that the `Config` kind is letting use the same vector type whether we know the size or not.

We can get access to the value of the `len` parameter using the function:

> viewParam :: ViewParam p t => TypeLens Base p -> t -> ParamType p

Here, a `TypeLens Base p` is a singleton type that identifies which parameter we are viewing in type `t`.  (The significance of `Base` will be explained later.)  The type lens we want is `_len :: TypeLens Base Param_len`.  All together, we use it as:

> viewParam _len 

### Unboxing the vector

If we know a vector's size at compile time, then the compiler has all the information it needs to unbox the vector.  Therefore, we can construct a 2d unboxed vector by:

> vv1 :: VPU.Vector (Static 2) (VPU.Vector (Static 10) Float)
> vv1 = VG.fromList [VG.fromList [1..10], VG.fromList [1..10]] 

or even a 3d vector by:

> vvv1 :: VPU.Vector (Static 20) (VPU.Vector (Static 2) (VPU.Vector (Static 10) Float))
> vvv1 = VG.replicate 20 vv1

In general, there are no limits to the depth the vectors get nested.

But what if we don't know 
