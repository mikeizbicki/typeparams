# typeparams

This library makes it easier to work with type level parameters.  
What does that mean?  
In the example below,

> data Example p1 (p2::Config Nat) (p3::Constraint) = Example p1 p1

`p1`, `p2`, and `p3` are the type level parameters.

We get two practical benefits:

1. You can now unbox unboxed vectors, yielding about a **25% speed improvement on certain numeric tasks**.

2. A simple interface for supercompilation.  In the example below, we get a **40x speed improvement**. 

## Type Lenses

Consider the `Example` data type above.
We can generate a number of useful classes and functions via the template haskell code:

> mkParams ''Example

The first thing generated is a class for each of the type parameters.
These classes have the structure `Param_` then the name of the parameter.
So the command above generated three classes called `Param_p1`, `Param_p2`, and `Param_p3`.
These classes are used to identify which parameter we want to manipulate.

Next, we get instances of the `GetParam` and `SetParam` type families.
These act as type level getters and setters.
For example, in the following code:

> type IntType = Example Int p2 Ord
> x = 10 :: GetParam Param_p1 BigType

`x` will be created with type `Int`.

