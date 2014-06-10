We've seen how to use the typeparams library to soup up our Functor and Applicative type classes.  But we've been naughty little haskellers---we've been using type lenses without discussing their laws.  That's the subject of today's post.  **Don't worry if you didn't read/understand the previous posts.**  This post is much simpler and does not require any background.

First, we'll translate the standard lens laws to the type level.  Then we'll see how these laws can greatly simplify the type signatures of our functions.  Finally, I'll propose a very simple (yes, I promise!) GHC extension that promotes rewrite rules to the type level.  These type level rewrite rules would automatically simplify our type signatures for us.  It's pretty freakin awesome.

Today, we won't actually import anything from the typeparams library.  Instead, we'll be building up everything from scratch.

> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE PolyKinds #-}
> {-# LANGUAGE RankNTypes #-}
> {-# LANGUAGE ConstraintKinds #-}
> {-# LANGUAGE FlexibleContexts #-}

> import Control.Category
> import Prelude hiding ( (.), id )
> import Data.Proxy
> import GHC.Exts

-------------------
-- what, exactly is a type lens?

Given a data type:

> data Example a b c = Example a b c

We construct the following empty classes:

> class Param_a (p :: * -> Constraint) t -- has kind :: * -> Constraint
> class Param_b (p :: * -> Constraint) t
> class Param_c (p :: * -> Constraint) t

Each of these classes uniquely identifies one of the parameters of the Example data type.  To use these lenses, we create the singleton type:

> data TypeLens p q = TypeLens

Now, we can create three values that uinquely identify the three type parameters:

> _a = TypeLens :: TypeLens p (Param_a p)
> _b = TypeLens :: TypeLens p (Param_b p)
> _c = TypeLens :: TypeLens p (Param_c p)

We're calling these things lenses, so they must be composable.  In fact, they compose really easy.  Check out their Category instance:

> instance Category TypeLens where
>   id    = TypeLens
>   t1.t2 = TypeLens

When we chain values together using the (.) composition operator, we create a chain of classes at the type level.  For example:

ghci> :t _a._b
_a._b :: TypeLens p (Param_a (Param_b p))

ghci> :t _a._b._c
_a._b._c :: TypeLens p (Param_a (Param_b (Param_c p)))

ghci> > :t _a._a._b._c._a._b
_a._a._b._c._a._b :: TypeLens p (Param_a (Param_a (Param_b (Param_c (Param_a (Param_b p))))))

These chains of classes correspond to a nesting of data types.  For the Example type we created above, _a._b would refer to the type param b1 in the type:

Example (Example a1 b1 c1) b2 c2

_a._b._c would refer to b2 in the type:

Example (Example a1 b1 (Example a2 b2 c2)) b0 c0

and _a._a._b._c._a._b would refer to the parameter b6 in the monster type:

Example 
    ( Example 
        ( Example 
            a2 
            ( Example 
                a3 
                b3 
                ( Example
                    ( Example
                        a5
                        ( Example
                            a6
                            b6
                            c6
                        )
                        c5
                    )
                    b4
                    c4
                )
            )
            c2
        )
        b1
        c1
    )
    b0
    c0

-------------------
-- getters and setters

The whole point of lenses is they give us an easy way to get and set parameters.  At the type level, we do that with these type families:

> type family GetParam (p :: * -> Constraint) (t :: *) :: *
> type family SetParam (p :: * -> Constraint) (a :: *) (t :: *) :: *

For our Example data type, the implementations look like:

> type instance GetParam (Param_a p) (Example a b c) = GetParam p a
> type instance GetParam (Param_b p) (Example a b c) = GetParam p b
> type instance GetParam (Param_c p) (Example a b c) = GetParam p c

> type instance SetParam (Param_a p) a' (Example a b c) = Example (SetParam p a' a) b c
> type instance SetParam (Param_b p) b' (Example a b c) = Example a (SetParam p b' b) c
> type instance SetParam (Param_c p) c' (Example a b c) = Example a b (SetParam p c' c)

These definitions are recursive, so we need a base case to halt the recursion:

> class Base t
> type instance GetParam Base t = t
> type instance SetParam Base t' t = t'

Here are two example usages of the GetParam family:

ghci> :t undefined :: GetParam (Param_a Base) (Example Int Float c)
  :: Int

ghci> :t undefined :: GetParam (Param_b Base) (Example Int Float c)
  :: Float

ghci> :t undefined :: GetParam (Param_a (Param_b Base)) (Example (Example a1 Int c1) b2 Float)
  :: Int

ghci> :t undefined :: GetParam (Param_c Base) (Example (Example a1 Int c1) b2 Float)
  :: Float

And corresponding uses of the SetParam family:

ghci> :t undefined :: SetParam (Param_a Base) Float (Example Int b c)
  :: Example Float b c

ghci> :t undefined :: SetParam (Param_a (Param_b Base)) Float (Example (Example a1 Int c1) b2 c2)
  :: Example (Example a1 Float c1) b2 c2

-------------------
-- the lens laws

The first lens law is that if we set a type parameter to its current value, then the overall type does not change.  In code, this looks like:

> type LensLaw1 lens t = t ~ SetParam lens (GetParam lens t) t 

The second lens law states that if we set a type parameter to a certain value, then get the value at the location of the lens, then we should get back our original type.  In code:

> type LensLaw2 lens a t = a ~ GetParam lens (SetParam lens a t)

And lastly, if we set the same parameter twice, then the last setter wins.  In code:

> type LensLaw3 lens a b t = a ~ GetParam lens (SetParam lens a (SetParam lens b t))

There are many other laws that can be derived from these three simple laws.  For example, we can derive this fourth lens law from laws 1 and 3:

> type LensLaw4 lens a b t = SetParam lens a (SetParam lens b t) ~ SetParam lens a t

We're glossing over some technicalities involving injective type families, here, buy we'll return to this later in the post.

-------------------
-- promoting quick check to the type level

Any time we have laws in Haskell, we've got to prove that they hold.  Sometimes, parametricity does this for us automatically (as in the case of the Functor laws).  But usually, we rely on test frameworks like QuickCheck.  Therefore, we need these frameworks at the type level.

This turns out to be straightforward.  We can use these functions to verify our laws:

> property_lensLaw1 :: LensLaw1 lens t => TypeLens Base lens -> t -> ()
> property_lensLaw1 _ _ = ()

> property_lensLaw2 :: LensLaw2 lens a t => TypeLens Base lens -> a -> t -> ()
> property_lensLaw2 _ _ _ = ()

> property_lensLaw3 :: LensLaw3 lens a b t => TypeLens Base lens -> a -> b -> t -> ()
> property_lensLaw3 _ _ _ _ = ()

We test the laws as follows.  First, specialize all the type variables in the function.  Then, ask the type checker if the function is valid.  If it is, then the law holds for the type variables we chose.

Here is an example:

ghci> property_lensLaw1 _a (undefined :: Example Int Float String)
()

ghci> property_lensLaw2 _a (undefined :: String) (undefined :: Example Int Float String)
()

ghci> property_lensLaw3 _a (undefined :: String) (undefined :: [a]) (undefined :: Example Int Float String)
()

Now, let's write some GetParam/SetParam instances that do not obey the laws and see what happens.  In the NationalSecurityAgency type below, the Getter works just fine, but the Setter is broken.  

> data NationalSecurityAgency x = LawBreaker

> class Param_x (p :: * -> Constraint) t
> _x = TypeLens :: TypeLens p (Param_x p)

> type instance GetParam (Param_x p) (NationalSecurityAgency x) = x
> type instance SetParam (Param_x p) x' (NationalSecurityAgency x) = NationalSecurityAgency String

When we test the first lens law using a String, everything works fine:

ghci> lensLaw1 _x (undefined :: NationalSecurityAgency String)
()

But when we test it using an Int, the type checker explodes:

ghci> lensLaw1 _x (undefined :: NationalSecurityAgency Int)

<interactive>:73:1:
    Couldn't match type ‘[Char]’ with ‘Int’
    Expected type: SetParam
                     (Param_x Base)
                     (GetParam (Param_x Base) (NationalSecurityAgency Int))
                     (NationalSecurityAgency Int)
      Actual type: NationalSecurityAgency Int
    In the expression: lensLaw1 _x (undefined :: NationalSecurityAgency Int)
    In an equation for ‘it’:
        it = lensLaw1 _x (undefined :: NationalSecurityAgency Int)

You can imagine a template haskell quickcheck that calls these property functions many times on random types to give a probabalistic test our type laws hold.

-------------------
-- using the laws

These laws will greatly simplify inferred types in our programs.  We'll see why using an example.

Consider the beloved Applicative sequencing operator (*>) .  In the standard libraries, it has the type:

(*>) :: Applicative f => f a -> f b -> f b

Sweet and simple.

In the applicative class we generated yesterday, however, the sequencing operator is pretty nasty looking.  GHCi reports it has the type of:

> (*>) :: 
>   ( Applicative lens
>       ( SetParam
>          lens
>          (a1 -> GetParam lens (SetParam lens (a -> GetParam lens tb1) tb1))
>          (SetParam lens (a -> GetParam lens tb1) tb1)
>       )
>   , Applicative lens (SetParam lens (a -> GetParam lens tb1) tb1)
>   , Applicative lens tb1
>   , (b1 -> a2 -> a2) ~ GetParam
>       lens
>       (SetParam
>          lens
>          (a1 -> GetParam lens (SetParam lens (a -> GetParam lens tb1) tb1))
>          (SetParam lens (a -> GetParam lens tb1) tb1))
>   , a1 ~ GetParam lens (SetParam lens a1 (SetParam lens (a -> GetParam lens tb1) tb1))
>   , tb0 ~ SetParam lens a tb1
>   , ta ~ SetParam lens a1 (SetParam lens (a -> GetParam lens tb1) tb1)
>   , a ~ GetParam lens (SetParam lens a tb1)
>   ) => ta 
>     -> tb0
>     -> TypeLens Base lens 
>     -> tb1

> (*>) = undefined
> class Applicative lens t

Yikes!  What the hell does that beast do?!

Somehow, we need to simplify this type signature, and the type lens laws are what lets us do this.  For example, one of the constraints above is:

a1 ~ GetParam lens (SetParam lens a1 (SetParam lens (a -> GetParam lens tb1) tb1))

We can use the third lens law to simplify this to:

a1 ~ GetParam lens (SetParam lens a1 tb1)

If we repeat this process many times, we get a type signature that looks like:

> newop :: 
>   ( Applicative lens ( SetParam lens ( a -> b -> b ) tb )
>   , Applicative lens ( SetParam lens (      b -> b ) tb )
>   , Applicative lens tb

>   , tb ~ SetParam lens b tb

>   , LensLaw2 lens (b->b) tb
>   , LensLaw2 lens b tb
>   , LensLaw3 lens (a -> b -> b) (b -> b) tb
>   , LensLaw3 lens a (b->b) tb
>   , LensLaw4 lens (a->b->b) (b->b) tb
>   , LensLaw4 lens a (b->b) tb
>   ) => SetParam lens a tb 
>     -> tb
>     -> TypeLens Base lens 
>     -> tb
> newop = (*>)

This looks quite a bit better, but is still less than ideal.  Actually, this is as far as you can get with the lens laws in GHC 7.8.  You need injective types to go further.  So the rest of this post will be a bit more theoretical about what we might be able to do in a future GHC.

-------------------
-- injecting power into the lens laws

Let's take another look at the type synonyms for the lens laws:

type LensLaw1 lens t = t ~ SetParam lens (GetParam lens t) t
type LensLaw2 lens a t = a ~ GetParam lens (SetParam lens a t)
type LensLaw3 lens a b t = a ~ GetParam lens (SetParam lens a (SetParam lens b t))

This code only enforces that the laws hold for certain parameters.  But that's not what we want!  All types are equal in the eyes of the law, so what we really want is type synonyms that look like:

type LensLaw1' = forall lens t. t ~ SetParam lens (GetParam lens t) t
type LensLaw2' = forall lens a t. a ~ GetParam lens (SetParam lens a t)
type LensLaw3' = forall lens a b t. a ~ GetParam lens (SetParam lens a (SetParam lens b t))

Unfortunately, sticking this into GHC yields the dreaded "type families may not be injective" error message.  With injective type families, we would be able to write these laws, and then our code would simplify further to:

newop' :: 
  ( Applicative lens ( SetParam lens ( a -> b -> b ) tb )
  , Applicative lens ( SetParam lens (      b -> b ) tb )
  , Applicative lens tb

  , tb ~ SetParam lens b tb

  , LensLaw1'
  , LensLaw2'
  , LensLaw3'
  ) => SetParam lens a tb 
    -> tb
    -> TypeLens Base lens 
    -> tb
newop' = (*>)

-------------------
-- a proposal for new syntax

We can still do better.  The lens laws are not something that applies only to specific functions.  They are global properties of the type families, and they apply everywhere.  Therefore, they should be implicitly added as constraints into every type signature.

We could make this happen by adding a new syntax called "type rules".  (The name comes from analogy with the rewrite rules at the value level.)  The syntax could look something like:

type rule LensLaw1' = forall lens t. t ~ SetParam lens (GetParam lens t) t
type rule LensLaw2' = forall lens a t. a ~ GetParam lens (SetParam lens a t)
type rule LensLaw3' = forall lens a b t. a ~ GetParam lens (SetParam lens a (SetParam lens b t))

And would allow us to write our function as:

newop'' :: 
  ( Applicative lens ( SetParam lens ( a -> b -> b ) tb )
  , Applicative lens ( SetParam lens (      b -> b ) tb )
  , Applicative lens tb

  , tb ~ SetParam lens b tb

  ) => SetParam lens a tb 
    -> tb
    -> TypeLens Base lens 
    -> tb
newop'' = (*>)

That is soooo much nicer!  

-------------------
-- Stay tuned

We still have some work to go to get our newop function's type signature as simple as (*>) from the standard library.  But I think we've got a realistic shot at it.  In a coming post I'll be proposing a way to combine the multiple Applicative constraints into a single constraint, and a nice looking sugar over the SetParam/GetParam type families.

