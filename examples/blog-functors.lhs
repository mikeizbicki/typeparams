
The typeparams package provides type lenses.  
Let's see what happens when we combine these type lenses with nested Functors.
(If you haven't read through the tutorial on the linked github page, you should do that first!)

First, enable GHC magic:

> {-# LANGUAGE TemplateHaskell #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE KindSignatures #-}
> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE UndecidableInstances #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE RankNTypes #-}
> {-# LANGUAGE OverlappingInstances #-}

and import our libraries:

> import Control.Category
> import Prelude hiding ((.),id,Functor(..))
> import Data.Params

We'll use the Either type as our main example.
It's defined as:

data Either a b = Left a | Right b

The Functor instance is pretty straightforward:

class Functor f where
    fmap :: (a -> b) -> f a -> f b

instance Functor (Either a) where
    fmap f (Left a) = Left a
    fmap f (Right b) = Right $ f b

But this instance has a key limitation: We can map a function only over the the last type.

Bifunctors are the current solution to this problem.
A recent, popular proposal suggested adding them to base (http://web.archiveorange.com/archive/v/KAst18B9pJtb8MpLOJD1).
But this is an ad hoc solution whose application does not extend far beyond the Either type.

Type lenses will (kinda sort of) provide a cleaner solution.
That is, they fix the problem about as well as regular old lenses fix the problems of record selectors.
As a bonus, we'll get a convenient mechanism for mapping over nested Functors.

-------------------
-- Defining the Functor 

Here is the alternative definition of the Functor class using type lenses:

> class Functor lens t where
>   fmap' :: a ~ GetParam lens t
>       => TypeLens p lens 
>       -> (a -> b) 
>       -> t 
>       -> SetParam lens b t

It's okay if you don't understand the type signature at first glace.  
(That's how know your using lenses, afterall!)
Let's step through it using the Either example.

The first argument is the type lens.
This indicates which parameter we will be mapping over the type t.
In the Either data type, we could use the variable _a to map over the Left component or _b to map over the Right.

Next, we encounter two type families, GetParam and SetParam.
These act as getters and setters at the type level.
In the above example, GetParam is used to extract arbitrary type params from a type.
It is defined as:

type family GetParam (p::k1) (t:: *) :: k3
type instance GetParam Param_a (Either a b) = a
type instance GetParam Param_b (Either a b) = b

The SetParam type similarly sets the type of arbitrary params in a type.
It is defined as:

type family SetParam (p::k1) (a::k2) (t:: *) :: *
type instance SetParam Param_a a' (Either a b) = Either a' b
type instance SetParam Param_b b' (Either a b) = Either a b'

These instances can be automatically provided for any type by calling the mkParams template haskell function like so:

> mkParams ''Either

Quick aside: With injective type families and a little sugar, we could make this definition of Functor a tad cleaner.  https://ghc.haskell.org/trac/ghc/ticket/6018#comment:25

-------------------
-- instances

We can replicate the traditional Functor instance with the code:

> instance Functor (Param_b Base) (Either a b) where
>   fmap' lens f (Left a) = Left a
>   fmap' lens f (Right b) = Right $ f b

and create a "Left" Functor instance as:

> instance Functor (Param_a Base) (Either a b) where
>   fmap' lens f (Left a) = Left $ f a
>   fmap' lens f (Right b) = Right b

Together, these instances let us run the commands:

ghci> fmap _b length $ Left "Roses are red,"
Left "Roses are red,"

ghci> fmap _b length $ Rightt "Violets are blue,"
Right 17

ghci> fmap _a length $ Left "Haskell is fun,"
Left 15

ghci> fmap _a length $ Right "Type lenses are cool."
Right "Type lenses are cool."

-------------------
-- nest those Functors

But wait!  There's more!

With the above definitions, we can't combine our type lenses at all.
Enter the funnily named and awkwardly typed zoom combinator:

zoom :: TypeLens a p -> TypeLens a (Zoom p)

This combinator lets us zoom into a composed type lens, removing the outer most layer.
For example, given the composed type lens:

ghci> :t _a._b._a._b
_a._b._a._b :: TypeLens a (Param_a (Param_b (Param_a (Param_b a))))

Then zooming in removes the first _a:

ghci> :t zoom (_a._b._a._b)
zoom (_a._b._a._b) :: TypeLens a (Param_b (Param_a (Param_b a)))

We will use this combinator to redefine our Functor instances.
The new instances will recursively map over every Functor in our input lens:

> instance Functor p b => Functor (Param_b p) (Either a b) where
>   fmap' lens f (Left a) = Left a
>   fmap' lens f (Right b) = Right $ fmap' (zoom lens) f b

> instance Functor p a => Functor (Param_a p) (Either a b) where
>   fmap' lens f (Left a) = Left $ fmap' (zoom lens) f a
>   fmap' lens f (Right b) = Right b

The type Base provides the base case of the recursion:

> instance Functor Base t where
>   fmap' _ f a = f a

Now, in order to call fmap', we must compose our lens with the type lens:

_base :: TypeLens Base Base

For example:

ghci> :t _a._b._a._b._base
deeplens :: TypeLens Base (Param_a (Param_b (Param_a (Param_b Base))))

And we call fmap' like:

ghci> fmap' (_a._b._a._b._base) length $ Left $ Right $ Left $ Right "still simpler than the lens package ;)"
Left (Right (Left (Right 42)))

ghci> fmap' (_a._b._a._b._base) length $ Left $ Right $ Left $ Left "... for now ..."
Left (Right (Left (Left "... for now ...")))

Composing all of our lenses with _base is tedious.
So let's write a function that automates that task:

> fmap :: 
>   ( Functor lens t
>   ) => TypeLens Base lens
>     -> (GetParam lens t -> c)
>     -> t
>     -> SetParam lens c t
> fmap lens = fmap' (lens._base)

And we call fmap as:

ghci> fmap (_a._b._a._b) length $ Left $ Right $ Left $ Left "mwahhahahaha"
Left (Right (Left (Left "mwahhahahaha")))

-------------------
-- there's everywhere!

We can easily define more of these new Functor instances.
In fact, the procedure is exactly as mechanical for type lens based Functors as it is for the traditional Functors.
All you have to do is replace every function application with a recursive Functor call:

f x --> fmap' (zoom lens) f x

Here are some examples using the list and Maybe functors:

> mkParams ''[]
> instance Functor p a => Functor (Param_a p) [a] where
>   fmap' lens f [] = []
>   fmap' lens f (a:as) = fmap' (zoom lens) f a : fmap' lens f as

> mkParams ''Maybe
> instance Functor p a => Functor (Param_a p) (Maybe a) where
>   fmap' lens f Nothing = Nothing
>   fmap' lens f (Just a) = Just $ fmap' (zoom lens) f a

Let's create a variable that uses all of our functors:

> monster = 
>   [ Nothing
>   , Just (Left "Hello!")
>   , Just (Right 42)
>   , Just (Left "World!")
>   ]

And go to town:

ghci> fmap (_a._a._a._a) succ monster
[Nothing,Just (Left "Ifmmp\""),Just (Right 42),Just (Left "Xpsme\"")]

ghci> fmap (_a._a._a) length monster
[Nothing,Just (Left 6),Just (Right 42),Just (Left 6)]

ghci> fmap (_a._a) (const 3.4) monster
[Nothing,Just 3.4,Just 3.4,Just 3.4]

ghci> fmap _a show monster
["Nothing","Just (Left \"Hello!\")","Just (Right 42)","Just (Left \"World!\")"]

-------------------
-- Tune in next time...

In our next installment, we'll tackle Applicative parsing with type lenses.
Thought the lens package had too many operators???
You 'aint seen 'nothin yet.






-------------------
-- Attempt 1


> class Functor1 lens t where
>   fmap1 :: TypeLens p (lens p) -> (GetParam lens t -> b) -> t -> SetParam lens b t

> instance Functor1 Param_b (Either a b) where
>   fmap1 lens f (Left a) = Left a
>   fmap1 lens f (Right b) = Right $ f b

> instance Functor1 Param_a (Either a b) where
>   fmap1 lens f (Left a) = Left $ f a
>   fmap1 lens f (Right b) = Right b

> testLeft = Left "hello world!"
> strRight = Right "hello world!"

ghci> fmap1 _b length strRight
Right1 12

ghci> fmap1 _a length strRight
Right1 "hello world!"
