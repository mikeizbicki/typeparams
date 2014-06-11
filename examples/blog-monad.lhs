
It's round 3 of typeparams versus GHC.  We'll be extending our Functor and Applicative classes to define a new Monad class.  It's all pretty simple if you just remember: lensified monads are like burritos where fiber optic cables tell you where to bite next.  There also just monoids in the category of lens-enhanced endofunctors.  Piece of cake.

-------------------
-- our naughty extensions

We'll be using all the same extensions as before:

> {-# LANGUAGE TemplateHaskell #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE KindSignatures #-}
> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE UndecidableInstances #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE RankNTypes #-}

But we'll be adding some pretty nasty ones today:

> {-# LANGUAGE OverlappingInstances #-}
> {-# LANGUAGE RebindableSyntax #-}

Oh well.  We're so far down the rabbit hole this doesn't really bother me.  What's a little overlapping instances compared to the rest of this nonsense?

We'll be needing all of our previous work on Functors and Applicatives.  It has been uploaded into hackage and is sitting in the appropriate modules:

> import Control.Category
> import Prelude hiding ( (.), id, Functor(..), Applicative(..), Monad(..) )
> import qualified Prelude as P
> import GHC.Exts
>
> import Data.Params
> import Data.Params.Applicative
> import Data.Params.Functor

And we're off!

-------------------
-- joins and cojoins

We will define our monads in terms of their join function.  In the standard libraries, join has the type:

join :: m (m a) -> m a

The input has the same type as the output, except that the Monad m is repeated twice.  There are two differences in the lensified join function:  First, the monad we're working with might be nested arbitrarily deeply in other data types.  Second, the argument it is monadic in might not be the last one.  Here is an example of what the join type signature would look like for the Left Either monad sitting within a Maybe Monad:

join :: TypeLens Base (Param_a (Param_a Base))
     -> Maybe (Either (Either String Int) Int)
     -> Maybe (Either String Int) 

Since we're all wannabe category theorists here, we'll create a CoJoin type family that transforms the output of the join function by duplicating the type at location specified by the lens:

> type family CoJoin (lens :: * -> Constraint) t
> type instance CoJoin lens t 
>   = SetParam' 
>       lens 
>       ( SetParam' 
>           ( Objective lens ) 
>           ( GetParam lens t ) 
>           ( GetParam (RemoveObjective lens) t )
>       ) 
>       t

(Remember that the Objective family returns the innermost type lens from our input, and the RemoveObjective familyreturns the lens that results when the innermost lens is taken away.)

CoJoin only has one instance, so we could have just used a type synonym.  That would make debugging harder, however.  The advantage of a type family is that when we ask GHCi what the type is, it will perform the substitutions for us.  For example:

ghci> :t undefined :: CoJoin (Param_a Base) (Maybe (Either String Int))
  :: Maybe (Maybe (Either String Int))

ghci> :t undefined :: CoJoin (Param_a (Param_a Base)) (Maybe (Either String Int))
  :: Maybe (Either (Either String Int) Int)

-------------------
-- monad

Now we're ready to see our new Monad class:

> class Applicative lens tfb => Monad lens tfb where
>   join ::
>       ( tffb ~ CoJoin lens tfb
>       ) => TypeLens Base lens
>         -> tffb -> tfb

The Left and Right Either instances are:

> instance Monad (Param_a Base) (Either a b) where
>   join lens (Left (Left a)) = Left a
>   join lens (Left (Right b)) = Right b
>   join lens (Right b) = Right b

> instance Monad (Param_b Base) (Either a b) where
>   join lens (Right (Right b)) = Right b
>   join lens (Right (Left a)) = Left a
>   join lens (Left a) = Left a

And here are some examples of join in action:

ghci> join _b (Right $ Right "monads") :: Either String String
Right "monads"

ghci> join _b (Right $ Left "are") :: Either String String
Left "are"

ghci> join _a (Left $ Left "so") :: Either String String
Left "so"

ghci> join _a (Right "awesome") :: Either String String
Right "awesome"

The instances above don't consider the case when our lenses point inside of the Either type.  We'll need to define two new recirsive instances to handle this case.  These instances are the reason we needed the OverlappingInstances language extension:

> instance 
>   ( Monad p a
>   , Either (CoJoin p a) b ~ CoJoin (Param_a p) (Either a b) -- follows from the lens laws
>   ) => Monad (Param_a p) (Either a b) 
>       where
>
>       join lens (Left a) = Left $ join (zoom lens) a
>       join lens (Right b) = Right b

> instance 
>   ( Monad p b
>   , Either a (CoJoin p b) ~ CoJoin (Param_b p) (Either a b) -- follows from the lens laws
>   ) => Monad (Param_b p) (Either a b) 
>       where
>
>       join lens (Left a) = Left a
>       join lens (Right b) = Right $ join (zoom lens) b

The equality constraints in the instances above are implied by the lens laws.  As we discussed yesterday, with the type rules language extension, those constraints could be removed completely, making the code a bit nicer.

Here are some examples of using join in the nested case:

ghci> join (_a._b) (Left $ Right $ Right "lenses") :: Either (Either a String) b
Left (Right "lenses")

ghci> join (_a._b) (Left $ Right $ Left "are") :: Either (Either String b) b
Left (Left "are")

ghci> join (_b._b) (Left "neat") :: Either String (Either a String)
Left "neat"

Sometimes we will get the same answer if we join in two separate locations.  In the first example below, we join the second two Right constructors, whereas in the second example, we join the first two Right constructors.  The results are the same:

ghci> join (_b._b) (Right $ Right $ Right "mind") :: Either a (Either a String)
Right (Right "mind")

ghci> join _b (Right $ Right $ Right "blowing") :: Either a (Either a String)
Right (Right "blowing")

We'll also be needing a Monad instance for Maybe, so here it is:

> instance Monad (Param_a Base) (Maybe a) where
>   join lens Nothing = Nothing 
>   join lens (Just Nothing) = Nothing
>   join lens (Just (Just a)) = Just a

> instance 
>   ( Monad p a
>   , Maybe (CoJoin p a) ~ CoJoin (Param_a p) (Maybe a) -- follows from the lens laws
>   ) => Monad (Param_a p) (Maybe a) 
>       where
>       join lens Nothing = Nothing
>       join lens (Just a) = Just $ join (zoom lens) a

-------------------
-- bind

From join and our Applicative instance, we can derive our monadic bind function.  We don't want to use the traditional (>>=) operator for bind just yet.  We will need to do something fancy with it to make do notation work out.  So instead, we will use the (\\=) operator for bind.  Its definition is:

(\\=) ::
  ( Monad lens tb
  , a ~ GetParam lens tfa
  , {- ... lens laws go here ... -}
  ) => ta -> (a -> tb) -> TypeLens Base lens -> tb

> infixl 1 \\=
> (m \\= f) lens = join lens $ fmap lens f m

We will to create the "minus bind operators" in the same way we created minus operators for the Applicative class.  Remember, the minus sign points to the parameters that will get a lens applied to them because they are "minus a lens".  These minus operators are defined as: 

> infixl 1  \\=-
> infixl 1 -\\=-
> infixl 1 -\\=
> (m  \\=- f) lens = ( m      \\= \a -> f a $ objective lens ) lens
> (m -\\=- f) lens = ( m lens \\= \a -> f a $ objective lens ) lens
> (m -\\=  f) lens = ( m lens \\= \a -> f a                  ) lens

-------------------
-- example time

For our example, we'll build a simple monadic filter.  The filterSmall function below sits in the Either Monad, but we'll be using Left to represent successes (the input passes through the filter), and Right to represent failure (the input doesn't pass through).

> filterSmall :: (Show a, Ord a) => a -> a -> Either a String
> filterSmall k x = if x > k 
>   then Left x 
>   else Right $ show x ++ " is too small"

We can call our function using the monadic bind by:

> chain1 :: Either Int String
> chain1 = at _a $ Left 20 \\= filterSmall 10 

ghci> chain1
Left 20

Instead of using the Left constructor, we can make things a little more generic by using the return function.  As usual, it is equivalent to pure:

> return :: Monad lens t
>   => GetParam lens t 
>   -> TypeLens Base lens
>   -> t
> return = pure 

Sine pure's last parameter is a type lens, we must use the left-minus (-\\=) variant of bind to sequence the computation:

> chain2 :: Either Int String
> chain2 = at _a $ return 20 -\\= filterSmall 10

ghci> chain2
Left 20

Similarly, all the bind operators take a type lens as their last parameter.  So any future binds must also use left-minus bind:

> chain3 :: Either Int String
> chain3 = at _a $ return 20 -\\= filterSmall 10 -\\= filterSmall 15

ghci> chain3
Left 20

And so on:

> chain4 :: Either Int String
> chain4 = at _a $ return 20 -\\= filterSmall 10 -\\= filterSmall 15 -\\= filterSmall 25

ghci> chain4
Right "20 is too small"

We can easily nest our monads.  Let's put all of the computations above inside a Maybe wrapper.  All we have to do is change the type signature and the lens:

> chain2' :: Maybe (Either Int String)
> chain2' = at (_a._a) $ return 20 -\\= filterSmall 10 

> chain3' :: Maybe (Either Int String)
> chain3' = at (_a._a) $ return 20 -\\= filterSmall 10 -\\= filterSmall 15

> chain4' :: Maybe (Either Int String)
> chain4' = at (_a._a) $ return 20 -\\= filterSmall 10 -\\= filterSmall 15 -\\= filterSmall 25

-------------------
-- do notation

We're using the RebindableSyntax language extension to construct a custom do notation.  We do this by defining our own (>>=) operator.  The most generic bind operator we have is the double minus bind (-\\=-).  Sometimes we will want to feed a lens to both sides of the bind, so that's what we'll use:

> infixl 1 >>=
> (m >>= f) lens = (m -\\=- f) lens

Notice that our (>>=) operator and the one from Prelude take different numbers of arguments!  GHC is awesome enough that this is not a problem.

RebindableSyntax also requires us to define functions for failed pattern matching and if statements.  Our definitions will be pretty simple:

> fail = error 

> ifThenElse False _ f = f
> ifThenElse True t _ = t

Now, we can take our chain2' function above and rewrite it in do notation.  Here it is again for easy reference:

chain2' :: Maybe (Either Int String)
chain2' = at (_a._a) $ return 20 -\\= filterSmall 10 

First, rewrite it to use (-\\=-) instead of (-\\=) by causing the right hand side to take a lens parameter even though it won't use it:

> chain2'' :: Maybe (Either Int String)
> chain2'' = at (_a._a) $ return 20 -\\=- (\x lens -> filterSmall 10 x)

Then, rewrite it using do notation:

> chain2''' :: Maybe (Either Int String)
> chain2''' = at (_a._a) $ do
>   x <- return 20
>   \lens -> filterSmall 10 x

It looks a little bit nicer if we use const to absorb the lens parameter:

> chain2'''' :: Maybe (Either Int String)
> chain2'''' = at (_a._a) $ do
>   x <- return 20
>   const $ filterSmall 10 x

Here is our other examples converted into do notation using the same technique:

> chain3''' :: Maybe (Either Int String)
> chain3''' = at (_a._a) $ do
>   x <- return 20 
>   y <- const $ filterSmall 10 x
>   const $ filterSmall 15 y

> chain4'' :: Maybe (Either Int String)
> chain4'' = at (_a._a) $ do
>   x <- return 20 
>   y <- const $ filterSmall 10 x
>   z <- const $ filterSmall 15 y
>   const $ filterSmall 25 z

And here is a more complicated expression with a nested do:

> chain5 :: Either a (Either a (Maybe (Either Int String)))
> chain5 = at (_b._b._a._a) $ do
>   x <- return 20
>   y <- do 
>       a <- const $ filterSmall x 10
>       b <- const $ filterSmall 1 3
>       return $ a+b
>   z <- const $ filterSmall y x
>   return $ z-x

But there is still a limitation.  Due to the way the types work out, the first line of a do block must always be a return statement when using the at function to specify our lens.  This is a by product of the extra lens parameter our (>>=) operator is passing around.  Fortunately, we can automate this construction with the following function:

> atM lens m = at (removeObjective lens) $ do
>   return $ at (objective lens) $ m

This lets us rewrite chain5 as:

> chain5' :: Either a (Either a (Maybe (Either Int String)))
> chain5' = atM (_b._b._a._a) $ do
>   let x = 20
>   y <- do 
>       a <- const $ filterSmall x 10
>       b <- const $ filterSmall 1 3
>       return $ a+b
>   z <- const $ filterSmall y x
>   return $ z-x

Now we fully support do notation!

Hooray!!

-------------------
--


We're almost done with this craziness...
