

> {-# LANGUAGE TemplateHaskell #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE KindSignatures #-}
> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE UndecidableInstances #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE RankNTypes #-}
> {-# LANGUAGE OverloadedStrings #-}

We've got a few more imports today to handle all our parsing needs:

> import Control.Category
> import Prelude hiding ((.), id, Functor(..), Applicative(..))
> import qualified Prelude as P
> import Data.Params
> import Data.Params.Functor

> import qualified Control.Applicative as Ap
> import qualified Data.Attoparsec.Text as A
> import Data.Attoparsec.Text (parse,Parser,Result)
> import Data.Monoid
> import Data.Text (Text,pack)

-------------------
-- Functor bonus

> infixl 4 <$>
> (<$>) :: 
>   ( Functor lens tb
>   , b ~ GetParam lens tb
>   , ta ~ SetParam lens a tb
>   ) => (a -> b) 
>     -> ta
>     -> TypeLens Base lens
>     -> tb
> (f <$> t) lens = fmap lens f t

ghci> length <$> (Left $ Right "test") $ _a._b
Left (Right 4)

We will find it useful to introduce a symbol just for specifying the type lens.  Since a lens specifies the location "at" which we are operating, we call our new operator @@.  It is defined as:

> infixr 0 @@
> (@@) :: (TypeLens p q -> b) -> TypeLens p q -> b
> (@@) = id

Use it like:

ghci> length <$> (Left $ Right "test") @@ _a._b
Left (Right 4)

Of course, one of the lens laws is that we must provide both prefix and infix versions of every combinator.  Therefore we also introduce the function:

> at :: TypeLens q p -> (TypeLens q p -> t) -> t
> at lens f = f lens

ghci> at (_a._b) $ length <$> (Left $ Right "test") 
Left (Right 4)

-------------------
-- Applicative

At last we're ready to see our new Applicative class:

> class Functor lens tb => Applicative lens tb where
>
>   pure :: GetParam lens tb -> TypeLens Base lens -> tb
>
>   ap :: 
>       ( tf ~ SetParam lens (a -> b) tb
>       , ta ~ SetParam lens a tb
>       , a ~ GetParam lens ta
>       , b ~ GetParam lens tb
>       )
>      => TypeLens Base lens 
>      -> tf
>      -> ta
>      -> tb

The functions pure and ap have the exact same meaning and laws as their counterparts in the standard libraries.  The only difference is the addition of the TypeLens parameter and corresponding constraints.

The Left and Right Applicative instances for the Either class are defined as:

> instance Applicative p a => Applicative (Param_a p) (Either a b) where
>   pure a lens = Left $ pure a (zoom lens)
>   ap lens (Right a) _ = Right a
>   ap lens (Left f) (Right a) = Right a
>   ap lens (Left f) (Left b) = Left $ ap (zoom lens) f b

> instance Applicative p b => Applicative (Param_b p) (Either a b) where
>   pure b lens = Right $ pure b (zoom lens)
>   ap lens (Left a) _ = Left a
>   ap lens (Right f) (Left a) = Left a
>   ap lens (Right f) (Right b) = Right $ ap (zoom lens) f b

And just like with Functors, we have to define the base case for our recusive definitions:

> instance Applicative Base t where
>   pure a _ = a
>   ap _ f = f

Now, to get the Applicative notation we all know and love, we redefine the <*> operator.  It is just a thin wrapper around the ap function in the class that rearranges the parameters to be more convenient:

> infixl 4 <*>
> (<*>) ::
>   ( Applicative lens tb
>   , tf ~ SetParam lens (a -> b) tb
>   , ta ~ SetParam lens a tb
>   , a ~ GetParam lens ta
>   , b ~ GetParam lens tb
>   ) => (TypeLens Base lens -> tf)
>     -> ta
>     -> (TypeLens Base lens -> tb)
> (<*>) tf ta lens = ap lens (tf lens) ta

Whew!

Now we're ready to test it out.  

We'll start with the doubly nested Either.  For nested Eithers, the lens we use specifies what the success constructors are.  Any other constructors will act as errors.

Here's an example without an error:

> fact1 :: Either (Either a String) b
> fact1 = (++) <$> Left (Right "haskell") <*> Left (Right " rocks!") @@ _a._b

ghci> fact1
Left (Right "haskell rocks!")

Here we have one possible way of signaling an error:

> fact2 :: Either (Either a String) String
> fact2 = (++) <$> Left (Right "python") <*> Right "error" @@ _a._b

ghci> fact2
Right "error"

And here we have the other way:

> fact3 :: Either (Either String String) b
> fact3 = (++) <$> Left (Right "c++") <*> Left (Left "error") @@ _a._b

ghci> fact3
Left (Left "error")

Of course, Applicatives are much more useful when our functions have many arguments.  Let's create a function that concatenates four strings together into a phrase:

> cat4 :: String -> String -> String -> String -> String
> cat4 a b c d = a ++ " " ++ b ++ " "++ c ++ " " ++ d

And create a phrase with no errors:

> phrase1 :: Either (Either a String) b
> phrase1 =  cat4 
>   <$> Left (Right "haskell")
>   <*> Left (Right "is")
>   <*> Left (Right "super")
>   <*> Left (Right "awesome")
>   @@ _a._b

ghci> phrase1
Left (Right "haskell is super awesome")

And a phrase with two errors:

> phrase2 :: Either (Either String String) String
> phrase2 =  cat4 
>   <$> Left (Right "python")
>   <*> Right "error"
>   <*> Left (Right "is")
>   <*> Left (Left "error")
>   @@ _a._b

ghci> phrase2
Right "error"

Notice that in phrase2 we had two different causes of errors.  The error with the fewest number of terms will always win.

> phrase3 :: Either (Either String String) String
> phrase3 =  cat4 
>   <$> Left (Right "python")
>   <*> Left (Left "error")
>   <*> Left (Right "is")
>   <*> Right "error"
>   @@ _a._b

ghci> phrase3
Right "error"

-------------------
-- making it pure

This is cool, but it's not yet very generic.  Everytime we want a success, we have to manually specify the constructors we want to use.  We can avoid that using the pure function.  It's type signature is:

pure :: Applicative lens tb 
     => GetParam lens tb -> TypeLens Base lens -> tb

The important thing to notice is that the second to last parameter takes a TypeLens.  We can substitute it into our phrase1 variable like:

> phrase1' :: Either (Either a String) b
> phrase1' =  cat4 
>   <$> (pure "haskell" @@ _a._b)
>   <*> (pure "is"      @@ _a._b)
>   <*> (pure "super"   @@ _a._b)
>   <*> (pure "awesome" @@ _a._b)
>   @@ _a._b

But this is nasty!  We have to specify the same TypeLens everywhere we want to use the pure function.

Thankfully, we don't have to do this.  The whole point of lenses is to create crazy new combinators that reduce boilerplate!  So let's do that!  The "ap minus" combinator will automatically apply the lens for us:

> infixl 4 <*>-
> (tf <*>- ta) lens = (tf <*> ta lens) lens

The minus sign signifies that the right side is "minus a lens" and so we should give it one automtically.  Using this combinator, we can rewrite our phrase to look like:

> phrase1'' :: Either (Either a String) b
> phrase1'' =  cat4 
>   <$> (pure "haskell" @@ _a._b)
>   <*>- pure "is"
>   <*>- pure "super"
>   <*>- pure "awesome"
>   @@ _a._b

In order to get rid of the first lens application, we'll need to perform the same trick to <$>:

> infixl 4 <$>-
> (f <$>- t) lens = (f <$> t lens) lens

And we get the beautiful:

> phrase1''' :: Either (Either a String) b
> phrase1''' =  cat4 
>   <$>- pure "haskell" 
>   <*>- pure "is"
>   <*>- pure "super"
>   <*>- pure "awesome"
>   @@ _a._b

-------------------
-- combinatorics with combinators

> (<*) :: 
>   ( Applicative lens ( SetParam lens ( b -> GetParam lens tf ) tf )
>   , Applicative lens tf
>   , Applicative lens tb
>   , tf ~ SetParam lens (a -> b) tb
>   , ta ~ SetParam lens a tb
>   , a ~ GetParam lens ta
>   , b ~ GetParam lens tb
>   , b ~ GetParam lens (SetParam lens b tf) 
>   , ( a -> b -> a ) ~ GetParam lens ( SetParam lens ( b -> GetParam lens tf ) tf )
>   ) => SetParam lens b tf
>     -> ta
>     -> TypeLens Base lens 
>     -> tb

There's two more Applicative combinators needed for parsing: *> and <* .  They use the same definition in the standard libraries, but with a third lens parameter:

> infixl 4 <*
> (u <* v) lens = pure const <*> u <*> v @@ lens

> infixl 4 *>
> (u *> v) lens = pure (const id) <*> u <*> v @@ lens

> (*>) :: 
>   ( Applicative lens ( SetParam lens ( a -> b -> b ) tb )
>   , Applicative lens ( SetParam lens (      b -> b ) tb )
>   , Applicative lens tb

-- >   , b ~ GetParam lens tb
-- >   , tb ~ SetParam lens b tb
-- >   --, LensLaw1 lens tb

>   , LensLaw1 lens tb
>   , LensLaw4 lens (a->b->b) (b->b) tb
>   , LensLaw4 lens a (b->b) tb
>   , LensLaw3 lens (a -> b -> b) (b -> b) tb
>   , LensLaw2 lens (b->b) tb
>   , LensLaw2 lens b tb
>   , LensLaw3 lens a (b->b) tb
>   ) => SetParam lens a tb 
>     -> tb
>     -> TypeLens Base lens 
>     -> tb

-- > type instance SetParam Base t' t = t'
-- > type instance GetParam Base t = t

> type LensLaw1 lens t = t ~ SetParam lens (GetParam lens t) t 
> type LensLaw2 lens a t = a ~ GetParam lens (SetParam lens a t)
> type LensLaw3 lens a b t = a ~ GetParam lens (SetParam lens a (SetParam lens b t))

> type LensLaw4 lens a b t = SetParam lens a (SetParam lens b t) ~ SetParam lens a t

-- > (*>) :: 
-- >   ( Applicative lens
-- >       ( SetParam
-- >          lens
-- >          (a1 -> GetParam lens (SetParam lens (a -> GetParam lens tb1) tb1))
-- >          (SetParam lens (a -> GetParam lens tb1) tb1)
-- >       )
-- >   , Applicative lens (SetParam lens (a -> GetParam lens tb1) tb1), Applicative lens tb1
-- >   , GetParam lens (SetParam lens a tb1) ~ a
-- >   , (b1 -> a2 -> a2) ~ GetParam
-- >       lens
-- >       (SetParam
-- >          lens
-- >          (a1 -> GetParam lens (SetParam lens (a -> GetParam lens tb1) tb1))
-- >          (SetParam lens (a -> GetParam lens tb1) tb1))
-- >   , a1 ~ GetParam lens (SetParam lens a1 (SetParam lens (a -> GetParam lens tb1) tb1))
-- >   , tb0 ~ SetParam lens a tb1
-- >   , ta ~ SetParam lens a1 (SetParam lens (a -> GetParam lens tb1) tb1)
-- >   ) => ta 
-- >     -> tb0
-- >     -> TypeLens Base lens 
-- >     -> tb1

Now we need to create all of the "minus" operators.  Remember that the minus sign points to the variable that will have the lens automatically applied for us:

> infixl 4  <*-
> infixl 4 -<*-
> infixl 4 -<*
> (u  <*- v) lens = ( u      <* v lens ) lens
> (u -<*- v) lens = ( u lens <* v lens ) lens
> (u -<*  v) lens = ( u lens <* v      ) lens

> infixl 4  *>-
> infixl 4 -*>-
> infixl 4 -*>
> (u  *>- v) lens = ( u      *> v lens ) lens
> (u -*>- v) lens = ( u lens *> v lens ) lens
> (u -*>  v) lens = ( u lens *> v      ) lens

Just remember the pattern, and you'll be fine!

-------------------
-- Parse time

Now for the really juicy bits.  We've already imported the attoparsec library.  We'll use the built-in "blind" Functor and Applicative instances to define our lensified ones as:

> mkParams ''Parser

> instance Functor p a => Functor (Param_a p) (Parser a) where
>   fmap' lens f parser = P.fmap (fmap' (zoom lens) f) parser

> instance Applicative (Param_a Base) (Parser a) where
>   pure a lens = Ap.pure $ pure a (zoom lens)
>   ap lens tf ta = tf Ap.<*> ta

And now we're ready to start parsing.  We'll start simple.  The attoparsec library provides a function called string that mathes a specified string.  We'll use it to create a Parser that matches the phrase "haskell rocks":

> chain1 :: TypeLens Base (Param_a Base) -> Parser Text
> chain1 = A.string "haskell" *> A.string " rocks"

ghci> parse (chain1 @@ _a) "haskell rocks"
Done "" " rocks"

In the above example, we chose to *not* specify the lens in the chain1 variable.  This means that if we want to chain it with another parser, we should use the minus then operator like:

> chain2 :: TypeLens Base (Param_a Base) -> Parser Text
> chain2 = chain1 -*> A.string "!"

ghci> parse (chain2 @@ _a) "haskell rocks!"
Done "" "!"

If we choose to compose on the right, then we'll need to move the minus sign to the right:

> chain3 :: TypeLens Base (Param_a Base) -> Parser Text
> chain3 = A.string "¡" *>- chain2 

ghci> parse (chain3 @@ _a) "¡haskell rocks!"
Done "" "!"

We have to use minus operators whenever we chain more than two parsers together.  In the example below, the first *> takes three parameters (two parsers and a lens).  It gets the lens from the minus of the first -*> operator.  That operator also needs a lens, which it gets from the next -*>, and so on.

> chain4 :: TypeLens Base (Param_a Base) -> Parser Text
> chain4 = A.string "do" *> A.string " you" -*> A.string " get" -*> A.string " it" -*> A.string " yet?"

ghci> parse (chain4 @@ _a) "do you get it yet?"
Done "" " yet?"

If we need to apply a lens to both sides, then we use the -*>- operator:

> chain5 :: TypeLens Base (Param_a Base) -> Parser Text
> chain5 = chain3 -*> A.string " ... " -*>- chain4

ghci> parse (chain5 @@ _a) "¡haskell rocks! ... do you get it yet?"
Done "" " yet?"

-------------------
-- stacking parsers

Everything in the last section we could have done without type lenses.  But now we're going to lift the Parser into an arbitrary data type and work with it.  

As a concrete example, we'll put our Parser inside a Maybe.  The Maybe Applicative instance is:

> instance Applicative p a => Applicative (Param_a p) (Maybe a) where
>   pure a lens = Just $ pure a (zoom lens)
>   ap lens Nothing _ = Nothing
>   ap lens (Just f) Nothing = Nothing
>   ap lens (Just f) (Just b) = Just $ ap (zoom lens) f b

And for convenience we'll use the following parseMaybe function.  It has the same effect as the parse function provided by attoparsec, but does everything from within a Maybe.

> parseMaybe :: Maybe (Parser a) -> Text -> Maybe (Result a)
> parseMaybe parser str = flip parse str <$> parser @@ _a

Next, we lensify our parser combinators.  This string lifts the string function provided by the attoparsec library into an arbitrary parameter specified by our type lens:

-- > string :: 
-- >   ( Applicative (Zoom p) tb
-- >   , GetParam (Zoom p) tb ~ Parser Text
-- >   ) => Text -> TypeLens Base p -> tb

> string c lens = pure (A.string c) (zoom lens)

At last we can get back to parsing.  

Let's just repeat the same 5 parse chains from above, but now within the Maybe context.  Notice two things;

1) The A.string function provided by the attoparsec library did not take a type parameter, but our new string function does.  This means there's a lot more minus combinators!

2) Instead of specifying our lens to focus on the _a parameter, we must focus on the _a._a parameter to hit the parser.

> chain1' :: TypeLens Base (Param_a (Param_a Base)) -> Maybe (Parser Text)
> chain1' = string "haskell" -*>- string " rocks"

ghci> parseMaybe (chain1' @@ _a._a) "haskell rocks"
Just Done "" " rocks"

> chain2' :: TypeLens Base (Param_a (Param_a Base)) -> Maybe (Parser Text)
> chain2' = chain1' -*>- string "!"

ghci> parse (chain2' @@ _a._a) "haskell rocks!"
Done "" '!'

> chain3' :: TypeLens Base (Param_a (Param_a Base)) -> Maybe (Parser Text)
> chain3' = string "¡" -*>- chain2' 

ghci> parse (chain3' @@ _a._a) "¡haskell rocks!"
Done "" '!'

> chain4' :: TypeLens Base (Param_a (Param_a Base)) -> Maybe (Parser Text)
> chain4' = string "do" -*>- string " you" -*>- string " get" -*>- string " it" -*>- string " yet?"

ghci> parse (chain4' @@ _a._a) "do you get it yet?"
Done "" " yet?"

> chain5' :: TypeLens Base (Param_a (Param_a Base)) -> Maybe (Parser Text)
> chain5' = chain3' -*>- string " ... " -*>- chain4'

ghci> parse (chain5' @@ _a._a) "¡haskell rocks! ... do you get it yet?"
Done "" " yet?"

And now what happens if we add a Maybe into the chain?  Nothing takes over and eats the whole Parser.  It doesn't matter if the Parse was failing or succeeding, the answer is Nothing.

> chain6 :: TypeLens Base (Param_a (Param_a Base)) -> Maybe (Parser Text)
> chain6 = string "python" -*> Nothing

ghci> parseMaybe (chain6 @@ _a._a) "python"
Nothing

ghci> parseMaybe (chain6 @@ _a._a) "haskell"
Nothing

-------------------
-- Alternative

In order for our parser to really be useful, we need to make choices and branch.  The Alternative class gives us this power.  It is defined as:

> class Applicative lens t => Alternative lens t where
>   empty :: TypeLens Base lens -> t
>   alt :: TypeLens Base lens -> t -> t -> t

With a Parser instance taken directly from the standard alternative class:

> instance Alternative (Param_a Base) (Parser a) where
>   empty _ = Ap.empty
>   alt _ = (Ap.<|>)

Now, we must again redefine our operators to use lenses.  Here is the <|> operator and its minus cousins:

> infixl 3 <|>
> (<|>) :: Alternative lens t => t -> t -> TypeLens Base lens -> t
> (t1 <|> t2) lens = alt lens t1 t2

> infixl 3  <|>-
> infixl 3 -<|>-
> infixl 3 -<|> 
> (t1  <|>- t2) lens = ( t1      <|> t2 lens ) lens
> (t1 -<|>- t2) lens = ( t1 lens <|> t2 lens ) lens
> (t1 -<|>  t2) lens = ( t1 lens <|> t2      ) lens

From this, we can derive the some and many combinators:

> some v = (:) <$>- v <*>- many v 
> many v = some v -<|>- pure [] 

Which we can use like:

> moto :: Parser [Text]
> moto = string "give me " -*>- some (string "haskell") @@ _a

ghci> parse moto "give me haskell!!!!!!!!!!!"
Done "!!!!!!!!!!" ["haskell"]

----

-- > instance Alternative p a => Alternative (Param_a p) (Maybe a) where
-- >   empty _ = Nothing
-- >   alt lens (Just a) Nothing  = trace "a" $ Nothing -- Just a
-- >   alt lens (Just a) (Just b) = trace "b" $ Just $ a <|> b @@ zoom lens
-- >   alt lens Nothing  (Just a) = trace "c" $ Just a
-- >   alt lens Nothing  Nothing  = trace "d" $ Nothing
-- 
-- > instance Alternative Base a => Alternative Base (Maybe a) where
-- >   empty _ = Nothing
-- >   alt _ (Just a) Nothing  = trace "e" $ Nothing
-- >   alt _ (Just a) (Just b) = trace "f" $ Just $ a <|> b @@ _base
-- >   alt _ Nothing  (Just a) = trace "g" $ Nothing
-- >   alt _ Nothing  Nothing  = trace "h" $ Nothing
-- 
-- 
-- > ilove   = string (pack "I love " ) @@ _a._a :: Maybe (Parser Text)
-- > haskell = string (pack "haskell" ) @@ _a._a :: Maybe (Parser Text)
-- > lenses  = string (pack "lenses"  ) @@ _a._a :: Maybe (Parser Text)
-- > python  = string (pack "python"  ) @@ _a._a :: Maybe (Parser Text)
-- 
-- > sentence :: Maybe (Parser Text)
-- > sentence = haskell <|>- (python *> Nothing) -<|> lenses @@ _a._a -- -<|>- haskell -<|>- lenses @@ _a._a

-------------------
-- parsers in parsers

-- > instance Applicative p a => Applicative (Param_a p) (Parser a) where
-- >   pure a lens = Ap.pure $ pure a (zoom lens)
-- >   ap lens tf ta = ap (zoom lens) <$> tf <*> ta @@ _a

-------------------
-- circuit parsing

Now we're ready for some super coolness.  We're going to design a parsing circuit that parses three unique Parse streams simultaneously!

Here is our Circuit definition:

> data Circuit x y z 
>   = Circuit (Maybe x) (Maybe y) (Maybe z)
>   | CircuitFail
>   deriving (Show)
> mkParams ''Circuit

The x, y, and z type params will hold the Parsers.  These Parsers are wrapped within a Maybe.  A value of Nothing represents that that parser will not consume any input.  A value if (Just parser) means that it will consume input.

The Functor instances are rather interesting because of the Maybe wrapper.  We must add a _a lens to the zoomed lens to make the types work out:

> instance Functor p x => Functor (Param_x p) (Circuit x y z) where
>   fmap' lens f CircuitFail = CircuitFail
>   fmap' lens f (Circuit x y z) = Circuit (fmap' (_a . zoom lens) f x) y z

> instance Functor p y => Functor (Param_y p) (Circuit x y z) where
>   fmap' lens f CircuitFail = CircuitFail
>   fmap' lens f (Circuit x y z) = Circuit x (fmap' (_a . zoom lens) f y) z

> instance Functor p z => Functor (Param_z p) (Circuit x y z) where
>   fmap' lens f CircuitFail = CircuitFail
>   fmap' lens f (Circuit x y z) = Circuit x y (fmap' (_a . zoom lens) f z)

The Applicative instances are where all the action is at.  In each case, the pure function is fairly straightforward.  It looks just like the other ones we've seen except that it applies the _a to the zoomed lens and gives default values of Nothing to the other parsers.

The ap function is where the magic lies.

> instance 
>   ( Applicative p x
>   , Monoid y
>   , Monoid z
>   ) => Applicative (Param_x p) (Circuit x y z) 
>       where
>   pure x lens = Circuit (pure x @@ (_a . zoom lens)) Nothing Nothing
>   ap lens CircuitFail _ = CircuitFail
>   ap lens _ CircuitFail = CircuitFail
>   ap lens (Circuit x1 y1 z1) (Circuit x2 y2 z2) = Circuit 
>       (ap (_a . zoom lens) x1 x2) 
>       (getFirst $ First y1 <> First y2) 
>       (getFirst $ First z1 <> First z2)

> instance (Monoid x, Applicative p y, Monoid z) => Applicative (Param_y p) (Circuit x y z) where
>   pure a lens = Circuit Nothing (pure a @@ _a . zoom lens) Nothing
>   ap lens CircuitFail _ = CircuitFail
>   ap lens _ CircuitFail = CircuitFail
>   ap lens (Circuit x1 y1 z1) (Circuit x2 y2 z2) = Circuit 
>       (getFirst $ First x1 <> First x2) 
>       (ap (_a . zoom lens) y1 y2) 
>       (getFirst $ First z1 <> First z2)

> instance (Monoid x, Monoid y, Applicative p z) => Applicative (Param_z p) (Circuit x y z) where
>   pure a lens = Circuit Nothing Nothing (pure a @@ _a . zoom lens)
>   ap lens CircuitFail _ = CircuitFail
>   ap lens _ CircuitFail = CircuitFail
>   ap lens (Circuit x1 y1 z1) (Circuit x2 y2 z2) = Circuit 
>       (getFirst $ First x1 <> First x2) 
>       (getFirst $ First y1 <> First y2)
>       (ap (_a . zoom lens) z1 z2) 

We write a nice wrapper so we can parse our circuits:

> parseCircuit
>   :: Circuit (Parser x) (Parser y) (Parser z)
>   -> Text 
>   -> Text
>   -> Text
>   -> Circuit (Result x) (Result y) (Result z)
> parseCircuit CircuitFail _ _ _ = CircuitFail
> parseCircuit (Circuit x y z) str1 str2 str3 = Circuit
>   ( parseMaybe x str1 )
>   ( parseMaybe y str2 )
>   ( parseMaybe z str3 )

And now here is a simple circuit for us:

> circ1 :: Circuit (Parser Text) (Parser Text) (Parser Text)
> circ1 = Circuit
>   (string (pack "haskell") @@ _a._a)
>   (string (pack "is"     ) @@ _a._a)
>   (string (pack "fun"    ) @@ _a._a)

ghci> parseCircuit circ1 "haskell" "is" "fun"
Circuit 
    (Just Done "" "haskell")
    (Just Done "" "is")
    (Just Done "" "fun")

ghci> parseCircuit (circ1 *> circ1 @@ _x._a) "haskell" "is" "fun"
Circuit 
    (Just Partial _) 
    (Just Done "" "is") 
    (Just Done "" "fun")

ghci> parseCircuit (circ1 *> circ1 @@ _x._a) "haskellhaskell" "is" "fun"
Circuit 
    (Just Done "" "haskell")
    (Just Done "" "is")
    (Just Done "" "fun")

> circ2 :: Circuit (Parser Text) (Parser y) (Parser z)
> circ2 = Circuit
>   (string (pack " with lenses") @@ _a._a)
>   Nothing 
>   Nothing

ghci> parseCircuit (circ1 *> circ2 @@ _x._a) "haskell" "is" "fun"
Circuit 
    (Just Partial _) 
    (Just Done "" "is") 
    (Just Done "" "fun")

ghci> parseCircuit (circ1 *> circ2 @@ _x._a) "haskell with lenses" "is" "fun"
Circuit 
    (Just Done "" " with lenses") 
    (Just Done "" "is") 
    (Just Done "" "fun")

ghci> parseCircuit (circ1 *> circ2 @@ _x._a) "haskell without lenses" "is" "fun"
Circuit 
    (Just Fail " without lenses" [] "Failed reading: takeWith") 
    (Just Done "" "is") 
    (Just Done "" "fun")

> circ3 :: Circuit (Parser Text) (Parser y) (Parser z)
> circ3 = pure (string (pack " with lenses") @@ _a) @@ _x

ghci> parseCircuit (circ1 *> circ3 @@ _x._a) "haskell with lenses" "is" "fun"
Circuit 
    (Just Done "" " with lenses") 
    (Just Done "" "is") 
    (Just Done "" "fun")


Next time we'll discover why lensified monads are like burritos infused with 
fiber optic cables.

