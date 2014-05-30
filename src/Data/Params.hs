{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Params
    ( 

    -- * Basic 
    Config (..)
    , mkParams

    , with1Param
    , apWith1Param

    , mkWith1Param
    , mkApWith1Param

    , mkApWith2Param
    , apWith2Param
    , mkApWith3Param
    , apWith3Param

    -- ** Classes
    , HasDictionary (..)
    , ViewParam (..)
    , ParamDict (..)

    , ApplyConstraint
    , coerceParamDict
    , TypeLens (..)
    , GetParam
    , SetParam
    , Base
    , ApplyConstraint_GetType
    , ApplyConstraint_GetConstraint

    , mkRuleFrac
    , intparam 
    , floatparam
    , Float (..)

    -- * Advanced 
    -- | The code in this section is only for advanced users when the 'mkParams'
    -- function proves insufficient for some reason.
    -- Getting the types to work out by hand can be rather complicated...
    -- if you must use these functions, then you'll probably need some migraine
    -- medication afterward.

    -- ** Template haskell generating code
    , mkParamClass_Star
    , mkParamClass_Config
    , mkTypeLens_Star
    , mkTypeLens_Config
    , mkHasDictionary_Star
    , mkHasDictionary_Config
    , mkViewParam_Star
    , mkViewParam_Config
    , mkApplyConstraint_Star
    , mkApplyConstraint_Config
    , Param_Dummy

    , mkParamInstance
    , mkReifiableConstraint
--     , mkReifiableConstraint'

    , mkGettersSetters

    -- ** General parameter classes
    -- | These classes were shamelessly stollen from <https://www.fpcomplete.com/user/thoughtpolice/using-reflection this excellent reflection tutorial>.
    -- If you want to understand how this library works, that's the place to start.
    , ReifiableConstraint(..)
--     , WithParam (..)
    , ConstraintLift (..)

    -- ** Helper functions
    , using
    , using'
    , apUsing
    , apUsing2
    , apUsing'

    -- * Modules
    , module GHC.TypeLits
    , module Data.Params.Frac

    , module Data.Reflection
    , module Data.Proxy
    , module Data.Constraint
    , module Data.Constraint.Unsafe
    )
    where

import Control.Category
import Control.Monad
import Data.Proxy
import Data.List (partition)
import Data.Monoid
import Data.Ratio
import Language.Haskell.TH hiding (reify)
import Language.Haskell.TH.Syntax hiding (reify)
import qualified Language.Haskell.TH as TH

import GHC.Float
import GHC.TypeLits
import Data.Params.Frac

import Data.Constraint
import Data.Constraint.Unsafe
import Data.Reflection
import Unsafe.Coerce
import GHC.Base (Int(..))

import Debug.Trace
import Prelude hiding ((.),id)

-------------------------------------------------------------------------------

-- | Use this function for getting the type parameter value from an 'Int'.
-- It has proper inlining to ensure that the 'fromIntegral' gets computed
-- at compile time.

{-# INLINE intparam #-}
intparam :: forall n. KnownNat n => Proxy (n::Nat) -> Int
intparam _ = fromIntegral $ natVal (Proxy::Proxy n)
-- return $ 
--     [ PragmaD $ RuleP 
--         ("intparam "++show i)
--         [ ]
--         ( AppE 
--             ( VarE $ mkName "intparam" )
--             ( SigE
--                 ( ConE $ mkName "Proxy" )
--                 ( AppT
--                     ( ConT $ mkName "Proxy" )
--                     ( LitT ( NumTyLit i ) )
--                 )
--             )
--         )
--         ( AppE ( ConE $ mkName "I#" ) (LitE $ IntPrimL i ) )
--         AllPhases
--     | i <- [0..10000]
--     ]


{-# NOINLINE floatparam #-}
floatparam :: forall n. KnownFrac n => Proxy (n::Frac) -> Float
floatparam _ = fromRational $ fracVal (Proxy::Proxy n)

mkRuleFrac :: Rational -> Q [Dec]
mkRuleFrac r = do
    let n=numerator r
        d=denominator r
    return $
        [ PragmaD $ RuleP
            ( "floatparam "++show r )
            [ ]
            ( AppE
                ( VarE $ mkName "floatparam" )
                ( SigE
                    ( ConE $ mkName "Proxy" )
                    ( AppT 
                        ( ConT $ mkName "Proxy" )
                        ( AppT 
                            ( AppT 
                                ( ConT $ mkName "/" ) 
                                ( LitT $ NumTyLit n ) 
                            )
                            ( LitT $ NumTyLit d)
                        )
                    )
                )
            ) 
            ( AppE
                ( ConE $ mkName "F#" )
                ( LitE $ FloatPrimL r )
            )
            AllPhases
        ]

-- "floatparam 1"      floatparam (Proxy::Proxy (1/1)) = 1 :: Float

-------------------------------------------------------------------------------
-- types

-- | (Kind) Specifies that the type parameter can be known either statically
-- or dynamically.
data Config a 
    = Static a -- ^ The parameter is statically set to 'a'
    | RunTime  -- ^ The parameter is determined at run time using the 'withParam' functions
    | Automatic -- ^ The parameter is determined at run time and the value is inferred automatically without user specification

---------------------------------------

---------------------------------------
-- shamelessly stolen functions for internal use only

newtype ConstraintLift (p :: * -> Constraint) (a :: *) (s :: *) = ConstraintLift { lower :: a }

class ReifiableConstraint p where
    data Def (p :: * -> Constraint) (a:: *) :: *
    reifiedIns :: Reifies s (Def p a) :- p (ConstraintLift p a s)

asProxyOf :: f s -> Proxy s -> f s
asProxyOf v _ = v

using :: forall p a. ReifiableConstraint p => Def p a -> (p a => a) -> a
using d m = reify d $ \(_ :: Proxy s) ->
    let replaceProof :: Reifies s (Def p a) :- p a
        replaceProof = trans proof reifiedIns
            where proof = unsafeCoerceConstraint :: p (ConstraintLift p a s) :- p a
    in m \\ replaceProof

using' :: forall p a b. ReifiableConstraint p => Def p b -> (p b => a) -> a
using' def = unsafeCoerce (using def)

apUsing :: forall p a b. ReifiableConstraint p => Def p a -> (p a => a) -> (p a => a -> b) -> b
apUsing d m f = reify d $ \(_ :: Proxy s) ->
    let replaceProof :: Reifies s (Def p a) :- p a
        replaceProof = trans proof reifiedIns
            where proof = unsafeCoerceConstraint :: p (ConstraintLift p a s) :- p a
    in (f m) \\ replaceProof 

apUsing' :: forall p a1 a2 b. ReifiableConstraint p => Def p a2 -> (p a2 => a1) -> (p a2 => a1 -> b) -> b
apUsing' def = unsafeCoerce $ apUsing def
    
apUsing2 :: forall p1 p2 a a1 a2 b. 
    ( ReifiableConstraint p1
    , ReifiableConstraint p2
    ) => Def p1 a1 
      -> Def p2 a2
      -> ((p1 a1,p2 a2) => a) 
      -> ((p1 a1,p2 a2) => a -> b) 
      -> b
apUsing2 d1 d2 m f = reify d2 $ \(_ :: Proxy s2) -> reify d1 $ \(_ :: Proxy s1) ->
    let replaceProof :: Reifies s1 (Def p1 a1) :- p1 a1
        replaceProof = trans proof reifiedIns
            where proof = unsafeCoerceConstraint :: p1 (ConstraintLift p1 a1 s1) :- p1 a1
        replaceProof2 :: Reifies s2 (Def p2 a2) :- p2 a2
        replaceProof2 = trans proof reifiedIns
            where proof = unsafeCoerceConstraint :: p2 (ConstraintLift p2 a2 s2) :- p2 a2
    in (f m) \\ replaceProof \\ replaceProof2

apUsing3 :: forall p1 p2 p3 a a1 a2 a3 b. 
    ( ReifiableConstraint p1
    , ReifiableConstraint p2
    , ReifiableConstraint p3
    ) => Def p1 a1 
      -> Def p2 a2
      -> Def p3 a3
      -> ((p1 a1,p2 a2,p3 a3) => a) 
      -> ((p1 a1,p2 a2,p3 a3) => a -> b) 
      -> b
apUsing3 d1 d2 d3 m f = reify d3 $ \(_ :: Proxy s3) -> 
                        reify d2 $ \(_ :: Proxy s2) -> 
                        reify d1 $ \(_ :: Proxy s1) ->
    let replaceProof :: Reifies s1 (Def p1 a1) :- p1 a1
        replaceProof = trans proof reifiedIns
            where proof = unsafeCoerceConstraint :: p1 (ConstraintLift p1 a1 s1) :- p1 a1
        replaceProof2 :: Reifies s2 (Def p2 a2) :- p2 a2
        replaceProof2 = trans proof reifiedIns
            where proof = unsafeCoerceConstraint :: p2 (ConstraintLift p2 a2 s2) :- p2 a2
        replaceProof3 :: Reifies s3 (Def p3 a3) :- p3 a3
        replaceProof3 = trans proof reifiedIns
            where proof = unsafeCoerceConstraint :: p3 (ConstraintLift p3 a3 s3) :- p3 a3
    in (f m) \\ replaceProof \\ replaceProof2 \\ replaceProof3

apUsing4 :: forall p1 p2 p3 p4 a a1 a2 a3 a4 b. 
    ( ReifiableConstraint p1
    , ReifiableConstraint p2
    , ReifiableConstraint p3
    , ReifiableConstraint p4
    ) => Def p1 a1 
      -> Def p2 a2
      -> Def p3 a3
      -> Def p4 a4
      -> ((p1 a1,p2 a2,p3 a3,p4 a4) => a) 
      -> ((p1 a1,p2 a2,p3 a3,p4 a4) => a -> b) 
      -> b
apUsing4 d1 d2 d3 d4 m f = reify d4 $ \(_ :: Proxy s4) ->
                        reify d3 $ \(_ :: Proxy s3) -> 
                        reify d2 $ \(_ :: Proxy s2) -> 
                        reify d1 $ \(_ :: Proxy s1) ->
    let replaceProof :: Reifies s1 (Def p1 a1) :- p1 a1
        replaceProof = trans proof reifiedIns
            where proof = unsafeCoerceConstraint :: p1 (ConstraintLift p1 a1 s1) :- p1 a1
        replaceProof2 :: Reifies s2 (Def p2 a2) :- p2 a2
        replaceProof2 = trans proof reifiedIns
            where proof = unsafeCoerceConstraint :: p2 (ConstraintLift p2 a2 s2) :- p2 a2
        replaceProof3 :: Reifies s3 (Def p3 a3) :- p3 a3
        replaceProof3 = trans proof reifiedIns
            where proof = unsafeCoerceConstraint :: p3 (ConstraintLift p3 a3 s3) :- p3 a3
        replaceProof4 :: Reifies s4 (Def p4 a4) :- p4 a4
        replaceProof4 = trans proof reifiedIns
            where proof = unsafeCoerceConstraint :: p4 (ConstraintLift p4 a4 s4) :- p4 a4
    in (f m) \\ replaceProof \\ replaceProof2 \\ replaceProof3 \\ replaceProof4

apUsing5 :: forall p1 p2 p3 p4 p5 a a1 a2 a3 a4 a5 b. 
    ( ReifiableConstraint p1
    , ReifiableConstraint p2
    , ReifiableConstraint p3
    , ReifiableConstraint p4
    , ReifiableConstraint p5
    ) => Def p1 a1 
      -> Def p2 a2
      -> Def p3 a3
      -> Def p4 a4
      -> Def p5 a5
      -> ((p1 a1,p2 a2,p3 a3,p4 a4,p5 a5) => a) 
      -> ((p1 a1,p2 a2,p3 a3,p4 a4,p5 a5) => a -> b) 
      -> b
apUsing5 d1 d2 d3 d4 d5 m f = reify d5 $ \(_ :: Proxy s5) ->
                        reify d4 $ \(_ :: Proxy s4) ->
                        reify d3 $ \(_ :: Proxy s3) -> 
                        reify d2 $ \(_ :: Proxy s2) -> 
                        reify d1 $ \(_ :: Proxy s1) ->
    let replaceProof :: Reifies s1 (Def p1 a1) :- p1 a1
        replaceProof = trans proof reifiedIns
            where proof = unsafeCoerceConstraint :: p1 (ConstraintLift p1 a1 s1) :- p1 a1
        replaceProof2 :: Reifies s2 (Def p2 a2) :- p2 a2
        replaceProof2 = trans proof reifiedIns
            where proof = unsafeCoerceConstraint :: p2 (ConstraintLift p2 a2 s2) :- p2 a2
        replaceProof3 :: Reifies s3 (Def p3 a3) :- p3 a3
        replaceProof3 = trans proof reifiedIns
            where proof = unsafeCoerceConstraint :: p3 (ConstraintLift p3 a3 s3) :- p3 a3
        replaceProof4 :: Reifies s4 (Def p4 a4) :- p4 a4
        replaceProof4 = trans proof reifiedIns
            where proof = unsafeCoerceConstraint :: p4 (ConstraintLift p4 a4 s4) :- p4 a4
        replaceProof5 :: Reifies s5 (Def p5 a5) :- p5 a5
        replaceProof5 = trans proof reifiedIns
            where proof = unsafeCoerceConstraint :: p5 (ConstraintLift p5 a5 s5) :- p5 a5
    in (f m) \\ replaceProof \\ replaceProof2 \\ replaceProof3 \\ replaceProof4 \\ replaceProof5
-------------------
-- for external use

data TypeLens (a:: * -> Constraint) (b:: * -> Constraint) = TypeLens

instance Category TypeLens where
    id = TypeLens
    a.b = TypeLens
    
class Base a 

-- data family ParamDict (p::k)

class HasDictionary p where
    type ParamType p :: *
    data ParamDict p
    typeLens2dictConstructor :: TypeLens base p -> (ParamType p -> ParamDict p)

class ViewParam p t where
    viewParam :: TypeLens Base p -> t -> ParamType p

coerceParamDict :: (ParamType p -> ParamDict p) -> (ParamType p -> ParamDict (a p))
coerceParamDict = unsafeCoerce

type ApplyConstraint p m = (ApplyConstraint_GetConstraint p) (ApplyConstraint_GetType p m)
type family ApplyConstraint_GetConstraint (p::k) :: * -> Constraint 
type family ApplyConstraint_GetType (p::k) t :: * 

type family GetParam (p::k1) (t::k2) :: k3
type family SetParam (p::k1) (a::k2) (t::k3) :: k3

newtype DummyNewtype a = DummyNewtype a

mkWith1Param :: proxy m -> (
    ( ReifiableConstraint (ApplyConstraint_GetConstraint p)
    , HasDictionary p
    ) => TypeLens Base p
      -> ParamType p
      -> (ApplyConstraint p m => m)
      -> m
      )
mkWith1Param _ = with1Param

with1Param :: forall p m.
    ( ReifiableConstraint (ApplyConstraint_GetConstraint p)
    , HasDictionary p
    ) => TypeLens Base p
      -> ParamType p
      -> (ApplyConstraint p m => m) 
      -> m
with1Param lens v = using' (unsafeCoerce DummyNewtype (\x -> p) :: Def (ApplyConstraint_GetConstraint p) (ApplyConstraint_GetType p m)) 
    where
        p = typeLens2dictConstructor lens v :: ParamDict p 

mkApWith1Param :: proxy m -> proxy n -> (
    ( ReifiableConstraint (ApplyConstraint_GetConstraint p)
    , HasDictionary p
    )  => TypeLens Base p
       -> ParamType p
       -> (ApplyConstraint p m => m -> n)
       -> (ApplyConstraint p m => m)
       -> n
       )
mkApWith1Param _ _ = apWith1Param

apWith1Param :: forall p m n.
    ( ReifiableConstraint (ApplyConstraint_GetConstraint p)
    , HasDictionary p
    ) => TypeLens Base p
      -> ParamType p
      -> (ApplyConstraint p m => m -> n) 
      -> (ApplyConstraint p m => m) 
      -> n
apWith1Param lens v = flip $ apUsing' 
    (unsafeCoerce DummyNewtype (\x -> p) :: Def (ApplyConstraint_GetConstraint p) (ApplyConstraint_GetType p m))
    where
        p = typeLens2dictConstructor lens v :: ParamDict p 

mkApWith2Param :: proxy m -> proxy n -> (
    ( ReifiableConstraint (ApplyConstraint_GetConstraint p1)
    , ReifiableConstraint (ApplyConstraint_GetConstraint p2)
    , HasDictionary p1
    , HasDictionary p2
    ) => TypeLens Base p1
      -> ParamType p1
      -> TypeLens Base p2
      -> ParamType p2
      -> ((ApplyConstraint p1 m, ApplyConstraint p2 m) => m -> n)
      -> ((ApplyConstraint p1 m, ApplyConstraint p2 m) => m)
      -> n
      )
mkApWith2Param _ _ = apWith2Param

apWith2Param :: forall p1 p2 m n.
    ( ReifiableConstraint (ApplyConstraint_GetConstraint p1)
    , ReifiableConstraint (ApplyConstraint_GetConstraint p2)
    , HasDictionary p1
    , HasDictionary p2
    ) => TypeLens Base p1
      -> ParamType p1
      -> TypeLens Base p2
      -> ParamType p2
      -> ((ApplyConstraint p1 m, ApplyConstraint p2 m) => m -> n)
      -> ((ApplyConstraint p1 m, ApplyConstraint p2 m) => m)
      -> n
apWith2Param lens1 v1 lens2 v2 = flip $ apUsing2
    (unsafeCoerce DummyNewtype (\x -> unsafeCoerce p1)) 
    (unsafeCoerce DummyNewtype (\x -> unsafeCoerce p2))
    where
        p1 = typeLens2dictConstructor lens1 v1 :: ParamDict p1
        p2 = typeLens2dictConstructor lens2 v2 :: ParamDict p2

mkApWith3Param :: proxy m -> proxy n -> (
    ( ReifiableConstraint (ApplyConstraint_GetConstraint p1)
    , ReifiableConstraint (ApplyConstraint_GetConstraint p2)
    , ReifiableConstraint (ApplyConstraint_GetConstraint p3)
    , HasDictionary p1
    , HasDictionary p2
    , HasDictionary p3
    ) => TypeLens Base p1
      -> ParamType p1
      -> TypeLens Base p2
      -> ParamType p2
      -> TypeLens Base p3
      -> ParamType p3
      -> ((ApplyConstraint p1 m, ApplyConstraint p2 m, ApplyConstraint p3 m) => m -> n)
      -> ((ApplyConstraint p1 m, ApplyConstraint p2 m, ApplyConstraint p3 m) => m)
      -> n
      )
mkApWith3Param _ _ = apWith3Param

apWith3Param :: forall p1 p2 p3 m n.
    ( ReifiableConstraint (ApplyConstraint_GetConstraint p1)
    , ReifiableConstraint (ApplyConstraint_GetConstraint p2)
    , ReifiableConstraint (ApplyConstraint_GetConstraint p3)
    , HasDictionary p1
    , HasDictionary p2
    , HasDictionary p3
    ) => TypeLens Base p1
      -> ParamType p1
      -> TypeLens Base p2
      -> ParamType p2
      -> TypeLens Base p3
      -> ParamType p3
      -> ((ApplyConstraint p1 m, ApplyConstraint p2 m, ApplyConstraint p3 m) => m -> n)
      -> ((ApplyConstraint p1 m, ApplyConstraint p2 m, ApplyConstraint p3 m) => m)
      -> n
apWith3Param lens1 v1 lens2 v2 lens3 v3 = flip $ apUsing3
    (unsafeCoerce DummyNewtype (\x -> unsafeCoerce p1)) 
    (unsafeCoerce DummyNewtype (\x -> unsafeCoerce p2))
    (unsafeCoerce DummyNewtype (\x -> unsafeCoerce p3))
    where
        p1 = typeLens2dictConstructor lens1 v1 :: ParamDict p1
        p2 = typeLens2dictConstructor lens2 v2 :: ParamDict p2
        p3 = typeLens2dictConstructor lens3 v3 :: ParamDict p3

-------------------------------------------------------------------------------
-- template haskell
 
-- | Constructs all the needed type classes and instances in order to use
-- typeparams in a simple manner.  Example usage:
--
-- > data NearestNeighbor (k :: Param Nat) (maxdist :: Param Float) elem = ...
-- > mkParams ''NearestNeighbor
--

mkParams :: Name -> Q [Dec]
mkParams dataname = do
    info <- TH.reify dataname
    let tyVarBndrL = case info of
            TyConI (NewtypeD _ _ xs _ _) -> xs
            TyConI (DataD _ _ xs _ _) -> xs
            FamilyI (FamilyD _ _ xs _) _ -> xs

    let (tyVarBndrL_Config,tyVarBndrL_Star) = partition filtergo tyVarBndrL 
        filtergo (KindedTV _ (AppT (ConT maybe) _)) = nameBase maybe=="Config"
        filtergo _ = False
    
    getterssetters <- mkGettersSetters dataname

    configparams <- forM tyVarBndrL_Config $ \tyVarBndr -> do
        let paramstr = tyVarBndr2str tyVarBndr
        let ( KindedTV _ k ) = tyVarBndr
        sequence
            [ mkParamClass_Config paramstr (kind2type k)
            , mkReifiableConstraint' paramstr [ paramClass_getParam paramstr (kind2type k) ]
            , mkTypeLens_Config paramstr
            , mkViewParam_Config paramstr dataname
            , mkApplyConstraint_Config paramstr dataname
            , mkHasDictionary_Config paramstr (kind2type k)
            , mkParamInstance paramstr (kind2type k) dataname
            ]

    starparams <- forM tyVarBndrL_Star $ \tyVarBndr -> do
        let paramstr = tyVarBndr2str tyVarBndr
        sequence
            [ mkTypeLens_Star paramstr
            , mkViewParam_Star paramstr dataname
            , mkApplyConstraint_Star paramstr dataname
            , mkHasDictionary_Star paramstr
            , mkParamClass_Star paramstr
            ]

    trace ("tyVarBndrL_Config="++show tyVarBndrL_Config++"\ntyVarBndrL_Star="++show tyVarBndrL_Star) $ 
        return $ getterssetters 
        ++ (concat $ concat $ configparams) 
        ++ (concat $ concat $ starparams)

---------------------------------------
-- convert kinds into other objects

kind2type :: Type -> Type
kind2type (AppT ListT t) = AppT ListT $ kind2type t
kind2type (AppT (ConT c) t) = if nameBase c=="Config"
    then kind2type t
    else error "kind2type nameBase c"
kind2type (ConT n) = ConT $ mkName $ case nameBase n of
    "Nat" -> "Int"
    "Frac" -> "Float"
--     "Frac" -> "Rational"
    "Symbol" -> "String"
    str -> error $ "mkParams does not currently support custom type "++str
kind2type x = error $ "kind2type on x="++show x
--     kind -> kind

kind2constraint :: Type -> Name
kind2constraint (AppT _ t) = kind2constraint t
kind2constraint (ConT n) = mkName $ case nameBase n of
    "Nat" -> "KnownNat"
    "Frac" -> "KnownFrac"
    "Symbol" -> "KnownSymbol"

kind2val :: Type -> Name
kind2val (AppT _ t) = kind2val t
kind2val (ConT n) = mkName $ case nameBase n of
    "Nat" -> "intparam"
    "Frac" -> "floatparam"
--     "Frac" -> "fracVal"
    "Symbol" -> "symbolVal"

kind2convert :: Type -> Name
kind2convert (AppT _ t) = kind2convert t
kind2convert (ConT n) = mkName $ case nameBase n of
    "Nat" -> "id"
    "Frac" -> "id"
--     "Frac" -> "fromRational"
    "Symbol" -> "id"
    _ -> "id"

param2class :: Name -> Name
param2class p = mkName $ "Param_" ++ nameBase p

param2func :: Name -> Name
param2func p = mkName $ "getParam_" ++ nameBase p

---------------------------------------
-- helper TH functions

tyVarBndr2str :: TyVarBndr -> String
tyVarBndr2str (PlainTV n) = nameBase n
tyVarBndr2str (KindedTV n _) = nameBase n

applyTyVarBndrL :: Name -> [ TyVarBndr ] -> Type
applyTyVarBndrL name xs = go xs (ConT name)
    where
        go [] t = t
        go (x:xs) t = go xs (AppT t (VarT $ mkName $ tyVarBndr2str x))

-------------------------------------------------------------------------------
-- template haskell

-- | Given a data type of the form
--
-- > data Type v1 v2 ... vk = ...
--
-- creates "GetParam" instances of the form
--
-- > type instance GetParam Param_v1 (Type v1 v2 ... vk) = v1
-- > type instance GetParam Param_v2 (Type v1 v2 ... vk) = v2
-- > ...
-- > type instnce GetParam Param_vk (Type v1 v2 ... vk) = vk
--
-- and "SetParam" instances of the form
--
-- > type instance SetParam Param_v1 newparam (Type v1 v2 ... vk) = Type newparam v2 ... vk
-- > type instance SetParam Param_v2 newparam (Type v1 v2 ... vk) = Type v1 newparam ... vk
-- > ...
-- > type instance SetParam Param_vk newparam (Type v1 v2 ... vk) = Type v1 v2 ... newparam
--
-- This function requires that the Param_vk classes have already been defined.
--
mkGettersSetters :: Name -> Q [Dec]
mkGettersSetters dataName = do
    c <- TH.reify dataName
    let tyVarBndrL = case c of
            TyConI (NewtypeD _ _ xs _ _) -> xs
            TyConI (DataD _ _ xs _ _ ) -> xs
            FamilyI (FamilyD _ _ xs _) _ -> xs
            otherwise -> error $ "Cannot mkGettersSetters on "++nameBase dataName++"; reify = "++show c

    let getters = 
            [ TySynInstD
                ( mkName "GetParam" )
                ( TySynEqn
                    [ ConT $ mkName $ "Param_" ++ tyVarBndr2str x
                    , applyTyVarBndrL dataName tyVarBndrL
                    ]
                    ( VarT $ mkName $ tyVarBndr2str x
                    )
                )
            | x <- tyVarBndrL
            ]

    let setters = 
            [ TySynInstD
                ( mkName "SetParam" )
                ( TySynEqn
                    [ ConT $ mkName $ "Param_" ++ tyVarBndr2str x
                    , VarT $ mkName $ "newparam"
                    , applyTyVarBndrL dataName tyVarBndrL
                    ]
                    ( applyTyVarBndrL dataName $ map 
                        (\a -> if tyVarBndr2str a==tyVarBndr2str x
                            then PlainTV $ mkName "newparam"
                            else a
                        ) 
                        tyVarBndrL 
                    )
                )
            | x <- tyVarBndrL
            ]

    return $ getters++setters

-- | Creates classes of the form
--
-- > class Param_paramname t where
-- >     getParam_paramname :: t -> paramT
-- >     {-# INLINE getParam_paramname #-}
--
-- NOTE: this function should probably not be called directly
mkParamClass_Config :: String -> Type -> Q [Dec]
mkParamClass_Config paramstr paramT = do
    isDef <- lookupTypeName $ "Param_"++paramstr
    return $ case isDef of
        Just _ -> []
        Nothing -> 
            [ ClassD
                [ ]
                ( mkName $ "Param_"++paramstr ) 
                [ PlainTV $ mkName "t" ]
                [ ]
                [ paramClass_getParam paramstr paramT ] 
            ]

paramClass_getParam :: String -> Type -> Dec
paramClass_getParam paramstr paramT
    = SigD
        (mkName $ "getParam_"++paramstr) 
        (AppT
            (AppT
                ArrowT
                (VarT $ mkName "t"))
            paramT)

-- | Creates classes of the form
--
-- > class Param_paramname (p :: * -> Constraint) (t :: *) where
--
-- NOTE: this function should probably not be called directly
mkParamClass_Star :: String -> Q [Dec]
mkParamClass_Star paramname = do
    isDef <- lookupTypeName $ "Param_"++paramname
    return $ case isDef of
        Just _ -> []
        Nothing -> 
            [ ClassD
                [ ]
                ( mkName $ "Param_"++paramname )
                [ KindedTV (mkName "p") (AppT (AppT ArrowT StarT) ConstraintT)
                , KindedTV (mkName "t") StarT
                ] 
                [ ]
                [ ]
            ]

-- | returns True if the parameter has kind *, False otherwise
-- isStarParam :: String -> Q Bool
-- isStarParam paramname = do
--     info <- TH.reify $ mkName $ "Param_"++paramname
--     return $ case info of
--         ClassI (ClassD _ _ xs _ _) _ -> length xs == 2 

-- | Creates a "TypeLens" for the given star paramname of the form
--
-- > _paramname :: TypeLens p (Param_paramname p)
-- > _paramname = TypeLens
--
mkTypeLens_Star :: String -> Q [Dec]
mkTypeLens_Star paramname = do
    isDef <- lookupValueName $ "_"++paramname
    return $ case isDef of
        Just _ -> []
        Nothing -> 
            [ ValD
                ( SigP
                    ( VarP $ mkName $ "_"++paramname )
                    ( ForallT 
                        [ PlainTV $ mkName "p" ]
                        [ ]
                        ( AppT 
                            ( AppT 
                                ( ConT $ mkName "TypeLens" ) 
                                ( VarT $ mkName "p" )
                            )
                            ( AppT
                                ( ConT $ mkName $ "Param_" ++ paramname )
                                ( VarT $ mkName "p" )
                            )
                        )
                    )
                )
                ( NormalB
                    ( VarE $ mkName $ "undefined" )
                )
                [ ]
            ]

-- | Creates a "TypeLens" for the given config paramname of the form
--
-- > _paramname :: TypeLens Base Param_paramname
-- > _paramname = TypeLens
--
mkTypeLens_Config :: String -> Q [Dec]
mkTypeLens_Config paramname = do
    isDef <- lookupValueName $ "_"++paramname
    return $ case isDef of
        Just _ -> []
        Nothing -> 
            [ ValD
                ( SigP
                    ( VarP $ mkName $ "_"++paramname )
                    ( ForallT 
                        [ ]
                        [ ]
                        ( AppT 
                            ( AppT 
                                ( ConT $ mkName "TypeLens" ) 
                                ( ConT $ mkName "Base" )
                            )
                            ( ConT $ mkName $ "Param_" ++ paramname )
                        )
                    )
                )
                ( NormalB
                    ( VarE $ mkName $ "undefined" )
                )
                [ ]
            ]

-- | This class is needed because I can't get type variables to work in "reifyInstances"
class Param_Dummy t

-- | Given the class Param_paramname that indexes a star parameter paramname, 
-- create an instance of the form
--
-- > instance 
-- >     ( HasDictionary p
-- >     ) => HasDictionary (Param_paramname p)
-- >         where
-- >     type ParamType (Param_paramname p) = ParamType p
-- >     newtype ParamDict (Param_paramname p) = ParamDict_paramname 
-- >        { unParamDict_paramname :: ParamType (Param_paramname) }
-- >     typeLens2dictConstructor _ = coerceParamDict $ typeLens2dictConstructor (TypeLens::TypeLens Base p)
-- >     {#- INLINE typeLens2dictConstructor #-}
--
mkHasDictionary_Star :: String -> Q [Dec]
mkHasDictionary_Star paramstr = do
    let paramname = mkName $ "Param_"++paramstr

    alreadyInstance <- do
        isDef <- lookupTypeName (nameBase paramname)
        case isDef of
            Nothing -> return False
            Just _ -> isInstance
                ( mkName "HasDictionary" ) 
                [ (AppT (ConT paramname) (ConT $ mkName "Param_Dummy")) ]

    return $ if alreadyInstance
        then [ ]
        else [ InstanceD
            [ ClassP (mkName "HasDictionary") [VarT $ mkName "p"] ]
            ( AppT 
                ( ConT $ mkName "HasDictionary" )
                ( AppT (ConT paramname) (VarT $ mkName "p") )
            )
            [ TySynInstD
                ( mkName "ParamType" )
                ( TySynEqn
                    [ AppT (ConT paramname) (VarT $ mkName "p") ]
                    ( AppT (ConT $ mkName "ParamType") (VarT $ mkName "p") )
                )
            , NewtypeInstD
                [ ]
                ( mkName "ParamDict" )
                [ AppT (ConT paramname) (VarT $ mkName "p") ]
                ( RecC
                    ( mkName $ "ParamDict_"++nameBase paramname )
                    [ ( mkName ("unParamDict_"++nameBase paramname)
                      , NotStrict
                      , AppT (ConT $ mkName "ParamType") (VarT $ mkName "p")
                      ) 
                    ]
                )
                [ ]
            , FunD
                ( mkName "typeLens2dictConstructor" )
                [ Clause
                    [ VarP $ mkName "x" ]
                    ( NormalB $ AppE
                        ( VarE $ mkName "coerceParamDict" )
                        ( AppE
                            ( VarE $ mkName "typeLens2dictConstructor" )
                            ( SigE
                                ( ConE $ mkName "TypeLens" ) 
                                ( AppT
                                    ( AppT
                                        ( ConT $ mkName "TypeLens" )
                                        ( ConT $ mkName "Base" )
                                    )
                                    ( VarT $ mkName "p" )
                                )
                            )
                        )
                    )
                    [ ]
                ]
            , PragmaD $ InlineP
                ( mkName "typeLens2dictConstructor" )
                Inline
                FunLike
                AllPhases
            ]
        ]

-- | Given the class Param_paramname that indexes a config parameter paramname
-- create an instance of the form
--
-- > instance HasDictionary Param_paramname where
-- >    type ParamType Param_paramname = paramtype
-- >    newtype ParamDict Param_len = ParamDict_paramname { getParamDict_paramname :: paramtype } 
-- >    typeLens2dictConstructor _ = ParamDict_paramname
--
mkHasDictionary_Config :: String -> Type -> Q [Dec]
mkHasDictionary_Config paramstr paramtype = do
    let paramname = mkName $ "Param_"++paramstr

    alreadyInstance <- do
        isDef <- lookupTypeName (nameBase paramname)
        case isDef of
            Nothing -> return False
            Just _ -> isInstance 
                ( mkName "HasDictionary" ) 
                [ ConT paramname ]

    return $ if alreadyInstance
        then [ ]
        else [ InstanceD
            [ ]
            ( AppT 
                ( ConT $ mkName "HasDictionary" )
                ( ConT paramname )
            )
            [ TySynInstD
                ( mkName "ParamType" )
                ( TySynEqn
                    [ ConT paramname ]
                    ( paramtype )
                )
            , NewtypeInstD
                [ ]
                ( mkName "ParamDict" )
                [ ConT paramname ]
                ( RecC
                    ( mkName $ "ParamDict_"++nameBase paramname )
                    [ ( mkName ("unParamDict_"++nameBase paramname)
                      , NotStrict
                      , paramtype
                      ) 
                    ]
                )
                [ ]
            , FunD
                ( mkName "typeLens2dictConstructor" )
                [ Clause
                    [ VarP $ mkName "x" ]
                    ( NormalB $ ConE $ mkName $ "ParamDict_"++nameBase paramname )
                    [ ]
                ]
            , PragmaD $ InlineP
                ( mkName $ "typeLens2dictConstructor" )
                Inline
                FunLike
                AllPhases
            ]
        ]


-- | Given star parameter paramname and data type dataname that has parameter paramname,
-- create type instances of the form
--
-- > type instance ApplyConstraint_GetConstraint (Param_paramname p) 
-- >    = ApplyConstraint_GetConstraint p 
-- >
-- > type instance ApplyConstraint_GetType (Param_paramname p) (dataname v1 v2 ... paramname ... vk) 
-- >    = ApplyConstraint_GetType p paramname
-- 
mkApplyConstraint_Star :: String -> Name -> Q [Dec]
mkApplyConstraint_Star paramstr dataname = do
    let paramname = mkName $ "Param_"++paramstr
    info <- TH.reify dataname
    let tyVarBndrL = case info of
            TyConI (NewtypeD _ _ xs _ _) -> xs
            TyConI (DataD _ _ xs _ _ ) -> xs
            FamilyI (FamilyD _ _ xs _) _ -> xs

    return 
        [ TySynInstD
            ( mkName "ApplyConstraint_GetConstraint" )
            ( TySynEqn
                [ (AppT (ConT paramname) (VarT $ mkName "p")) ]
                ( AppT (ConT $ mkName "ApplyConstraint_GetConstraint" ) (VarT $ mkName "p") )
            )
        , TySynInstD
            ( mkName "ApplyConstraint_GetType" )
            ( TySynEqn
                [ (AppT (ConT paramname) (VarT $ mkName "p"))
                , applyTyVarBndrL dataname tyVarBndrL
                ]
                ( AppT
                    ( AppT
                        ( ConT $ mkName "ApplyConstraint_GetType" )
                        ( VarT $ mkName "p" )
                    )
                    ( VarT $ mkName paramstr )
                )
            )        
        ]

-- | Given star parameter paramname and data type dataname that has parameter paramname,
-- create type instances of the form
--
-- > type instance ApplyConstraint_GetConstraint Param_paramname
-- >    = ApplyConstraint_GetConstraint Param_paramname
-- >
-- > type instance ApplyConstraint_GetType Param_paramname (dataname v1 v2 ... paramname ... vk) 
-- >    = ApplyConstraint_GetType Param_paramname (dataname v1 v2 ... paramname ... vk)
-- 
mkApplyConstraint_Config :: String -> Name -> Q [Dec]
mkApplyConstraint_Config paramstr dataname = do
    let paramname = mkName $ "Param_"++paramstr
    info <- TH.reify dataname
    let tyVarBndrL = case info of
            TyConI (NewtypeD _ _ xs _ _) -> xs
            TyConI (DataD _ _ xs _ _ ) -> xs
            FamilyI (FamilyD _ _ xs _) _ -> xs

    return 
        [ TySynInstD
            ( mkName "ApplyConstraint_GetConstraint" )
            ( TySynEqn
                [ ConT paramname ]
                ( ConT paramname )
            )
        , TySynInstD
            ( mkName "ApplyConstraint_GetType" )
            ( TySynEqn
                [ ConT paramname 
                , applyTyVarBndrL dataname tyVarBndrL
                ]
                ( applyTyVarBndrL dataname tyVarBndrL )
            )        
        ]

-- | Given star parameter paramname and data type dataname that has parameter paramname,
-- create an instance of the form
--
-- > instance 
-- >     ( ViewParam p paramname 
-- >     ) => ViewParam (Param_paramname p) (dataname v1 v2 ... paramname ... vk)
-- >         where
-- >     viewParam _ _ = viewParam (undefined::TypeLens Base p) (undefined :: paramname)
--
mkViewParam_Star :: String -> Name -> Q [Dec]
mkViewParam_Star paramstr dataname = do
    let paramname = mkName $ "Param_"++paramstr

    info <- TH.reify dataname
    let tyVarBndrL = case info of
            TyConI (NewtypeD _ _ xs _ _) -> xs
            TyConI (DataD _ _ xs _ _ ) -> xs
            FamilyI (FamilyD _ _ xs _) _ -> xs

    return $
        [ InstanceD
            [ ClassP 
                (mkName "ViewParam") 
                [ VarT $ mkName "p"
                , VarT $ mkName paramstr
                ]
            ]
            ( AppT 
                ( AppT
                    ( ConT $ mkName "ViewParam" )
                    ( AppT (ConT paramname) (VarT $ mkName "p") )
                )
                ( applyTyVarBndrL dataname tyVarBndrL )
            )
            [ FunD
                ( mkName "viewParam" )
                [ Clause
                    [ VarP $ mkName "x", VarP $ mkName "y" ]
                    ( NormalB $ AppE
                        ( AppE
                            ( VarE $ mkName "viewParam" )
                            ( SigE 
                                ( VarE $ mkName "undefined" )
                                ( AppT
                                    ( AppT 
                                        ( ConT $ mkName "TypeLens" ) 
                                        ( ConT $ mkName "Base") 
                                    ) 
                                    ( VarT $ mkName "p" )
                                )
                            )
                        )
                        ( SigE
                            ( VarE $ mkName "undefined" )
                            ( VarT $ mkName paramstr )
                        )
                    )
                    [ ]
                ] 
            , PragmaD $ InlineP
                ( mkName $ "viewParam" )
                Inline
                FunLike
                AllPhases
            ]
        ]

-- | Given star parameter paramname and data type dataname that has parameter paramname,
-- create an instance of the form
--
-- > instance
-- >     ( Param_paramname (dataname v1 v2 ... paramname ... vk)
-- >     ) => ViewParam Param_paramname (dataname v1 v2 ... paramname ... vk) where
-- >     viewParam _ _ = getParam_paramname (undefined::dataname v1 v2 ... paramname ... vk)
--
mkViewParam_Config :: String -> Name -> Q [Dec]
mkViewParam_Config paramstr dataname = do
    info <- TH.reify dataname
    let tyVarBndrL = case info of
            TyConI (NewtypeD _ _ xs _ _) -> xs
            TyConI (DataD _ _ xs _ _ ) -> xs
            FamilyI (FamilyD _ _ xs _) _ -> xs
    return 
        [ InstanceD
            [ ClassP 
                ( mkName $ "Param_"++paramstr) 
                [ applyTyVarBndrL dataname tyVarBndrL ]
            ]
            ( AppT 
                ( AppT
                    ( ConT $ mkName "ViewParam" )
                    (ConT $ mkName $ "Param_"++paramstr) 
                )
                ( applyTyVarBndrL dataname tyVarBndrL )
            )
            [ FunD
                ( mkName "viewParam" )
                [ Clause
                    [ VarP $ mkName "x", VarP $ mkName "y" ]
                    ( NormalB $ AppE
                        ( VarE $ kind2convert $ AppT (ConT $ mkName "ParamType") (ConT $ mkName $ "Param_"++paramstr) )
                        ( AppE
                            ( VarE $ mkName $ "getParam_"++paramstr)
                            ( SigE
                                ( VarE $ mkName "undefined" )
                                ( applyTyVarBndrL dataname tyVarBndrL )
                            )
                        )
                    )
                    [ ]
                ] 
            , PragmaD $ InlineP
                ( mkName $ "viewParam" )
                Inline
                FunLike
                AllPhases
            ]
        ]

-- | creates instances of the form
--
-- > instance (KnownNat paramName) => Param_paramName (Static paramName) where
-- >     param_paramName m = fromIntegral $ natVal (Proxy::Proxy paramName)
--
mkParamInstance :: String -> Type -> Name -> Q [Dec]
mkParamInstance paramStr paramType dataName  = do
    info <- TH.reify dataName
    let tyVarL = case info of
            TyConI (NewtypeD _ _ xs _ _) -> xs
            TyConI (DataD _ _ xs _ _ ) -> xs
            FamilyI (FamilyD _ _ xs _) _ -> xs

    let tyVarL' = filter filtergo tyVarL
        filtergo (KindedTV n k) = nameBase n==paramStr
        filtergo (PlainTV n) = nameBase n == paramStr

    let [KindedTV paramName paramKind] = tyVarL'

    return
        [ InstanceD
            [ ClassP
                ( kind2constraint paramKind )
                [ VarT paramName ]
            ]
            (AppT 
                (ConT $ param2class paramName)
                (tyVarL2Type tyVarL (AppT (PromotedT $ mkName "Static") (VarT paramName))))
            [ FunD
                ( mkName $ "getParam_"++nameBase paramName )
                [ Clause
                    [ VarP $ mkName "m" ]
                    (NormalB $
                        (AppE
                            (VarE $ kind2convert paramKind)
                            (AppE
                                (VarE $ kind2val paramKind)
                                (SigE
                                    (ConE $ mkName "Proxy")
                                    (AppT
                                        (ConT $ mkName "Proxy")
                                        (VarT paramName)
                                    )
                                )
                            )
                        )
                    )
                    []
                ]
            , PragmaD $ InlineP
                ( mkName $ "getParam_"++nameBase paramName )
                Inline
                FunLike
                AllPhases
            ]
        ]
    where
        tyVarL2Type xs matchType = go $ reverse xs
            where
                go [] = ConT $ mkName $ nameBase dataName
                go ((PlainTV n):xs) = AppT (go xs) (VarT n)
                go ((KindedTV n k):xs) = AppT (go xs) $ if nameBase n==paramStr
                    then matchType 
                    else (VarT n)



-- | helper for 'mkReifiableConstraints''
mkReifiableConstraint :: String -> Q [Dec]
mkReifiableConstraint paramstr = do
    let name = mkName $ "Param_"++paramstr
    info <- TH.reify name
    let funcL = case info of
            ClassI (ClassD _ _ _ _ xs) _ -> xs
            otherwise -> error "mkReifiableConstraint parameter must be a type class"
    mkReifiableConstraint' paramstr funcL

-- | creates instances of the form
--
-- > instance ReifiableConstraint Def_Param_paramName where
-- >     data Def (Def_Param_paramName) a = Param_paramName {}  
--
mkReifiableConstraint' :: String -> [Dec] -> Q [Dec] 
mkReifiableConstraint' paramstr funcL = do
    let paramname = mkName $ "Param_"++paramstr
--     isDef <- isInstance (mkName "ReifiableConstraint") [ConT paramname]
    alreadyInstance <- do
        isDef <- lookupTypeName (nameBase paramname)
        case isDef of
            Nothing -> return False
            Just _ -> isInstance 
                ( mkName "ReifiableConstraint" ) 
                [ ConT paramname ]

    return $ if alreadyInstance
        then [ ]
        else [ InstanceD 
                []
                (AppT (ConT $ mkName "ReifiableConstraint") (ConT paramname))
                [ NewtypeInstD 
                    []
                    (mkName "Def")
                    [ ConT paramname, VarT tyVar]
                    ( RecC 
                        (mkName $ "Def_"++nameBase paramname) 
                        [ (mkName $ nameBase fname ++ "_", NotStrict, insertTyVar (tyVar) ftype) 
                            | SigD fname ftype <- funcL
                        ]
                    )
                    []
                , ValD 
                    (VarP $ mkName "reifiedIns") 
                    (NormalB $ 
                        (AppE 
                            (ConE $ mkName "Sub")
                            (ConE $ mkName "Dict"))
                    ) 
                    []
                ]
            , InstanceD
                [ ClassP 
                    ( mkName "Reifies" )
                    [ VarT $ mkName "s"
                    , AppT
                        (AppT
                            (ConT $ mkName "Def")
                            (ConT paramname))
                        (VarT $ mkName "a")
                    ]
                ]
                (AppT 
                    (ConT paramname) 
                    (AppT 
                        (AppT 
                            (AppT (ConT $ mkName "ConstraintLift") (ConT paramname))
                            (VarT tyVar))
                        (VarT $ mkName "s"))
                )
                ( concat [  
                    [ FunD 
                        fname 
                        [ Clause
                            [ VarP $ mkName "a" ]
                            (NormalB $
                                AppE
                                    (AppE
                                        (VarE $ mkName $ nameBase fname++"_")
                                        (AppE 
                                            (VarE (mkName "reflect"))
                                            (VarE (mkName "a"))))
                                    (AppE
                                        (VarE $ mkName "lower")
                                        (VarE $ mkName "a"))
                            )
                            [] 
                        ]
                    , PragmaD $ InlineP
                        fname 
                        Inline
                        FunLike
                        AllPhases
                    ]
                | SigD fname ftype <- funcL
                ] )
            ]
    where

        tyVar = mkName "a"

        insertTyVar :: Name -> Type -> Type
        insertTyVar name (ForallT xs cxt t) = ForallT [] [] (insertTyVar name t)
        insertTyVar name (AppT t1 t2) = AppT (insertTyVar name t1) (insertTyVar name t2)
        insertTyVar name (VarT _) = VarT name
        insertTyVar name ArrowT = ArrowT
        insertTyVar name a = a

-------------------------------------------------------------------------------
-- test

