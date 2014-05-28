{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Params
    ( 

    -- * Basic 
    Param (..)
    , mkParams

    , with1Param
    , apWith1Param

    , mkWith1Param
    , mkApWith1Param

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

    -- * Advanced 
    -- | The code in this section is only for advanced users when the 'mkParams'
    -- function proves insufficient for some reason.
    -- Getting the types to work out by hand can be rather complicated...
    -- if you must use these functions, then you'll probably need some migraine
    -- medication afterward.

    -- ** Template haskell generating code
    , mkParamClasses
    , mkParamClass
    , mkStarParamClass
    , mkTypeLens
    , mkHasDictionary_Star
    , mkHasDictionary_Config
    , mkViewParam_Star
    , mkViewParam_Config
    , mkApplyConstraint_Star
    , mkApplyConstraint_Config

    , mkParamInstance
    , mkReifiableConstraint
    , mkReifiableConstraint'

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
    , intparam 

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
import Language.Haskell.TH hiding (reify)
import Language.Haskell.TH.Syntax hiding (reify)
import qualified Language.Haskell.TH as TH

import GHC.TypeLits
import Data.Params.Frac

import Data.Constraint
import Data.Constraint.Unsafe
import Data.Reflection
import Unsafe.Coerce

import Debug.Trace
import Prelude hiding ((.),id)

-------------------------------------------------------------------------------

-- | Use this function for getting the type parameter value from an 'Int'.
-- It has proper inlining to ensure that the 'fromIntegral' gets computed
-- at compile time.

{-# INLINE [1] intparam #-}
intparam :: forall n. KnownNat n => Proxy (n::Nat) -> Int
intparam _ = fromIntegral $ natVal (Proxy::Proxy n)

return $ 
    [ PragmaD $ RuleP 
        ("intparam "++show i)
        [ TypedRuleVar 
            (mkName "s")
            (AppT (ConT (mkName "Proxy")) (LitT $ NumTyLit i))
        ]
        (AppE 
            (VarE $ mkName "intparam")
            (VarE $ mkName "s")
        )
        (LitE $ IntegerL i)
        AllPhases
    | i <- [0..1000]
    ]

-------------------------------------------------------------------------------
-- types

-- | (Kind) Specifies that the type parameter can be known either statically
-- or dynamically.
data Param a 
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

apUsing' :: forall p a1 a2 b. ReifiableConstraint p => Def p a2 -> (p a2 => a1) -> (p a2 => a1 -> b) -> b
apUsing' def = unsafeCoerce $ apUsing def
    
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

-- apWith2Param ::
--     ( ReifiableConstraint p2
--     , ReifiableConstraint p4
--     ) => ParamDict p1 p2 m1 m2
--       -> ParamDict p3 p4 m1 m3
--       -> ((p2 m2,p4 m3) => m1 -> n)
--       -> ((p2 m2,p4 m3) => m1)
--       -> n
-- apWith2Param p1 p2 = flip $ apUsing2 
--     (unsafeCoerce DummyNewtype (\x -> unsafeCoerce p1)) 
--     (unsafeCoerce DummyNewtype (\x -> unsafeCoerce p2))

-------------------------------------------------------------------------------
-- template haskell
 
-- | Constructs all the needed type classes and instances in order to use
-- typeparams in a simple manner.  Example usage:
--
-- > data NearestNeighbor (k :: Param Nat) (maxdist :: Param Float) elem = ...
-- > mkParams ''NearestNeighbor
--
mkParams :: Name -> Q [Dec]
mkParams dataName = do
    tmp <- TH.reify dataName
    let varL = case tmp of
            TyConI (NewtypeD _ _ xs _ _) -> xs
            TyConI (DataD _ _ xs _ _) -> xs
            FamilyI (FamilyD _ _ xs _) _ -> xs

    let varL' = map mapgo $ filter filtergo varL
        filtergo (KindedTV _ (AppT (ConT maybe) _)) = nameBase maybe=="Param"
        filtergo _ = False
        mapgo (KindedTV name (AppT _ k)) = 
            (nameBase name,k,kind2type k)

    paramClass <- liftM concat $ mapM (\(n,k,t) -> mkParamClass n t) varL' 
    reifiableC <- liftM concat $ mapM (\(n,k,t) -> mkReifiableConstraint' 
            (mkName $ "Param_"++n) 
            [SigD (mkName $ "getParam_"++n) $ AppT (AppT ArrowT (VarT $ mkName "m")) t ])
         varL' 
    paramInsts <- liftM concat $ mapM (\(n,k,t) -> mkParamInstance n t dataName) varL' 

--     withParamClass <- liftM concat $ mapM (\(n,k,t) -> mkWithParamClass n t) varL'
--     withParamInsts <- liftM concat $ mapM (\(n,k,t) -> mkWithParamInstance n t dataName) varL' 

    return $ paramClass++reifiableC++paramInsts -- ++withParamClass++withParamInsts
--     return $ paramClass++reifiableC -- ++paramInsts -- ++withParamClass

---------------------------------------
-- convert kinds into other objects

kind2type :: Type -> Type
kind2type (AppT ListT t) = AppT ListT $ kind2type t
kind2type (ConT n) = ConT $ mkName $ case nameBase n of
    "Nat" -> "Int"
    "Frac" -> "Rational"
    "Symbol" -> "String"
    str -> error $ "mkParams does not currently support custom type "++str
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
    "Frac" -> "fracVal"
    "Symbol" -> "symbolVal"

kind2convert :: Type -> Name
kind2convert (AppT _ t) = kind2convert t
kind2convert (ConT n) = mkName $ case nameBase n of
    "Nat" -> "id"
    "Frac" -> "fromRational"
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

-- | Given a data type of the form
--
-- > data Type (v1 :: Param k1) v2 = ...
--
-- creates the following "Param_" classes that uniquely identify the parameters
--
-- > class Param_v1 t where getParam_v1 :: t -> kind2type k1
-- > class Param_v2 t where getParam_v2 :: t -> ()
--
mkParamClasses :: Name -> Q [Dec]
mkParamClasses dataName = do
    c <- TH.reify dataName
    let tyVarBndrL = case c of
            TyConI (NewtypeD _ _ xs _ _) -> xs
            TyConI (DataD _ _ xs _ _) -> xs
            FamilyI (FamilyD _ _ xs _) _ -> xs

    let (tyVarBndrL_config,tyVarBndrL_notconfig) = partition filtergo tyVarBndrL 
        filtergo (KindedTV _ (AppT (ConT maybe) _)) = nameBase maybe=="Param"
        filtergo _ = False

    liftM concat $ forM tyVarBndrL_config $ \(KindedTV name (AppT _ k)) -> 
        mkParamClass (nameBase name) (kind2type k)

    liftM concat $ forM tyVarBndrL_notconfig $ \ tv ->
        mkStarParamClass (tyVarBndr2str tv)

-- | Creates classes of the form
--
-- > class Param_paramname t where
-- >     getParam_paramname :: t -> paramT
--
-- NOTE: this function should probably not be called directly
mkParamClass :: String -> Type -> Q [Dec]
mkParamClass paramname paramT = do
    isDef <- lookupTypeName $ "Param_"++paramname
    return $ case isDef of
        Just _ -> []
        Nothing -> 
            [ ClassD
                [ ]
                ( mkName $ "Param_"++paramname ) 
                [ PlainTV $ mkName "t" ]
                [ ]
                [ SigD
                    (mkName $ "getParam_"++paramname) 
                    (AppT
                        (AppT
                            ArrowT
                            (VarT $ mkName "t"))
                        paramT)
                ]
            ]

-- | Creates classes of the form
--
-- > class Param_paramname (p :: * -> Constraint) (t :: *) where
--
-- NOTE: this function should probably not be called directly
mkStarParamClass :: String -> Q [Dec]
mkStarParamClass paramname = do
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
isStarParam :: String -> Q Bool
isStarParam paramname = do
    info <- TH.reify $ mkName $ "Param_"++paramname
    return $ case info of
        ClassI (ClassD _ _ xs _ _) _ -> length xs == 2 

-- | Creates a "TypeLens" for the given paramname.
-- If paramname corresponds to a star parameter, then create a "TypeLens" of the form
--
-- > _paramname :: TypeLens p (Param_paramname p)
-- > _paramname = TypeLens
--
-- Else, if paramname correponds to a config parameter, then create a "TypeLens" of the form
--
-- > _paramname :: TypeLens Base Param_paramname
-- > _paramname = TypeLens
--
mkTypeLens :: String -> Q [Dec]
mkTypeLens paramname = do
    isDef <- lookupValueName $ "_"++paramname
    isStar <- isStarParam paramname
    return $ case isDef of
        Just _ -> []
        Nothing -> if isStar
            then 
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
            else
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

-- | Given the class Param_paramname that indexes a star parameter paramname, 
-- create an instance of the form
--
-- > instance 
-- >     ( HasDictionary p
-- >     ) => HasDictionary (Param_paramname p)
-- >         where
-- >     type ParamType (Param_paramname p) = ParamType p
-- >     typeLens2dictConstructor _ = coerceParamDict $ typeLens2dictConstructor (TypeLens::TypeLens Base p)
mkHasDictionary_Star :: Name -> Q [Dec]
mkHasDictionary_Star paramname = return
    [ InstanceD
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
mkHasDictionary_Config :: Name -> Type -> Q [Dec]
mkHasDictionary_Config paramname paramtype = return
    [ InstanceD
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
mkViewParam_Star paramname dataname = do
    info <- TH.reify dataname
    let tyVarBndrL = case info of
            TyConI (NewtypeD _ _ xs _ _) -> xs
            TyConI (DataD _ _ xs _ _ ) -> xs
            FamilyI (FamilyD _ _ xs _) _ -> xs
    return 
        [ InstanceD
            [ ClassP 
                (mkName "ViewParam") 
                [ VarT $ mkName "p"
                , VarT $ mkName paramname
                ]
            ]
            ( AppT 
                ( AppT
                    ( ConT $ mkName "ViewParam" )
                    ( AppT (ConT $ mkName $ "Param_"++paramname) (VarT $ mkName "p") )
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
                            ( VarT $ mkName paramname )
                        )
                    )
                    [ ]
                ] 
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
                        ( VarE $ mkName $ "getParam_"++paramstr)
                        ( SigE
                            ( VarE $ mkName "undefined" )
                            ( applyTyVarBndrL dataname tyVarBndrL )
                        )
                    )
                    [ ]
                ] 
            ]
        ]

-- | creates instances of the form
--
-- > instance (KnownNat paramName) => Param_paramName (Static paramName) where
-- >     param_paramName m = fromIntegral $ natVal (Proxy::Proxy paramName)
--
mkParamInstance :: String -> Type -> Name -> Q [Dec]
mkParamInstance paramStr paramType dataName  = do
    c <- TH.reify dataName
    let tyVarL = case c of
            TyConI (NewtypeD _ _ xs _ _) -> xs
            TyConI (DataD _ _ xs _ _ ) -> xs
            FamilyI (FamilyD _ _ xs _) _ -> xs
            otherwise -> error $ "c = "++show c

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
mkReifiableConstraint paramStr = do
    let name = mkName $ "Param_"++paramStr
    info <- TH.reify name
    let funcL = case info of
            ClassI (ClassD _ _ _ _ xs) _ -> xs
            otherwise -> error "mkReifiableConstraint parameter must be a type class"
    mkReifiableConstraint' name funcL

-- | creates instances of the form
--
-- > instance ReifiableConstraint Def_Param_paramName where
-- >     data Def (Def_Param_paramName) a = Param_paramName {}  
--
mkReifiableConstraint' :: Name -> [Dec] -> Q [Dec] 
mkReifiableConstraint' c funcL = do
--     isDef <- lookupValueName $ {-"Def_"++-}nameBase c
--     isDef <- lookupTypeName $ {-"Def_"++-}nameBase c
--     isDef <- reifyInstances (mkName "ReifiableConstraint") [ConT c]
    isDef <- isInstance (mkName "ReifiableConstraint") [ConT c]
--     return $ case isDef of
--         Just x -> []
--         Nothing -> 
    return $ if isDef
        then []
        else [ InstanceD 
                []
                (AppT (ConT $ mkName "ReifiableConstraint") (ConT c))
--                 [ DataInstD 
                [ NewtypeInstD 
                    []
                    (mkName "Def")
                    [ ConT c, VarT tyVar]
                    ( trace ("xx="++show ("Def_"++nameBase c)) $ RecC 
                        (mkName $ "Def_"++nameBase c) 
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
                            (ConT c))
                        (VarT $ mkName "a")
                    ]
                ]
                (AppT 
                    (ConT c) 
                    (AppT 
                        (AppT 
                            (AppT (ConT $ mkName "ConstraintLift") (ConT c))
                            (VarT tyVar))
                        (VarT $ mkName "s"))
                )
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
                    | SigD fname ftype <- funcL
                ]
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

