{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module allows relatively simple type level parameters.
-- Unlike most Haskell libraries, the type signatures here are not much help.
-- It is easier to see how to use the library with an example.
--
-- The classic example for using type parameters is to create a list whose size is 
-- represented in the type.  
-- For example:
-- 
-- > newtype StaticList (len::Param Nat) a = StaticList [a]
-- >     deriving (Read,Show,Eq,Ord)
-- >
-- > mkParams ''StaticList
-- 
-- The 'mkParams' function creates a number of helper classes and instances.
-- It inspects the type parameters whose kind is a 'Param' something, and ignores
-- the rest.
-- In this case, we create a type class called Param_len.
-- Our StaticList will be an instance of this type class whenever the len parameter
-- is valid.
-- More details on this in the next section.
-- For now, we'll see how to use the length information by creating a Monoid 
-- instance.
-- Notice that the param_len function is used to extract the type level length.
-- 
-- > instance 
-- >   ( Monoid a
-- >   , Param_len (StaticList len a)
-- >   ) => Monoid (StaticList len a) 
-- >       where
-- >
-- >       mempty = StaticList $ replicate n mempty
-- >           where 
-- >               n = param_len (undefined::StaticList len a)
-- >
-- >       mappend (StaticList a) (StaticList b) = StaticList $ zipWith mappend a b
-- 
-- So that's how we use the type level information.
-- Now we need to see how to set the information.
-- In the following simple main function, we create two variables.
-- The length of the static variable is determined at compile time, whereas the length
-- of the dynamic variable is read from stdin.
-- 
-- > main = do
-- >       putStr "Enter a size for the dynamic list: "
-- >       size <- readLn
-- >       let static  =                     mempty :: StaticList (Static 5) (Maybe [Int])
-- >       let dynamic = setParam (len size) mempty :: StaticList RunTime    (Maybe [Int])
-- >       putStrLn $ "static  = " ++ show static
-- >       putStrLn $ "dynamic = " ++ show dynamic
-- 
-- This example can be found in more detail <https://github.com/mikeizbicki/typeparams/blob/master/examples/example1-StaticList.lhs on github>.
-- There are many more examples of advanced features in the repository.

module Data.Params
    ( 

    -- * Basic 
    Param (..)
    , mkParams

    , withParam
    , withParam2
    , withParam3

    -- * Advanced 
    -- | The code in this section is only for advanced users when the 'mkParams'
    -- function proves insufficient for some reason.
    -- Getting the types to work out by hand can be rather complicated...
    -- if you must use these functions, then you'll probably need some migraine
    -- medication afterward.

    -- ** Template haskell generating code
    , mkParamClass
    , mkParamInstance
    , mkSetParamClass
    , mkSetParamInstance
    , mkReifiableConstraint
    , mkReifiableConstraint'

    -- ** General parameter classes
    -- | These classes were shamelessly stollen from <https://www.fpcomplete.com/user/thoughtpolice/using-reflection this excellent reflection tutorial>.
    -- If you want to understand how this library works, that's the place to start.
    , ReifiableConstraint(..)
    , SetParam (..)
    , ConstraintLift (..)

    -- ** Helper functions
    , using
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

import Control.Monad
import Language.Haskell.TH hiding (reify)
import Language.Haskell.TH.Syntax hiding (reify)
import qualified Language.Haskell.TH as TH

import GHC.TypeLits
import Data.Params.Frac

import Data.Constraint
import Data.Constraint.Unsafe
import Data.Reflection
import Data.Proxy

import Debug.Trace

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

newtype ConstraintLift (p :: * -> Constraint) (a :: *) (s :: *) = ConstraintLift { lower :: a }

class ReifiableConstraint p where
    data Def (p :: * -> Constraint) (a:: *) :: *
    reifiedIns :: Reifies s (Def p a) :- p (ConstraintLift p a s)

class SetParam p m where
    data DefParam p m :: *
    setParam :: DefParam p m -> (p m => m) -> m

---------------------------------------
-- shamelessly stolen functions for internal use only

asProxyOf :: f s -> Proxy s -> f s
asProxyOf v _ = v

using' :: forall p a b. ReifiableConstraint p => Def p a -> (p a => a) -> (a -> b) -> b
using' d m f = reify d $ \(_ :: Proxy s) ->
    let replaceProof :: Reifies s (Def p a) :- p a
        replaceProof = trans proof reifiedIns
            where proof = unsafeCoerceConstraint :: p (ConstraintLift p a s) :- p a
    in (f m) \\ replaceProof

using :: forall p a. ReifiableConstraint p => Def p a -> (p a => a) -> a
using d m = reify d $ \(_ :: Proxy s) ->
    let replaceProof :: Reifies s (Def p a) :- p a
        replaceProof = trans proof reifiedIns
            where proof = unsafeCoerceConstraint :: p (ConstraintLift p a s) :- p a
    in m \\ replaceProof
    
using2 :: (ReifiableConstraint p1, ReifiableConstraint p2) => 
    (Def p1 a, Def p2 a) -> ((p1 a, p2 a) => a) -> a
using2 (p1,p2) f = using p1 $ using p2 $ f

-------------------
-- for external use

-- | dynamically specifies a single 'RunTime' parameter
withParam :: SetParam p m => DefParam p m -> (p m => m) -> m
withParam = setParam

-- | dynamically specifies two 'RunTime' parameters
withParam2 :: (SetParam p1 m, SetParam p2 m) => 
    DefParam p1 m -> DefParam p2 m -> ((p1 m, p2 m) => m) -> m
withParam2 p1 p2 f = setParam p1 $ setParam p2 $ f

-- | dynamically specifies three 'RunTime' parameters
withParam3 :: (SetParam p1 m, SetParam p2 m, SetParam p3 m) =>
    DefParam p1 m -> DefParam p2 m -> DefParam p3 m -> ((p1 m, p2 m, p3 m) => m) -> m
withParam3 p1 p2 p3 f = setParam p1 $ setParam p2 $ setParam p3 $ f

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


    paramClass <- liftM concat $ mapM (\(n,k,t) -> mkParamClass n $ return t) varL' 
    reifiableC <- liftM concat $ mapM (\(n,k,t) -> mkReifiableConstraint' 
            (mkName $ "Param_"++ n) 
            [SigD (mkName $ "param_"++n) $ AppT (AppT ArrowT (VarT $ mkName "m")) t ])
         varL' 
    paramInsts <- liftM concat $ mapM (\(n,k,t) -> mkParamInstance n t dataName) varL' 

    setParamClass <- liftM concat $ mapM (\(n,k,t) -> mkSetParamClass n $ return t) varL'
    setParamInsts <- liftM concat $ mapM (\(n,k,t) -> mkSetParamInstance n t dataName) varL' 

    return $ paramClass++reifiableC++paramInsts++setParamClass++setParamInsts

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
param2func p = mkName $ "param_" ++ nameBase p

---------------------------------------
-- helper TH functions

-- | creates instances of the form
--
-- > instance (KnownNat paramName) => Param_paramName (Static paramName) where
-- >     param_paramName m = fromIntegral $ natVal (Proxy::Proxy paramName)
-- >
-- > instance SetParam Param_paramName (dataName) where
-- >     data DefParam Param_paramName (dataName ...) = SetParam_dataName_paramName (...)
-- >     setParam = (...)
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
--                 (trace ("paramType="++show paramKind) $ mkName "KnownNat")
                ( kind2constraint paramKind )
                [ VarT paramName ]
            ]
            (AppT 
                (ConT $ param2class paramName)
                (tyVarL2Type tyVarL (AppT (PromotedT $ mkName "Static") (VarT paramName))))
            [ FunD
                ( mkName $ "param_"++nameBase paramName )
                [ Clause
                    [ VarP $ mkName "m" ]
                    (NormalB $
                        (AppE
                            (VarE $ kind2convert paramKind)
--                             (VarE $ mkName "fromIntegral")
                            (AppE
--                                 (VarE $ mkName "natVal")
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
        , InstanceD
            []
            (AppT 
                (AppT
                    (ConT $ mkName "SetParam")
                    (ConT (param2class paramName))
                )
                (tyVarL2Type tyVarL (PromotedT $ mkName "RunTime"))
            )
            [ DataInstD
                []
                (mkName $ "DefParam")
                [ ConT $ param2class paramName, tyVarL2Type tyVarL (PromotedT $ mkName "RunTime") ]
                [ RecC 
                    (mkName $ "SetParam_"++nameBase dataName++"_"++nameBase paramName)
                    [(mkName $ "unSetParam_"++nameBase dataName++"_"++nameBase paramName,NotStrict,paramType)]
                ]
                []
            , FunD
                (mkName $ "setParam")
                [ Clause
                    [VarP $ mkName "p", VarP $ mkName "a"]
                    (NormalB $
                        AppE
                            (AppE
                                (VarE $ mkName "using")
                                (AppE
                                    (ConE $ mkName $ "Def_Param_"++nameBase paramName)
                                    (LamE
                                        [VarP $ mkName"x"]
                                        (AppE
                                            (VarE $ mkName $ "unSetParam_"++nameBase dataName++"_"++nameBase paramName)
                                            (VarE $ mkName "p")
                                        )
                                    )
                                )
                            )
                            (VarE $ mkName "a")
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

-- | Creates classes of the form
--
-- > class Param_paramname m where
-- >     param_paramname :: m -> paramT
--
mkParamClass :: String -> Q Type -> Q [Dec]
mkParamClass paramname qparamT = do
    paramT <- qparamT
    isDef <- lookupTypeName $ "Param_"++paramname
    return $ case isDef of
        Just _ -> []
        Nothing -> 
            [ ClassD
                []
                (mkName $ "Param_"++paramname) 
                [PlainTV $ mkName "m"]
                []
                [ SigD
                    (mkName $ "param_"++paramname) 
                    (AppT
                        (AppT
                            ArrowT
                            (VarT $ mkName "m"))
                        paramT)
                ]
            ]

-- | Creates classes of the form:
--
-- > class SetParam_paramname m where
-- >     paramname :: paramT -> DefParam Param_paramname m
--
mkSetParamClass :: String -> Q Type -> Q [Dec]
mkSetParamClass paramname qparamT = do
    paramT <- qparamT
    isDef <- lookupTypeName $ "SetParam_"++paramname
    return $ case isDef of
        Just _ -> []
        Nothing ->
            [ ClassD
                []
                (mkName $ "SetParam_"++paramname)
                [PlainTV $ mkName "m"]
                []
                [SigD
                    (mkName $ paramname)
                    (AppT
                        (AppT
                            ArrowT
                            paramT
                        )
                        (AppT
                            (AppT
                                (ConT $ mkName "DefParam")
                                (ConT $ mkName $ "Param_"++paramname)
                            )
                            (VarT $ mkName "m")
                        )
                    )
                ]
            ]

-- | Creates instances of the form:
--
-- > instance SetParam_paramname dataname where
-- >     paramname = SetParam_dataName_paramname
--
mkSetParamInstance :: String -> Type -> Name -> Q [Dec]
mkSetParamInstance paramStr paramType dataName  = do
    c <- TH.reify dataName
    let tyVarL = case c of
            TyConI (NewtypeD _ _ xs _ _) -> xs
            TyConI (DataD _ _ xs _ _ ) -> xs
            FamilyI (FamilyD _ _ xs _) _ -> xs
            otherwise -> error $ "mkSetParamInstance patern match failed; c = "++show c

    let tyVarL' = filter filtergo tyVarL
        filtergo (KindedTV n k) = nameBase n==paramStr
        filtergo (PlainTV n) = nameBase n == paramStr

    let [KindedTV paramName _] = tyVarL'

    return
        [ InstanceD
            [ ]
            (AppT 
                (ConT $ mkName $ "SetParam_"++nameBase paramName)
                (tyVarL2Type tyVarL (PromotedT $ mkName "RunTime") )
            )
            [ FunD
                ( mkName $ nameBase paramName )
                [ Clause
                    [ ]
                    (NormalB $ (ConE $ mkName $ "SetParam_"++nameBase dataName++"_"++nameBase paramName))
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
mkReifiableConstraint :: Name -> Q [Dec]
mkReifiableConstraint c = do
    info <- TH.reify c
    let funcL = case info of
            ClassI (ClassD _ _ _ _ xs) _ -> xs
            otherwise -> error "mkReifiableConstraint parameter must be a type class"
    mkReifiableConstraint' c funcL

-- | creates instances of the form
--
-- > instance ReifiableConstraint Def_Param_paramName where
-- >     data Def (Def_Param_paramName) a = Param_paramName {}  
--
mkReifiableConstraint' :: Name -> [Dec] -> Q [Dec] 
mkReifiableConstraint' c funcL = do 
    isDef <- lookupTypeName $ nameBase c
    return $ case isDef of
        Just _ -> []
        Nothing -> 
            [ InstanceD 
                []
                (AppT (ConT $ mkName "ReifiableConstraint") (ConT c))
                [ DataInstD 
                    []
                    (mkName "Def")
                    [ ConT c, VarT tyVar]
                    [ RecC 
                        (mkName $ "Def_"++nameBase c) 
                        [ (mkName $ nameBase fname ++ "_", NotStrict, insertTyVar (tyVar) ftype) 
                            | SigD fname ftype <- funcL
                        ]
                    ]
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
{-

data ReflectionTest1 (a::Maybe Nat) = ReflectionTest1 Int 
    deriving (Read,Show,Eq,Ord)

instance (ParamA (ReflectionTest1 a)) => Monoid (ReflectionTest1 a) where
    mempty = ReflectionTest1 a 
        where
            a = paramA (undefined::ReflectionTest1 a)
    mappend a b = a


data ReflectionTest (a::Maybe Nat) (b::Maybe Nat) = ReflectionTest Int Int Int
    deriving (Read,Show,Eq,Ord)

instance (ParamA (ReflectionTest a b), ParamB (ReflectionTest a b)) => Monoid (ReflectionTest a b) where
    mempty = ReflectionTest a b $ a+b
        where
            a = paramA (undefined::ReflectionTest a b)
            b = paramB (undefined::ReflectionTest a b)
    mappend a b = a

---------------------------------------

class ParamA p where paramA :: p -> Int

instance ReifiableConstraint ParamA where
    data Def ParamA a = ParamA { paramA_ :: Int }
    reifiedIns = Sub Dict

instance Reifies s (Def ParamA a) => ParamA (ConstraintLift ParamA a s) where
    paramA a = paramA_ (reflect a)

class ParamB p where paramB :: p -> Int

instance ReifiableConstraint ParamB where
    data Def ParamB a = ParamB { paramB_ :: Int }
    reifiedIns = Sub Dict

instance Reifies s (Def ParamB a) => ParamB (ConstraintLift ParamB a s) where
    paramB a = paramB_ (reflect a)

instance KnownNat a => ParamA (ReflectionTest1 (Just a)) where
    paramA _ = fromIntegral $ natVal (Proxy :: Proxy a)

instance KnownNat a => ParamA (ReflectionTest (Just a) b) where
    paramA _ = fromIntegral $ natVal (Proxy :: Proxy a)

instance KnownNat b => ParamB (ReflectionTest a (Just b)) where
    paramB _ = fromIntegral $ natVal (Proxy :: Proxy b)

class SetParamA m where
    a :: Int -> DefParam ParamA m

instance SetParamA (ReflectionTest1 Nothing) where
    a = DefParam_ParamA1 . ParamA

instance SetParamA (ReflectionTest Nothing b) where
    a = DefParam_ParamA . ParamA

a1 = DefParam_ParamA1 . ParamA
instance SetParam ParamA (ReflectionTest1 Nothing) where
    data DefParam ParamA (ReflectionTest1 Nothing) = 
            DefParam_ParamA1 { unDefParam1 :: Def ParamA (ReflectionTest1 Nothing) }
    setParam p a = using (unDefParam1 p) a

a2 = DefParam_ParamA . ParamA
instance SetParam ParamA (ReflectionTest Nothing b) where
    data DefParam ParamA (ReflectionTest Nothing b) = 
            DefParam_ParamA { unDefParam :: Def ParamA (ReflectionTest Nothing b) }
    setParam p a = using (unDefParam p) a

b = DefParam_ParamB . ParamB
instance SetParam ParamB (ReflectionTest a Nothing) where
    data DefParam ParamB (ReflectionTest a Nothing) = 
            DefParam_ParamB { unDefParamB :: Def ParamB (ReflectionTest a Nothing ) }
    setParam p a = using (unDefParamB p) a

-------------------------------------------------------------------------------
-- simple instances

instance ReifiableConstraint Eq where
    data Def Eq a = Eq { eq_ :: a -> a -> Bool }
    reifiedIns = Sub Dict

instance Reifies s (Def Eq a) => Eq (ConstraintLift Eq a s) where
    a == b = eq_ (reflect a) (lower a) (lower b)

instance ReifiableConstraint Ord where
    data Def Ord a = Ord { compare_ :: a -> a -> Ordering }
    reifiedIns = Sub Dict

instance Reifies s (Def Ord a) => Eq (ConstraintLift Ord a s) where
    a == b = isEq $ compare_ (reflect a) (lower a) (lower b)
        where
            isEq EQ = True
            isEq _ = False

instance Reifies s (Def Ord a) => Ord (ConstraintLift Ord a s) where
    compare a b = compare_ (reflect a) (lower a) (lower b)

instance ReifiableConstraint Monoid where
    data Def Monoid a = Monoid { mappend_ :: a -> a -> a, mempty_ :: a }
    reifiedIns = Sub Dict

instance Reifies s (Def Monoid a) => Monoid (ConstraintLift Monoid a s) where
    mappend a b = ConstraintLift $ mappend_ (reflect a) (lower a) (lower b) 
    mempty = a where a = ConstraintLift $ mempty_ (reflect a)
-}
