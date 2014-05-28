{-# LANGUAGE TemplateHaskell #-}

-- | Provides a generic interface for working with parameterized vectors.
module Data.Params.Vector
    (
    -- * Classes
    ValidVector (..)
    , Param_len (..)
    , Param_elem (..)
    , Def (..)
    , _len
    , _elem

    -- * Modules
--     , module VG
--     , module VGM
    )
    where

import Language.Haskell.TH hiding (reify)
import Language.Haskell.TH.Syntax hiding (reify)
import qualified Language.Haskell.TH as TH

import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM

import Data.Params

mkParamClass "len" (ConT ''Int)
mkStarParamClass "elem" 
mkReifiableConstraint "len"
mkTypeLens "len"
mkTypeLens "elem"
mkHasDictionary_Star ''Param_elem
mkHasDictionary_Config ''Param_len (ConT ''Int)

-- mkWithParamClass "len" (ConT ''Int)

type ValidVector vec elem = (Param_len (vec elem), VG.Vector vec elem)
