{-# LANGUAGE TemplateHaskell #-}

-- | Provides a generic interface for working with parameterized vectors.
module Data.Params.Vector
    (
    -- * Classes
    Param_len (..)
    , Param_elem (..)
    , Def (..)
    , _len
    , _elem

    )
    where

import Language.Haskell.TH hiding (reify)
import Language.Haskell.TH.Syntax hiding (reify)
import qualified Language.Haskell.TH as TH

import Data.Params

mkParamClass_Config "len" (ConT ''Int)
mkParamClass_Star "elem" 
mkReifiableConstraint "len"
mkTypeLens_Config "len"
mkTypeLens_Star "elem"
mkHasDictionary_Star "elem"
mkHasDictionary_Config "len" (ConT ''Int)

