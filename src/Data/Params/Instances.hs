{-# LANGUAGE TemplateHaskell #-}

-- | This module exports instances for common Haskell types
module Data.Params.Instances
    where

import Data.Maybe

import Data.Params

-- mkParamClasses ''Either
-- mkGettersSetters ''Either
mkParams ''Either

-- mkTypeLens_Star "a"
-- mkTypeLens_Star "b"
-- mkHasDictionary_Star ''Param_a
-- mkHasDictionary_Star ''Param_b
-- mkViewParam_Star "a" ''Either
-- mkViewParam_Star "b" ''Either
-- mkApplyConstraint_Star "a" ''Either
-- mkApplyConstraint_Star "b" ''Either
