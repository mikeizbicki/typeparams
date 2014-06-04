{-# LANGUAGE TemplateHaskell #-}

-- | This module exports instances for common Haskell types
module Data.Params.Instances
    where

import Data.Maybe

import Data.Params

mkParams ''Maybe
mkParams ''Either
