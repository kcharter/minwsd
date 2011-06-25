{-# LANGUAGE FlexibleInstances #-}

module Unparse where

import qualified Data.Text as T

import Tokens

class Unparse a where
  unparse :: a -> String
  
instance Unparse Token where
  unparse = T.unpack . content
  
instance Unparse [Token] where
  unparse = concatMap unparse
