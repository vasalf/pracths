{-# LANGUAGE ScopedTypeVariables #-}
module Reshow where

import Data.Maybe (listToMaybe)

reshowAs :: forall t. (Read t, Show t) => String -> Maybe String
reshowAs = fmap show . fmap fst . listToMaybe . (reads @t)

reshowAs' :: forall t proxy. (Read t, Show t) => proxy t -> String -> Maybe String
reshowAs' _ = fmap show . r
  where
    r :: String -> Maybe t
    r = fmap fst . listToMaybe . reads
