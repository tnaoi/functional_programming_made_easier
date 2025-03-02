module Main where

import Prelude

import Ch9 as Ch9
import Effect (Effect)

main :: Effect Unit
main = do
  Ch9.verifyOrBoolSemigroup
  Ch9.verifyOrBoolMonoid
  Ch9.verifyMod4Semigroup
  Ch9.verifyMod4Group
  Ch9.verifyMaybeSemigroup
