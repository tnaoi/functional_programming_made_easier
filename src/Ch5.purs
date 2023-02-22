module Ch5
  ( (#)
  , apply
  , applyFlipped
  , const
  , flip
  , null
  , singleton
  , test
  )
  where

import Data.List (List(..), (:))
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, show, discard)
import Prim.RowList (Nil)

-- flip :: ∀ a b c. (a -> b -> c) -> b -> a -> c
-- flip f x y = f y x

-- -- Alternative version of flip with lambda.
-- flip :: ∀ a b c. (a -> b -> c) -> (b -> a -> c)
-- flip f = \x y -> f y x

flip :: ∀ a b c. (a -> b -> c) -> b -> (a -> c)
flip f x = \y -> f y x

const :: ∀ a b. a -> b -> a
const x _ = x 

apply :: ∀ a b. (a -> b) -> a -> b
apply f x = f x

applyFlipped :: ∀ a b. a -> (a -> b) -> b
applyFlipped = flip apply

singleton :: ∀ a. a -> List a
singleton x = x : Nil

null :: ∀ a. List a -> Boolean
null Nil = true
null _   = false

infixr 0 apply as $
infixl 1 applyFlipped as #

test :: Effect Unit
test = do
  -- log $ show $ flip const 1 2
  -- flip const 1 2 # show # log
  log $ show $ singleton "xyz"
  log $ show $ null Nil
  log $ show $ null ("abc" : Nil)
