module Ch5
  where

import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, (+), (==), show, discard)
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

snoc :: ∀ a. List a -> a -> List a
snoc Nil x = singleton x
snoc (y : ys) x = y : snoc ys x

-- -- Not tail recursive.
-- length :: ∀ a. List a -> Int
-- length Nil = 1
-- length (_ : ys) = length ys

-- Tail recursive version of length using private go function.
length :: ∀ a. List a -> Int
length l = go 0 l where
  go :: Int -> List a -> Int
  go acc Nil      = acc
  go acc (_ : xs) = go (1 + acc) xs

head :: ∀ a. List a -> Maybe a
head Nil = Nothing
head (x : _) = Just x

tail :: ∀ a. List a -> Maybe (List a)
tail Nil      = Nothing
tail (_ : xs) = Just xs

last :: ∀ a. List a -> Maybe a
last Nil    = Nothing
last (_ : xs) = if length xs == 1 then head xs else last xs

init :: ∀ a. List a -> Maybe (List a)
init Nil  = Nothing
init l    = Just $ go l where
  go Nil       = Nil
  go (_ : Nil) = Nil
  go (x : xs)  = x : go xs

uncons :: ∀ a. List a -> Maybe { head :: a, tail :: List a}
uncons Nil = Nothing
uncons (x : xs) = Just { head: x, tail : xs}

infixr 0 apply as $
infixl 1 applyFlipped as #


test :: Effect Unit
test = do
  -- log $ show $ flip const 1 2
  -- flip const 1 2 # show # log
  -- log $ show $ singleton "xyz"
  -- log $ show $ null Nil
  -- log $ show $ null ("abc" : Nil)
  -- log $ show $ snoc (1 : 2 : Nil) 3
  -- log $ show $ length $ 1 : 2 : 3 : Nil
  -- log $ show $ head (Nil :: List Unit)
  -- log $ show $ head ("abc" : "123" : Nil)
  -- log $ show $ tail (Nil :: List Unit)
  -- log $ show $ tail ("abc" : "123" : Nil)

  log $ show $ (last Nil :: Maybe Unit)
  log $ show $ last ("a" : "b" : "c" : Nil)

  log $ show $ init (Nil :: List Unit)
  log $ show $ init (1 : Nil)
  log $ show $ init (1 : 2 : Nil)
  log $ show $ init (1 : 2 : 3 : Nil)

  log $ show $ uncons (1 : 2 : 3 : Nil)