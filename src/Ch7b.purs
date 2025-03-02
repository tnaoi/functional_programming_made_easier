module Ch7b
  ( Person
  )
  where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show (class Show)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, between, discard, show, ($), (<), (<=), (<>), (==), (>), (||))


newtype CSV = CSV String
derive instance newtypeCSV :: Newtype CSV _
derive newtype instance eqCSV :: Eq CSV

class ToCSV a where
  toCSV :: a -> CSV

data Occupation = Doctor | Dentist | Lawyer | Unemployed
derive instance genericOccupation :: Generic Occupation _

data Person = Person
  { name :: FullName
  , age :: Age
  , occupation :: Occupation
  }

newtype FullName = FullName String
instance showFullName :: Show FullName where
  show (FullName name) = name

newtype Age = Age Int
derive instance newtypeAge :: Newtype Age _ 
derive newtype instance showAge :: Show Age

instance showOccupation :: Show Occupation where
  show = genericShow

instance toCSVPerson :: ToCSV Person where
  toCSV (Person {name, age, occupation}) = 
    CSV $ show name <> "," <> show age <> "," <> show occupation


test :: Effect Unit
test = do
  log $ show $ toCSV (Person
{ name: FullName "Sue Smith"
, age: Age 23
, occupation: Doctor
}) == CSV "Sue Smith,23,Doctor" 
