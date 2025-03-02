module Ch9 where

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)
import Prelude (class Eq, class Show, Unit, discard, show, ($), (&&), (==))

data AndBool = AFalse | ATrue

derive instance eqAndBool :: Eq AndBool
derive instance genericAndBool :: Generic AndBool _

class Semigroup a where
  append :: a -> a -> a

class Semigroup a <= Monoid a where
  mempty :: a

infixr 5 append as <>

instance showAndBool :: Show AndBool where
  show = genericShow

instance semigroupAndBool :: Semigroup AndBool where
  append ATrue ATrue = ATrue
  append _ _ = AFalse

instance monoidAndBool :: Monoid AndBool where
  mempty = ATrue

--------------
-- OrBool
--------------
data OrBool = OFalse | OTrue

derive instance eqOrBool :: Eq OrBool
derive instance genericOrBool :: Generic OrBool _

instance showOrBool :: Show OrBool where
  show = genericShow

instance semigroupOrBool :: Semigroup OrBool where
  append OFalse OFalse = OFalse
  append _ _ = OTrue

instance monoidOrBool :: Monoid OrBool where
  mempty = OFalse

---------
-- Mod 4
---------
data Mod4 = Zero | One | Two | Three

derive instance eqMod4 :: Eq Mod4
derive instance genericMod :: Generic Mod4 _

instance showMod4 :: Show Mod4 where
  show = genericShow

instance semigroupMod4 :: Semigroup Mod4 where
  append Zero x = x
  append x Zero = x

  append One One = Two
  append One Two = Three
  append One Three = Zero

  append Two One = Three
  append Two Two = Zero
  append Two Three = One

  append Three One = Zero
  append Three Two = One
  append Three Three = Two

instance monoidMod4 :: Monoid Mod4 where
  mempty = Zero

----------------
-- Group
----------------
class Monoid a <= Group a where
  ginverse :: a -> a

instance groupMod4 :: Group Mod4 where
  ginverse Zero = Zero
  ginverse One = Three
  ginverse Two = One
  ginverse Three = One

------------------
-- Tests
------------------

-- test :: Effect Unit
-- test = do
--   log $ show $ ATrue <> ATrue
--   log $ show $ ATrue <> AFalse
--   log $ show $ AFalse <> AFalse
--   log $ show $ mempty <> ATrue == ATrue
--   log $ show $ mempty <> AFalse == ATrue

-- verifyAndBoolSemigroup :: Effect Unit
-- verifyAndBoolSemigroup = do
--   log "Verifying AndBool Semigroup Laws (1 test)"
--   log $ show $ AFalse <> (ATrue <> ATrue) == AFalse <> (ATrue <> ATrue)

-- verifyAndBoolMonoid :: Effect Unit
-- verifyAndBoolMonoid = do
--   log "Verifying AndBool Monoid Laws (2 tests)"
--   log $ show $ mempty <> ATrue == ATrue <> mempty && ATrue <> mempty == ATrue
--   log $ show $ mempty <> AFalse == AFalse <> mempty && AFalse <> mempty == AFalse

verifyOrBoolSemigroup :: Effect Unit
verifyOrBoolSemigroup = do
  log "Verifying OrBool Semigroup Laws (1 test)"
  log $ show $ (OFalse <> OTrue) <> OTrue == OFalse <> (OTrue <> OTrue)

verifyOrBoolMonoid :: Effect Unit
verifyOrBoolMonoid = do 
  log "Verify OrBool Monoid Laws (2 tests)"
  log $ show $ mempty <> OTrue == OTrue <> mempty && OTrue <> mempty == OTrue 
  log $ show $ mempty <> OFalse == OFalse <> mempty && OFalse <> mempty == OFalse

verifyMod4Semigroup :: Effect Unit
verifyMod4Semigroup = do
  log "Verify Mod4 Semigroup Laws (1 test)"
  log $ show $ (One <> Two) <> Three == One <> (Two <> Three)
