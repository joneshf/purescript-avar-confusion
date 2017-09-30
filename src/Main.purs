module Main where

import Prelude

import Control.Monad.Eff (kind Effect, Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Eff.Ref (REF, modifyRef, newRef, readRef)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Function.Uncurried as Fn
import Data.Maybe (Maybe(..))
import Test.Assert (ASSERT, assert')

type TestEff = Eff (avar ∷ AVAR, assert ∷ ASSERT, console ∷ CONSOLE, ref ∷ REF)

main ∷ TestEff Unit
main = do
  test_put_take

test ∷ String → TestEff Boolean → TestEff Unit
test s k = k >>= \r → assert' s r *> log ("[OK] " <> s)

test_put_take ∷ TestEff Unit
test_put_take = test "put/take" do
  ref ← newRef ""
  var ← makeEmptyVar
  _ ← putVar "foo" var $ traverse_ \_ →
    modifyRef ref (_ <> "bar")
  _ ← takeVar var $ traverse_ \val →
    modifyRef ref (_ <> val)
  eq "barfoo" <$> readRef ref

type AVarEff eff = Eff (avar ∷ AVAR | eff)

type AVarCallback eff a = (Either Error a → AVarEff eff Unit)

foreign import data AVar ∷ Type → Type

foreign import data AVAR ∷ Effect

data AVarStatus a
  = Killed Error
  | Filled a
  | Empty

-- | Creates a fresh AVar.
foreign import makeEmptyVar ∷ ∀ eff a. AVarEff eff (AVar a)

-- | Sets the value of the AVar. If the AVar is already filled, it will be
-- | queued until the value is emptied. Multiple puts will resolve in order as
-- | the AVar becomes available. Returns an effect which will remove the
-- | callback from the pending queue.
putVar ∷ ∀ eff a. a → AVar a → AVarCallback eff Unit → AVarEff eff (AVarEff eff Unit)
putVar value avar cb = Fn.runFn4 _putVar ffiUtil value avar cb

-- | Takes the AVar value, leaving it empty. If the AVar is already empty,
-- | the callback will be queued until the AVar is filled. Multiple takes will
-- | resolve in order as the AVar fills. Returns an effect which will remove
-- | the callback from the pending queue.
takeVar ∷ ∀ eff a. AVar a → AVarCallback eff a → AVarEff eff (AVarEff eff Unit)
takeVar avar cb = Fn.runFn3 _takeVar ffiUtil avar cb

foreign import _putVar ∷ ∀ eff a. Fn.Fn4 FFIUtil a (AVar a) (AVarCallback eff Unit) (AVarEff eff (AVarEff eff Unit))
foreign import _takeVar ∷ ∀ eff a. Fn.Fn3 FFIUtil (AVar a) (AVarCallback eff a) (AVarEff eff (AVarEff eff Unit))

type FFIUtil =
  { left ∷ ∀ a b. a → Either a b
  , right ∷ ∀ a b. b → Either a b
  , nothing ∷ ∀ a. Maybe a
  , just ∷ ∀ a. a → Maybe a
  , killed ∷ ∀ a. Error → AVarStatus a
  , filled ∷ ∀ a. a → AVarStatus a
  , empty ∷ ∀ a. AVarStatus a
  }

ffiUtil ∷ FFIUtil
ffiUtil =
  { left: Left
  , right: Right
  , nothing: Nothing
  , just: Just
  , killed: Killed
  , filled: Filled
  , empty: Empty
  }
