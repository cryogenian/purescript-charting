module Main where

import Prelude
import Control.Monad.Eff (Eff())
import Debug.Trace
import Unsafe.Coerce
import Data.Either
import Control.Monad.Free
import Data.Void

data Exists (f :: # * -> *)

foreign import data Position :: *
foreign import data Size :: *
foreign import data Tick :: *
foreign import data Color :: *

data LabelF next
  = Position Position next
  | Shown Boolean next
  | Tick Tick next
  | Name String next

type Label = Free LabelF

data ScaleF a next
  = Min a next
  | Max a next
  | Smallest (a -> next)
  | Biggest (a -> next)
  | Average (a -> next)
  | All (Array a -> next)

type Scale a = Free (ScaleF a)

type Projection a x =
  { get :: a -> x
  , label :: Label Unit
  , scale :: Scale x Unit
  }

newtype SerieI a c r =
  SerieI { _type :: String
         , child :: c
         | r}
type Serie a c = Exists (SerieI a c)

type LineSerieR a r =
  { x :: Projection a (Either Number String)
  , y :: Projection a Number
  | r }

type PieSerieR a r =
  { theta :: Projection a Number
  , radius :: Projection a Number
  , color :: Projection a Color
  | r }

type BarSerieR a r =
  -- Here `cat` shouldn't be `String` but something like
  -- {name :: String, stack :: Boolean}
  { cat :: Projection a String
  , val :: Projection a Number
  | r }

line :: forall a r. LineSerieR a r -> Serie a Void
line r = unsafeCoerce r # _{_type = "line"} # unsafeCoerce

bar :: forall a r. BarSerieR a r -> Serie a Void
bar r = unsafeCoerce r # _{_type = "bar"} # unsafeCoerce

pie :: forall a r. PieSerieR a r -> Serie a Void
pie r = unsafeCoerce r # _{_type = "pie"} # unsafeCoerce

newtype Chart f a = Chart (f a)


data InstallF a next
  = Size Size next
  | InstallPosition Position next
  | ShowAxisLabel (forall r out. r -> Projection a out) Boolean next
  | XLabelPosition Position next
  | YLabelPosition Position next

type Install a = Free (InstallF a)

install :: forall f a r c. Chart f a -> Serie a c -> Install a Unit -> Chart f a
install chart serie install = chart -- TODO

vertical
  :: forall a b c. Serie a Void -> (a -> b) -> Serie b c -> Serie a (Serie b c)
vertical main projection child = unsafeCoerce main


main :: Eff () Unit
main = do
  traceAnyA "Hello!"
