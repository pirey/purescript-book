module Test.MySolutions where

import Prelude

import Data.Array (nub, nubEq)
import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

-- Point

newtype Point
  = Point
  { x :: Number
  , y :: Number
  }

instance Show Point where
  show (Point {x, y}) = "(" <> show x <> ", " <> show y <> ")"

-- Complex

newtype Complex = Complex { real:: Number, imaginary :: Number }

instance Show Complex where
  show (Complex {real, imaginary}) = show real <> sep <> show imaginary <> "i"
    where
      sep = if imaginary >= 0.0 then "+" else ""

instance Eq Complex where
  eq (Complex {real: r1, imaginary: i1}) (Complex {real: r2, imaginary: i2}) =
    r1 == r2 && i1 == i2

instance Semiring Complex where
  one = Complex {real: 1.0, imaginary: 0.0}

  zero = Complex {real: 0.0, imaginary: 0.0}

  add (Complex {real: r1, imaginary: i1}) (Complex {real: r2, imaginary: i2}) =
    Complex {real: r1 + r2, imaginary: i1 + i2}

  mul (Complex {real: r1, imaginary: i1}) (Complex {real: r2, imaginary: i2}) =
    Complex {real: r1 * r2 + i1 * i2 * -1.0, imaginary: r1 * i2 + r2 * i1}

derive newtype instance complexRing :: Ring Complex

-- Shape

data Shape
  = Circle Point Number
  | Rectangle Point Number Number
  | Line Point Point
  | Text Point String

derive instance genericShape :: Generic Shape _

instance Show Shape where
  show = genericShow

derive instance Eq Point
derive instance Eq Shape
derive instance Ord Point
derive instance Ord Shape

dedupShapes :: Array Shape -> Array Shape
dedupShapes = nubEq

dedupShapesFast :: Array Shape -> Array Shape
dedupShapesFast = nub

-- NonEmtpy

data NonEmpty a = NonEmpty a (Array a)

derive instance Eq a => Eq (NonEmpty a)
-- instance Eq a => Eq (NonEmpty a) where
--   eq (NonEmpty e1 a1) (NonEmpty e2 a2) = e1 == e2 && a1 == a2

instance Show a => Show (NonEmpty a) where
  show (NonEmpty e1 a1) = show e1 <> " " <> show a1

instance Semigroup (NonEmpty a) where
  append (NonEmpty e1 a1) (NonEmpty e2 a2) = NonEmpty e1 (a1 <> [e2] <> a2)

instance Functor NonEmpty where
  map f (NonEmpty e a) = NonEmpty (f e) (map f a)

instance Foldable NonEmpty where
  foldl f z (NonEmpty e a) = foldl f z ([e] <> a)
  foldr f z (NonEmpty e a) = foldr f z ([e] <> a)
  foldMap f (NonEmpty e a) = foldMap f ([e] <> a)

-- Extended

data Extended a = Infinite | Finite a

derive instance Eq a => Eq (Extended a)

instance Ord a => Ord (Extended a) where
  compare Infinite Infinite = EQ
  compare Infinite (Finite _) = GT
  compare (Finite _) Infinite = LT
  compare (Finite a1) (Finite a2) = compare a1 a2

-- OneMore

data OneMore f a = OneMore a (f a)

instance Foldable f => Foldable (OneMore f) where
  foldl fn z (OneMore a fa) = foldl fn (fn z a) fa
  foldr fn z (OneMore a fa) = fn a (foldr fn z fa)
  foldMap fn (OneMore a fa) = fn a <> foldMap fn fa
