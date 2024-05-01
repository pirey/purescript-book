module Test.MySolutions where

import Prelude

import Data.Array (length, nub, nubByEq, nubEq)
import Data.Foldable (class Foldable, foldMap, foldl, foldr, maximum)
import Data.Formatter.Internal (repeat)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Hashable (class Hashable, hash, hashEqual)

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


unsafeMaximum :: Partial => Array Int -> Int
unsafeMaximum xs =
  case maximum xs of
      Nothing -> 100
      Just x -> x

newtype Multiply = Multiply Int

instance Semigroup Multiply where
  append (Multiply n) (Multiply m) = Multiply (n * m)

instance Monoid Multiply where
  mempty = Multiply 1

class Monoid m <= Action m a where
  act :: m -> a -> a

instance Action Multiply Int where
  act (Multiply n) m = n * m

instance Action Multiply String where
  act (Multiply n) s = repeat s n

instance Action m a => Action m (Array a) where
  act m arr = map (act m) arr

newtype Self m = Self m

instance Monoid m => Action m (Self m) where
  act m1 (Self m2) = Self (m1 <> m2)

derive newtype instance Show Multiply
derive newtype instance Eq Multiply
derive newtype instance Show m => Show (Self m)
derive newtype instance Eq m => Eq (Self m)

arrayHasDuplicates :: forall a. Hashable a => Array a -> Boolean
arrayHasDuplicates arr =
  let
      hashAndValEqual a b = hashEqual a b && a == b
  in
    length arr /= (length $ nubByEq hashAndValEqual arr)

newtype Hour = Hour Int

instance Eq Hour where
  eq (Hour n) (Hour m) = mod n 12 == mod m 12

instance Hashable Hour where
  hash (Hour h) = hash $ mod h 12
