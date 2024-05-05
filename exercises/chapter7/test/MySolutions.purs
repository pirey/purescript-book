module Test.MySolutions where

import Prelude

import Data.AddressBook (Address, PhoneNumber, PhoneType(..), address, phoneNumber)
import Data.AddressBook.Validation (Errors, matches, nonEmpty, validateAddress, validatePhoneNumbers)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String.Regex (Regex)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (class Foldable, class Traversable, foldMap, foldl, foldr, sequence, traverse)
import Data.Validation.Semigroup (V)

addMaybe :: forall a. Semiring a => Maybe a -> Maybe a -> Maybe a
addMaybe m1 m2 = (+) <$> m1 <*> m2

subMaybe :: Maybe Int -> Maybe Int -> Maybe Int
subMaybe m1 m2 = (-) <$> m1 <*> m2

mulMaybe :: Maybe Int -> Maybe Int -> Maybe Int
mulMaybe m1 m2 = (*) <$> m1 <*> m2

divMaybe :: Maybe Int -> Maybe Int -> Maybe Int
divMaybe m1 m2 = (/) <$> m1 <*> m2

addApply :: forall f a. Apply f => Semiring a => f a -> f a -> f a
addApply m1 m2 = (+) <$> m1 <*> m2

subApply :: forall f a. Apply f => Ring a => f a -> f a -> f a
subApply m1 m2 = (-) <$> m1 <*> m2

mulApply :: forall f a. Apply f => Semiring a => f a -> f a -> f a
mulApply m1 m2 = (*) <$> m1 <*> m2

divApply :: forall f a. Apply f => EuclideanRing a => f a -> f a -> f a
divApply m1 m2 = (/) <$> m1 <*> m2

combineMaybe :: forall a f. Applicative f => Maybe (f a) -> f (Maybe a)
combineMaybe (Just x) = map Just x
combineMaybe _ = pure Nothing

stateRegex :: Regex
stateRegex = unsafeRegex "^[a-zA-Z]{2}$" noFlags

nonEmptyRegex :: Regex
nonEmptyRegex = unsafeRegex "\\S" noFlags

validateAddressImproved :: Address -> V (Array String) Address
validateAddressImproved addr = ado
  street <- matches "Street" nonEmptyRegex addr.street
  city <- matches "City" nonEmptyRegex addr.city
  state <- matches "State" stateRegex addr.state
in address street city state

data Tree a = Leaf | Branch (Tree a) a (Tree a)

derive instance Eq a => Eq (Tree a)
derive instance Generic (Tree a) _

instance Show a => Show (Tree a) where
  show t = genericShow t

instance Functor Tree where
  map _ Leaf = Leaf
  map f (Branch t1 v t2) = Branch (map f t1) (f v) (map f t2)

instance Foldable Tree where
  foldl _ acc Leaf = acc
  foldl f acc (Branch t1 v t2) = foldl f (f (foldl f acc t1) v) t2
  foldr _ acc Leaf = acc
  foldr f acc (Branch t1 v t2) = foldr f (f v (foldr f acc t2)) t1
  foldMap _ Leaf = mempty
  foldMap f (Branch t1 v t2) = foldMap f t1 <> f v <> foldMap f t2


instance Traversable Tree where
  traverse _ Leaf = pure Leaf
  traverse f (Branch t1 v t2) = ado
    mt1 <- traverse f t1
    mv <- f v
    mt2 <- traverse f t2
    in Branch mt1 mv mt2

  sequence Leaf = pure Leaf
  sequence (Branch t1 v t2) = ado
    mt1 <- sequence t1
    mv <- v
    mt2 <- sequence t2
    in Branch mt1 mv mt2

traversePreOrder :: forall a m b. Applicative m => (a -> m b) -> Tree a -> m (Tree b)
traversePreOrder _ Leaf = pure Leaf
traversePreOrder f (Branch t1 v t2) = ado
  mv <- f v
  mt1 <- traversePreOrder f t1
  mt2 <- traversePreOrder f t2
  in Branch mt1 mv mt2

traversePostOrder :: forall a m b. Applicative m => (a -> m b) -> Tree a -> m (Tree b)
traversePostOrder _ Leaf = pure Leaf
traversePostOrder f (Branch t1 v t2) = ado
  mt1 <- traversePostOrder f t1
  mt2 <- traversePostOrder f t2
  mv <- f v
  in Branch mt1 mv mt2

type PersonOptionalAddress
  = { firstName :: String
    , lastName :: String
    , homeAddress :: Maybe Address
    , phones :: Array PhoneNumber
    }

personOptionalAddress :: String -> String -> Maybe Address -> Array PhoneNumber -> PersonOptionalAddress
personOptionalAddress firstName lastName homeAddress phones = { firstName, lastName, homeAddress, phones }

validatePersonOptionalAddress :: PersonOptionalAddress -> V Errors PersonOptionalAddress
validatePersonOptionalAddress p =
  personOptionalAddress
    <$> nonEmpty "First Name" p.firstName
    <*> nonEmpty "Last Name" p.lastName
    <*> traverse validateAddress p.homeAddress
    <*> validatePhoneNumbers "Phone Numbers" p.phones

sequenceUsingTraverse :: forall a m t. Traversable t => Applicative m => t (m a) -> m (t a)
sequenceUsingTraverse t = traverse identity t

-- Exercise 7
traverseUsingSequence :: forall a b m t. Traversable t => Applicative m => (a -> m b) -> t a -> m (t b)
traverseUsingSequence f t = sequence $ map f t

examplePerson :: PersonOptionalAddress
examplePerson =
  { firstName: "John"
  , lastName: "Smith"
  -- , homeAddress: Just $ address "123 Fake St." "FakeTown" "CA"
  , homeAddress: Nothing
  , phones:
  [ phoneNumber HomePhone "555-555-5555"
  , phoneNumber CellPhone "555-555-0000"
  ]
}
