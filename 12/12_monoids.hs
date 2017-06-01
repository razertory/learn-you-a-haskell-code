-- MONOIDS


-- This type class if for types whose values can be combineg together with
-- a binary operation.

-- A monoid is made up of an associative binary function and a value that
-- acts as an identity with respect to that function. E.g. 1 with * and [] with ++.
class Monoid m where
    mempty  :: m
    mappend :: m -> m -> m

    mconcat :: [m] -> m
    mconcat  = foldr mappend mempty

-- first: only concrete types can be made instance of Monoid. This is
-- different to Applicative and Functor.
--
-- mempty (a polymorphic constant) represents the identity value for a
-- particular monoid.
--
-- mappend is the binary function. It takes two monoid values and returns
-- a third of the same type.
--
-- mconcat takes a list of monoid values and reduces it to one value using
-- mappend. The default implementation of mconcat is fine for most cases,
-- so we generally don't worry about it.


-- The Monoid Laws
         mempty `mappend` x = x
         x `mappend` mempty = x
(x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)
-- the first two state that mempty acts like the identity wrt mappend
-- the third states that mappend is associative.


-- Meet Some Monoids

-- List Are Monoids
instance Monoid [a] where
    mempty  = []
    mappend = (++)
-- remember we require a concrete type, hence [a] instead of []
[1,2,3] `mappend` [4,5,6]
-- [1,2,3,4,5,6]
("one" `mappend` "two") `mappend` "three"
-- "onetwothree"
"one" `mappend` ("two" `mappend` "three")
-- "onetwothree"
"one" `mappend` "two" `mappend` "three"
-- "onetwothree"
"pang" `mappend` mempty
-- "pang"
mconcat [[1,2],[3,6],[9]]
-- [1,2,3,6,9]
mempty :: [a]
-- []

-- Product and Sum
-- we've already noted that * and 1 together can be used as a monoid
0 + 4
-- 4
5 + 0
-- 5
(1 + 3) + 5
1 + (3 + 5)

-- we have two equally valid ways that numbers can be monads, so
-- Data.Monoid uses newtype!
newtype Product a = Product { getProduct :: a }
    deriving (Eq, Ord, Read, Show, Bounded)

instance Num a => Monoid (Product a) where
    mempty = Product 1
    Product x `mappend` Product y = Product (x * y)
-- sum is defined similarly

getProduct $ Product 3 `mappend` Product 9
-- 27
getProduct $ Product 3 `mappend` mempty
-- 3
getProduct $ Product 3 `mappend` Product 4 `mappend` Product 2
-- 24
getProduct . mconcat . map Product $ [3,4,2]
-- 24
getSum $ Sum 2 `mappend` Sum 9
-- 11
getSum $ mempty `mappend` Sum 3
-- 3
getSum . mconcat . map Sum $ [1,2,3]
-- 6

-- Any and All

-- Bool with || and && are two different ways we can act like a monoid
newtype Any = Any { getAny :: Bool }
    deriving (Eq, Ord, Read, Show, Bounded)

instance Monoid Any where
    mempty = Any False
    Any x `mappend` Any y = Any (x || y)

getAny $ Any True `mappend` Any False
-- True
getAny $ mempty `mappend` Any True
-- True
getAny . mconcat . map Any $ [False, False, False, True]
-- True
getAny $ mempty `mappend` mempty
-- False
-- if *Any* of the values are true, then the result is true

newtype All = All { getAll :: Bool }

instance Monoid All where
    mempty = All True
    All x `mappend` All y = All (x && y)

getAll $ mempty `mappend` All True
-- True
getAll $ mempty `mappend` All False
-- False
getAll . mconcat . map All $ [True, True, True]
-- True
getAll . mconcat . map All $ [True, True, False]
-- False

-- The Ordering Monoid
1 `compare` 2
-- LT
2 `compare` 2
-- EQ
3 `compare` 2
-- GT

instance Monoid Ordering where
    mempty = EQ
    LT `mappend` _ = LT
    EQ `mappend` y = y
    GT `mappend` _ = GT
-- We keep the value on the left, unless it is EQ, then we take the right
-- value
--
-- This implementation is similar to how we alphabetically compare words.
LT `mappend` GT
-- LT
GT `mappend` LT
-- GT
mempty `mappend` LT
-- LT
mempty `mappend` GT
-- GT

-- How is this monoid useful? Imagine we need a function that takes two
-- strings, compares their lengths, and returns an Ordering. But if the
-- strings are of the same length, instead of returning EQ right away, we
-- want to compare them alphabetically. One way:
lengthCompare    :: String -> String -> Ordering
lengthCompare x y = let a = length x `compare` length y
                        b = x `compare` y
                    in  if a == EQ then b else a
-- as a monoid:
import Data.Monoid

lengthCompare    :: String -> String -> Ordering
lengthCompare x y = (length x `compare` length y) `mappend`
                    (x `compare` y)

lengthCompare "zen" "ants"
-- LT
lengthCompare "zen" "ant"
-- GT

-- Using Ordering monoid properties to build a function, the lhs is kept
-- unless it is EQ then it's the rhs that's taken. So putting the most
-- important criterion as the first parameter.

-- Expanding lengthCompare to have a new second most important criterion
lengthCompare    :: String -> String -> Ordering
lengthCompare x y = (length x `compare` length y) `mappend`
                    (vowels x `compare` vowels y) `mappend`
                    (x `compare` y)
    where vowels  = length . filter (`elem` "aeiou")

lengthCompare "zen" "anna"
-- LT
lengthCompare "zen" "ana"
-- LT
lengthCompare "zen" "ann"
-- GT

-- The Ordering monoid allows us to easily compare things by many different
-- criteria and put those criteria in an order themselves, ranging from
-- most important to least.


-- Maybe the Monoid

instance Monoid a => Monoid (Maybe a) where
    mempty = Nothing
    Nothing `mappend` m = m
    m `mappend` Nothing = m
    Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)
-- Class constraint says: Maybe a is an instance of Monoid only if a is an
-- instance of monoid
Nothing `mappend` Just "andy"
-- Just "andy"
Just LT `mappend` Nothing
-- Just LT
Just (Sum 3) `mappend` Just (Sum 4)
-- Just (Sum {getSum = 7})
-- This is useful when we're dealing with monoids as results of
-- computations that may have failed.

-- What if the type of the contents of the Maybe is not an instance of
-- Monoid? In the previous declaration the only case where we must rely
-- on the contents being monoids is when both parameters of mappend are
-- Just values. When we don't know if the contents are monoids, we can't
-- use mappend between them.
--
-- Instead, we can discard the second value and keep the first one. For
-- that purpose, the First a type exists
newtype First a = First { getFirst :: Maybe a }
    deriving (Eq, Ord, Read, Show)

instance Monoid (First a) where
    mempty = First Nothing
    First (Just x) `mappend` _ = First (Just x)
    First Nothing  `mappend` x = x

getFirst $ First (Just 'a') `mappend` First (Just 'b')
-- Just 'a'
getFirst $ First Nothing `mappend` First (Just 'b')
-- Just 'b'
getFirst $ First (Just 'a') `mappend` First Nothing
-- Just 'a'
-- First is useful if we have a bunch of Maybe values and we just want to
-- know if any of them is a Just
getFirst . mconcat . map First $ [Nothing, Just 9, Just 10]
-- Just 9

-- if we want to keep the second parameter if both given to mappend are
-- Just values then Data.Monoid provides Last. Works similar to First.
getLast . mconcat . map Last $ [Nothing, Just 9, Just 10]
-- Just 10
getLast $ Last (Just "one") `mappend` Last (Just "two")
-- Just "two"


-- Folding with Monoids
--
-- Because there are so many datastructures that work well with folds,
-- the Foldable type class was introduced. Like Functor is for things that
-- can be mapped over, Foldable is for things that can be folded up. It's
-- found in Data.Foldable. We can use this with Monoids.
import qualified Data.Foldable as F

-- This is no longer true in new versions of the Prelude.
:t foldr
-- foldr :: (a -> b -> b) -> b -> [a] -> b
:t F.foldr
-- F.foldr :: (F.Foldable t) => (a -> b -> b) -> b -> t a -> b

foldr (*) 1 [1,2,3]
-- 6
F.foldr (*) 1 [1,2,3]
-- 6
F.foldl (+) 2 (Just 9)
-- 11
F.foldr (||) False (Just True)
-- True

-- Recall the tree data structure from Chapter 7:
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)
-- In chapter 7, we made it an instace of Functor, now Foldable

-- One way to make the Tree type constructor an instance of Foldable is
-- to directly implement foldr. Another, of much easier way, is to
-- implement the foldMap function of the Foldable type class.
:t foldMap
foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
--
-- first parameter is a function that takes a value of the type the foldable
-- structure contains and returns a monoid value.
-- Second param is a foldable structure containing values of type a
-- It maps that function over the structure, producing a foldable
-- structure that contains monoid values. Then, by doing mappend between
-- those monoid values and joins them into a single monoid value.
--
-- All we have to do is implement foldMap and we get foldr, foldl for free
instance F.Foldable Tree where
    foldMap f EmptyTree    = mempty
    foldMap f (Node x l r) = F.foldMap f l `mappend`
                             f x           `mappend`
                             F.foldMap f r

testTree = Node 5
            (Node 3
                (Node 1 EmptyTree EmptyTree)
                (Node 6 EmptyTree EmptyTree)
            )
            (Node 9
                (Node 8 EmptyTree EmptyTree)
                (Node 10 EmptyTree EmptyTree)
            )

F.foldl (+) 0 testTree
-- 42
F.foldl (*) 1 testTree
-- 64800

-- foldMap isn't just useful for creating new instances of Foldable. It
-- also comes in handy for reducing our structure to a single monoid value.
-- If we want to know if any number in our tree is equal to 3, we can:
getAny $ F.foldMap (\x -> Any $ x == 3) testTree
-- True
-- The function takes a number and returns a monoid value: a Bool wrapped
-- in Any.
getAny $ F.foldMap (\x -> Any $ x > 15) testTree
-- False
F.foldMap (\x -> [x]) testTree
-- [1,3,6,5,8,9,10]
-- Each element in the tree becomes a singleton list. Then mappend takes
-- place between all those elements and concatenates them all into a single
-- list.
--
-- These tricks work on any instance of Foldable.
