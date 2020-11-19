module Shapes
( Point(..)
, Shape(..)
, area
, nudge
, baseCircle
, baseRect
) where

import qualified Data.Map as Map

data Bool' = False' | True' deriving Show

data Shape' = Circle' Float Float Float
            | Rectangle' Float Float Float Float
            deriving Show

data Point = Point Float Float
  deriving (Show)

data Shape = Circle Point Float
           | Rectangle Point Point
  deriving (Show)

area :: Shape -> Float
area (Circle _ r) = pi * r ^ 2
area (Rectangle (Point x1 y1) (Point x2 y2)) =
  (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b =
  Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

nudgedCircle = nudge (Circle (Point 34 34) 10) 5 10

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)

nudgedRectangle = nudge (baseRect 40 100) 60 23
concentricCircles = map (baseCircle) [4,5,6,6]

-- Record Syntax

-- without record syntax... :|
data Person = Person String String Int Float String String deriving (Show)

firstName' :: Person -> String
firstName' (Person firstname _ _ _ _ _) = firstname

lastName' :: Person -> String
lastName' (Person _ lastname _ _ _ _) = lastname

age' :: Person -> Int
age' (Person _ _ age _ _ _) = age

height' :: Person -> Float
height' (Person _ _ _ height _ _) = height

phoneNumber' :: Person -> String
phoneNumber' (Person _ _ _ _ number _) = number

flavour' :: Person -> String
flavour' (Person _ _ _ _ _ flavour) = flavour

guy = Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"
guy'sFirstName = firstName' guy
guy'sHeight = height' guy
guy'sFlavour = flavour' guy

-- with record syntax :)
data Person2 = Person2 { firstName :: String
                       , lastName :: String
                       , age :: Int
                       , height :: Float
                       , phoneNumber :: String
                       , flavour :: String
                       } deriving (Show)

frank = Person2 "Frank" "J" 12 1.12 "1234123" "arst"
frank' = Person2 { firstName="Frank", lastName="J"
                 , age=12, height=1.12
                 , phoneNumber="1234123", flavour="arst" }

data Car = Car { company :: String
               , model :: String
               , year :: Int
               } deriving (Show)

-- Can change the order of the fields
-- Can't do this without record syntax

myBaby = Car {model="Mustang", company="Ford", year=1967}

-- use record syntax with lots of params that aren't obvious
-- e.g. data Vector = Vector Int Int Int is resonably obvious


-- Type Parameters

data Maybe' a = Nothing' | Just' a -- Maybe is a type constructor

maybeInt = Just 3 :: Maybe Int
maybeFloat = Just 3 :: Maybe Float

-- A type is concrete if it doesn't take any parameters at all, like
-- Int or Bool, or if it takes type parameters and they're all filled
-- up, like Maybe Char.

data IntMaybe = INothing | IJust Int
data StringMaybe = SNothing | SJust String
data ShapeMaybe = ShNothing | ShJust Shape

-- type params are better as you don't get ^^ that.

tellCar :: Car -> String
tellCar (Car {company=c, model=m, year=y}) =
  "This " ++ c ++ " " ++ m ++ " was made in " ++ show y
stang = Car {company="Ford", model="Mustang", year=1967}

-- parameterising Car value constructor i.e. Car a b c = Car {...
-- is pointless as tellCar becomes more complicated and we'd end
-- up using Car String String Int most of the time anyway.

-- bad practice to put type constraints in data declarations
-- e.g. toList :: Ord k => Map k a -> [(k, a)] doesn't care about
-- the order of the keys, so no need for (Ord k)

-- No type constraint
data Vector a = Vector a a a deriving (Show)

-- Even if you put it in the type, you still have to put in on the functions

vplus :: Num a => Vector a -> Vector a -> Vector a
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

dotProd :: Num a => Vector a -> Vector a -> a
(Vector i j k) `dotProd` (Vector l m n) = i*l + j*m + k*n

vmult :: Num a => Vector a -> a -> Vector a
(Vector i j k) `vmult` m = Vector (i*m) (j*m) (k*m)

v1 = Vector 3 5 8 `vplus` Vector 9 2 8
v2 = Vector 3 5 8 `vplus` Vector 9 2 8 `vplus` Vector 0 2 3
v3 = Vector 3 9 7 `vmult` 10
v4 = Vector 4 9 5 `dotProd` Vector 9.0 2.0 4.0
v5 = Vector 2 9 3 `vmult` (Vector 4 9 5 `dotProd` Vector 9 2 4)

-- Derived Instances
-- Equating People
data Person3 = Person3 { firstName3 :: String
                       , lastName3 :: String
                       , age3 :: Int
                       } deriving (Eq, Show, Read)

mikeD = Person3 {firstName3="Michael", lastName3="Diamond", age3=43}
adRock = Person3 {firstName3="Adam", lastName3="Horovitz", age3=41}
mca = Person3 {firstName3="Adam", lastName3="Yauch", age3=44}

mca'adRock = mca == adRock
mikeD'adRock = mikeD == adRock
mikeD'mikeD = mikeD == mikeD
mikeD'newPerson = mikeD == Person3 {firstName3="Michael", lastName3="Diamond", age3=43}

beastieBoys = [mca, adRock, mikeD]
isMikeDInBeastieBoys = mikeD `elem` beastieBoys

showMikeD = show mikeD
showMikeD' = "mikeD is: " ++ show mikeD

mysteryDude = "Person3 { firstName3=\"Michael\"" ++
              ", lastName3=\"Diamond\"" ++
              ", age3=43}"
readMysteryDude = read mysteryDude :: Person3

readJust3 = read "Just 3" :: Maybe Int

-- data Bool = False | True deriving (Ord)
trueCompFalse = True `compare` False
trueGtFalse = True > False
trueLtFalse = True < False
-- instances of Ord are ordered in the manner they are defined.
nothingLtJust100 = Nothing < Just 100
nothingGtJustMinus49999 = Nothing > Just (-49999)
just3cmpJust2 = Just 3 `compare` Just 2
just100GtJust50 = Just 100 > Just 50
-- can't do Just (*3) > Just (*2). (*3),(*2) aren't instances of Ord


-- Any Day of the Week
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Eq, Ord, Show, Read, Bounded, Enum)

wednesday = Wednesday
showWednesday = show Wednesday
readSaturday = read "Saturday" :: Day

saturdayEqSunday = Saturday == Sunday
saturdayEqSaturday = Saturday == Saturday
minDay = minBound :: Day
maxDay = maxBound :: Day
succMonday = succ Monday
predSaturday = pred Saturday
rangeThuSun = [Thursday .. Sunday]
minMaxRangeDay = [minBound .. maxBound] :: [Day]


-- Type Synonyms
-- 类型别名（同义）

-- [Char] and String are type synonyms.
-- defined as: type String = [Char]

-- data creates a new type
-- type creates a type synonym... First dodgy thing so far.

-- the following are exactly the same
-- toUpperString :: [Char] -> [Char]
-- toUpperString :: String -> String

phoneBook :: [(String, String)]
phoneBook =
    [("betty", "555-2938")
    ,("bonnie", "452-2928")
    ,("patsy", "493-2928")
    ,("lucille", "205-2928")
    ,("wendy", "939-8282")
    ,("penny", "853-2492")
    ]
-- this definition doesn't tell us much other than it's a "String to String" association list.

type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)]
-- could then say: phoneBook :: PhoneBook

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name, pnumber) `elem` pbook

-- parameterised type synonyms
type AssocList k v = [(k, v)]
-- can be used like: Eq k => k -> AssocList k v -> Maybe v

-- partially applied type constructor
type IntMap v = Map.Map Int v
-- or
type IntMap2 = Map.Map Int

-- don't confuse type synonyms and type constructors
-- we can't do: AssocList [(1,2),(4,5),(7,9)]

-- we can do:
assocListIntInt = [(1,2),(3,5),(8,9)] :: AssocList Int Int
-- and we can still use the list like a normal (Int,Int) list

-- type synonyms (and types generally) can only be used in the type portion
-- of Haskell. This includes data & type declarations as well as after ::
-- in type declarations or type annotations


-- Go Left, Then Right
data Either' a b = Left' a
                 | Right' b
  deriving (Eq, Ord, Read, Show)

right'20 = Right' 20
left'woot = Left' "w00t"
-- :t Right' 'a'
-- :t Left True

-- Nothing is used to indicate failure
-- If interested in how/why something failed:
-- use Left constructor for failures, Right constructor for succes results

data LockerState = Taken
                 | Free
  deriving (Show, Eq)

type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map
  = case Map.lookup lockerNumber map of
      Nothing            -> Left $ "Locker "
                                    ++ show lockerNumber
                                    ++ " doesn't exist!"
      Just (state, code) -> if state /= Taken
                            then Right code
                            else Left $ "Locker "
                                         ++ show lockerNumber
                                         ++ " is already taken!"

lockers :: LockerMap
lockers = Map.fromList
    [(100,(Taken, "ZD39I"))
    ,(101,(Free, "JAH3I"))
    ,(102,(Free, "IQSA9"))
    ,(103,(Free, "QOTSA"))
    ,(104,(Taken, "893JJ"))
    ,(105,(Taken, "99292"))
    ]

locker = lockerLookup 101 lockers
locker' = lockerLookup 100 lockers
locker'' = lockerLookup 109 lockers


-- Recursive Data Structures

data List a = Empty
            | Cons a (List a)
  deriving (Show, Read, Eq, Ord)

-- using record syntax
-- data List a = Empty | Cons { listHead :: a, listTail :: List a }
--     deriving (Show, Read, Eq, Ord)

empty = Empty
fiveConsEmpty = 5 `Cons` Empty
fourConsFiveCons = 4 `Cons` (5 `Cons` Empty)
-- using `Cons` here to show that it is just the : operator

-- improving our list
infixr 5 :-:       -- fixity: how tightly the operator binds and whether
                   -- it's left or right-associative
                      -- infixl/infixr
                      -- bigger number higher precedence

data List' a = Empty'
             | a :-: List' a
  deriving (Show, Read, Eq, Ord)

consing = 3 :-: 4 :-: 5 :-: Empty'
hundredConsWithConsing = 100 :-: consing

-- our implementation of ++
infixr 5 ^++

(^++) :: List' a -> List' a -> List' a
Empty'     ^++ ys = ys
(x :-: xs) ^++ ys = x :-: (xs ^++ ys)

listOne = 3 :-: 4 :-: 5 :-: Empty'
listTwo = 6 :-: 7 :-: Empty'
listThree = listOne ^++ listTwo

-- Let's Plant a Tree
data Tree a = EmptyTree
            | Node a (Tree a) (Tree a)
  deriving (Show)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: Ord a => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a  = Node a (treeInsert x left) right
    | x > a  = Node a left (treeInsert x right)

treeElem :: Ord a => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a = treeElem x left
    | x > a = treeElem x right

nums = [8,6,4,1,7,3,5]
-- remember: pretty much everything that traverses a list one item at a
--           time and returns a value can be implemented with a fold.
numsTree = foldr treeInsert EmptyTree nums

eightInTree = 8 `treeElem` numsTree
hundredInTree = 100 `treeElem` numsTree
oneInTree = 1 `treeElem` numsTree
tenInTree = 10 `treeElem` numsTree


-- Type Classes 102

{-- definition of Eq
class Eq a where            -- 'a' must be lowercase. >1 char allowed
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool  -- function type declarations must be provided
    x == y = not (x /= y)
    x /= y = not (x == y)   -- implementation doesn't

-- Equal if not inequal. inequal if not equal
-- Final type of functions will be: Eq a => a -> a -> Bool
-}

-- A traffic light data type
data TrafficLight = Red | Yellow | Green  -- no derivations here.

instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False
-- class used to create new classes
-- instance for making types instances of classes
-- 'a' in class definition replaced with type in instance definition

-- Only need to implement == as class definition has them defined in terms
-- of one another.
-- Called: Minimal Complete Definition

-- Otherwise both would need to be implemented in the instance definition

-- Hand rolling it as an instance of Show
instance Show TrafficLight where
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"

redEqRed = Red == Red
redEqYellow = Red == Yellow
redElem = Red `elem` [Red, Yellow, Green]
showTrafficLights = [Red, Yellow, Green]
-- deriving Eq would work same way
-- deriviing Show wouldn't be same. It'd just say Red, Yellow or Green

-- subclassing
{-
class Eq a => Num a where   -- says that a type must be an Eq before
                            -- it can be a Num. subclass through class
                            -- constraint on class definition!
    -- ... Function declarations here
-}

-- parameterized types as instances of type classes
{-
instance Eq m => Eq (Maybe m) where  -- this definition guarantees m
                                     -- is an instance of Eq first
    Just x == Just y = x == y
    Nothing == Nothing = True
    _ == _ = False
-}

-- class constraints in class defs are about subclassing.
-- class constraints in instance defs are requirements about type contents

-- :info Maybe
-- shows all the type classes that Maybe is an instance of.


-- A Yes-No type class: javascript 'if' like behaviour for shits & giggles
-- 0, "" and false are all falsey in javascript

class YesNo a where
    yesno :: a -> Bool

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where -- works on all lists. remember String == [Char]
    yesno [] = False
    yesno _ = True

instance YesNo Bool where
    yesno = id  -- id is the identity function

instance YesNo (Maybe a) where
    yesno (Just _) = True
    yesno Nothing = False

instance YesNo (Tree a) where
    yesno EmptyTree = False
    yesno _ = True

instance YesNo TrafficLight where
    yesno Red = False
    yesno _ = True

yesnoZeroLength = yesno $ length []
yesnoHaha = yesno "haha"
yesnoEmptyString = yesno ""
yesnoJustZero = yesno $ Just 0
yesnoTrue = yesno True
yesnoEmptyTree = yesno EmptyTree
yesnoEmptyList = yesno []
yesnoListOfZeroes = yesno [0,0,0]
-- :t yesno

-- now for the javascript like 'if'
yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult =
    if yesno yesnoVal
       then yesResult
       else noResult

ifResult = yesnoIf [] "Yeah!" "No!"
ifResult' = yesnoIf [2,3,4] "Yeah!" "No!"
ifResult'' = yesnoIf True "Yeah!" "No!"
ifResult''' = yesnoIf (Just 500) "Yeah!" "No!"
ifResult'''' = yesnoIf Nothing "Yeah!" "No!"


-- The Functor Type Class

{-
class Functor f where               -- For things that can be mapped over
    fmap :: (a -> b) -> f a -> f b
-}

{- --E.g.
instance Functor [] where
    fmap = map
-}
fmapList = fmap (*2) [1..3]
mapList = map (*2) [1..3]

-- f in the class definition of Functor should be a type constructor
-- not a concrete type!

-- Maybe as a functor
{-
instance Functor Maybe where
    fmap f (Just x) = Just (f x)
    fmap f Nothing = Nothing
-}
fmapTest = fmap (++ " Hey guys im inside the just") (Just "Something serious.")
fmapTest' = fmap (++ "Hey guys im inside the just") Nothing
fmapTest'' = fmap (*2) (Just 200)
fmapTest''' = fmap (*2) Nothing

-- Trees are functors too
instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)

fmapEmptyTree = fmap (*2) EmptyTree
fmapMultiplyTreeElems = fmap (*4) (foldr treeInsert EmptyTree [5,7,3])

-- Either as a functor
{-
instance Functor (Either a) where
    fmap f (Right x) = Right (f x)
    fmap f (Left x) = Left x
-}

{-
instance Functor (Map k) where
    fmap = map
-}


-- Kinds and Some Type-Foo

{-
:k Int
Int :: * -- This indicates that Int is a concrete type

:k Maybe
Maybe :: * -> *   -- takes a concrete type and returns another

:k Maybe Int
Maybe Int :: *  -- Maybe Int is a concrete type
-}

-- types are the labels of values
-- kinds are the labels of types

{-
:k Either
Either :: * -> * -> * -- takes 2 concrete types to produce a concrete type

:k Either String
Either String :: * -> *

:k Either String Int
Either String Int :: *
-}

-- Functor wants types that only take one argument.
-- Functor wants types of kind * -> *
-- This is why we had to give Functor an (Either a) and not just Either
-- !
