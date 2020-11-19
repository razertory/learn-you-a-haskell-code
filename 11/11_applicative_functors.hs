-- Functors Redux
--
-- functors are things that can be mapped over like lists, Maybes and trees
-- They are described by the type class Functor which has one type class method
-- fmap
--
-- :t fmap
-- fmap :: (a -> b) -> f a -> f b
--
-- to make a type constructor an instance of Functor, its kind must be
-- * -> *
-- e.g. Maybe
--
-- To make Either into a Functor instance have to write:
--
-- instance Functor (Either a) where
--
-- its type would then be:
-- :t fmap
-- fmap :: (b -> c) -> Either a b -> Either a c


-- IO Actions as Functors
--
-- IO is an instance of Functor
-- 其中 do 关键字引入一个块，标识那些带有副作用的代码，比如对文件进行读和写操作。被 do 包围的 <- 操作符效果等同于赋值
instance Functor IO where
    fmap f action = do
        result <- action
        return (f result)

-- E.g.
main = do line <- getLine
          let line' = reverse line
          putStrLn $ "You said " ++ line' ++ " backwards!"
          putStrLn $ "Yes, you said " ++ line' ++ " backwards!"
-- run ./11_backwards_1

-- Rewritten using fmap
main = do line <- fmap reverse getLine
          putStrLn $ "You said " ++ line ++ " backwards!"
          putStrLn $ "Yes, you really said " ++ line ++ " backwards!"
-- run ./11_backwards_2

-- mapping reverse over getLine gives us an IO Action that gets a line from
-- standad input and applies reverse to its result
-- biding the result to a name using <- gives a value that has already had
-- reverse applied to it.
--
-- If you ever find yourself binding the respult of an IO action to a name,
-- only then to apply a function to that and call it something else, that's
-- a good candidate for using fmap
--
-- Example of applying more than one function to data inside a functor
import Data.Char
import Data.List

main = do
    line <- fmap (intersperse '-' . reverse . map toUpper) getLine
    putStrLn line
-- run ./11_reverse_intersperse


-- Functions as Functors
--
-- Given a function of type a -> b. It can be rewritten as (->) a b.
-- But this takes two parameters and for it to be a functor it has to
-- take just one.
--
-- the implementation in Control.Monad.Instances is as follows:
instance Functor ((->) r) where
    fmap f g = (\x -> f (g x))
--
-- recall fmap's type
-- fmap :: (a -> b) -> f a -> f b
--
-- replacing each f with (->) r
-- fmap :: (a -> b) -> ((->) r a) -> ((->) r b)
--
-- now we can write (->) r a and (->) r b as infix
-- fmap :: (a -> b) -> (r -> a) -> (r -> b)
--
-- So mapping a function over a function much produce a function.
-- This is function composition!
--
-- Here's another way to write this instance:
instance Functor ((->) r) where
    fmap = (.)

a = fmap (*3) (+100) 1
b = (*3) `fmap` (+100) $ 1
c = (*3) . (+100) $ 1
z = fmap (show . (*3)) (+100) 1
-- load 11_composition_as_fmap.hs


-- Again fmap's type:
-- fmap :: (Functor f) => (a -> b) -> f a -> f b
--
-- because Haskell curries functions this can be writen as:
-- fmap :: (a -> b) -> (f a -> f b)
--
-- it takes a function a -> b and produces a function (f a -> f b)
-- This is called Lifting a function
--
:t fmap (*2)
-- fmap (*2) :: (Functor f, Num b) => f b -> f b
--
:t fmap (replicate 3)
-- fmap (replicate 3) :: Functor f => f a -> f [a]
--
fmap (++"!") reverse "hello"
-- "olleh!"

-- fmap can be thought of in two ways:
-- a.  As a function that takes a function and a functor value and then maps that
--     function over the functor value
-- b.  As a function that takes a function and lifts that function so it operates
--     on functor values

import Control.Monad.Instances
-- Reminder
instance Functor (Either a) where
    fmap f (Right x) = Right (f x)
    fmap f (Left  x) = Left  x

a = fmap (replicate 3) [1,2,3,4]
b = fmap (replicate 3) (Just 4)
c = fmap (replicate 3) (Right "blah")
d = fmap (replicate 3) Nothing
e = fmap (replicate 3) (Left "foo")
-- load 11_fmap_on_replicate.hs


-- Functor Laws

-- Law 1.
fmap id == id
--
-- This implies that fmap over the functor value doesn't do anything that is
-- hidden.

fmap id (Just 3)
-- Just 3
id (Just 3)
-- Just 3
fmap id [1..5]
-- [1,2,3,4,5]
id [1..5]
-- [1,2,3,4,5]
fmap id []
-- []
id []
-- []
fmap id Nothing
-- Nothing
id Nothing
-- Nothing
--
-- recall the implementation of fmap for Maybe to see why fmap id == id holds
instance Functor Maybe where
    fmap f (Just x) = Just (f x)
    fmap f Nothing  = Nothing

-- Law 2.
fmap (f . g) x == fmap f (fmap g x)
fmap (f . g) x == (fmap f . fmap g) x
--
-- This says that composing two functions then mapping the result function
-- over the functor is the same as mapping one function over a functor then
-- mapping the result over another one
--
-- Using Maybe as an example (Nothing part is trivial due to definition.)
fmap (f . g) (Just x)
Just ((f . g) x)
Just (f (g x))

fmap f (fmap g (Just x))
fmap f (Just (g x))
Just (f (g x))


-- Breaking the Law
data CMaybe a = CNothing | CJust Int a
    deriving (Show, Eq) -- C means counter

-- CNothing
--
-- CJust 0 "haha"
--
:t CNothing
-- CNothing :: CMaybe a
--
:t CJust 0 "haha"
-- CJust 0 "haha" :: CMaybe [Char]
--
:t CJust 100 [1,2,3]
-- CJust 100 [1,2,3] :: Num t => CMaybe [t]

instance Functor CMaybe where
    fmap f CNothing          = CNothing
    fmap f (CJust counter x) = CJust (counter + 1) (f x)
-- Like the definition for Maybe except the additional increment of counter

fmap (++"ha") (CJust 0 "ho")
-- CJust 1 "hoha"
fmap (++"he") (fmap (++"ha") (CJust 0 "ho"))
-- CJust 2 "hohahe"
fmap (++"blah") CNothing
-- CNothing

-- Does this obey functor laws? We only need one counter example
fmap id (CJust 0 "haha")
-- CJust 1 "haha"
id (CJust 0 "haha")
-- CJust 0 "haha"
-- load 11_CMaybe.hs

-- CMaybe is part of the functor type class, but since Law 1 doesn't hold
-- it is not a functor!

-- Having functors obey the two laws means you can reason about the code.
-- Mapping over a functor with do nothing more than map.

-- When making your own functors it might be a good idea to through in a
-- few tests to prove that it behaves properly!



-- Using Applicative Functors
--
-- So far we've only mapped functions that take one parameter. What happens
-- if we map a function that requires two parameters?
--
:t fmap (*) (Just 3)
-- fmap (*) (Just 3) :: Maybe (Int -> Int)
--
-- this would have a value of Just (3 *)
-- i.e. a function wrapped in a just
--
-- to use it however... is a bit messy
fmap (\f -> f 3) $ fmap (*) (Just 3)
-- Just 9
--
-- other examples:
--
:t fmap (++) (Just "hey")
-- fmap (++) (Just "hey") :: Maybe ([Char] -> [Char])
fmap (\f -> f " how're you?") $ fmap (++) (Just "hey")
-- Just "hey how're you?"
--
:t fmap compare (Just 'a')
-- fmap compare (Just 'a') :: Mabye (Char -> Ordering)
fmap (\f -> f 'z') $ fmap compare (Just 'a')
-- Just LT
--
:t fmap compare "A LIST OF CHARS"
-- fmap compare "A LIST OF CHARS" :: [Char -> Ordering]
fmap (\f -> f 'A') $ fmap compare "A LIST OF CHARS"
-- [EQ,LT,GT,GT,GT,GT,LT,GT,GT,LT,GT,GT,EQ,GT,GT]
--
:t fmap (\x y z -> x + y / z) [3,4,5,6]
-- fmap (\x y z -> x + y / z) [3,4,5,6] :: Fractional a => [a -> a -> a]
fmap (\z -> z 2) $ fmap (\y -> y 1) $ fmap (\x y z -> x + y / z) [3,4,5,6]
-- [3.5, 4.5, 5.5, 6.5]


-- With normal functors we can't map Just (3 *) over (Just 5)
-- we can't map functions that are inside functor values over another
-- functor value. This is where applicative functors come in.


-- Say Hello to Applicative

-- the Applicative type class defines two functions:
class (Functor f) => Applicative f where
    pure  :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
-- no default implementation is provided, so have to make your own if you
-- define a class as an applicative functor

-- firstly, if something wants to be Applicative, it has to be an instance of
-- Functor first. I.e. we can use fmap on it.
--
-- pure takes any type a and wraps it inside f. Like putting a value into a
-- default (or pure) context
--
-- <*> is like fmap, but more powerful.
-- It takes a functor value with a function inside it and a functor value
-- then applies the function over the value in second functor


-- Maybe the Applicator Functor
--
-- the applicative instance implementation for Maybe
instance Applicative Maybe where
    pure = Just
    Nothing  <*> _         = Nothing
    (Just f) <*> something = fmap f something

-- pure wraps something in an applicative value, hence Just.
-- There is no function inside Nothing, so we return Nothing.
-- Otherwise we extract f and map f over something with fmap.

-- examples
import Control.Applicative

Just (+3) <*> Just 9
-- Just 12
pure (+3) <*> Just 10
-- Just 13
pure (+3) <*> Just 9
-- Just 12
Just (++"hahah") <*> Nothing
-- Nothing
Nothing <*> Just "woot"
-- Nothing

-- Use "pure" when you are dealing with values in an applicative context,
-- rather than a raw Just.
--
-- These examples could easily have been achieved with mapping unwrapped
-- functions over functors like we did before: fmap (\f -> ... ) something
--
-- With normal functors mapping a function over a functor, you can't really
-- get the result out in any general way. Applicative functors allow you to
-- operate on several functor with a single function


-- The Applicative Style

pure (+) <*> Just 3 <*> Just 5
-- Just 8
pure (+) <*> Just 3 <*> Nothing
-- Nothing
pure (+) <*> Nothing <*> Just 5
-- Nothing

-- <*> is left associative, so
pure (+) <*> Just 3 <*> Just 5
(pure (+) <*> Just 3) <*> Just 5
(Just (+) <*> Just 3) <*> Just 5
Just (3+) <*> Just 5
Just 8
-- applicative functors and applicative style allow us to apply functions that
-- don't expect applicative values and apply to values that are applicative
--
-- since pure f <*> x == fmap f x, we don't really need to wrap the function
-- in the first place, so Control.Applicative also exports <$>

(<$>)  :: (Functor f) => (a -> b) -> f a -> f b
f <$> x = fmap f x

-- now we can instead write
(++) <$> Just "johntra" <*> Just "volta"
-- Just "johntravolta"


-- Lists

-- The list type constructor [] is an applicative functor
instance Applicative [] where
    pure x = [x]
    fs <*> xs = [f x | f <- fs, x <- xs]

-- pure creates a minimal context that can yield a value
-- [] and Nothing do not contain values.
pure "Hey" :: [String]
-- ["Hey"]
pure "Hey" :: Maybe String
-- Just "Hey"
[(*0),(+100),(^2)] <*> [1,2,3]
-- [0,0,0,101,102,103,1,4,9]
[(+),(*)] <*> [1,2] <*> [3,4]
-- [4,5,5,6,3,4,6,8]
(++) <$> ["ha","heh","hmm"] <*> ["?","!","."]
-- ["ha?","ha!","ha.","heh?","heh!","heh.","hmm?","hmm!","hmm."]

-- list could be viewed as non-deterministic computations
-- values like 100 or "what" can be viewed as deterministic: only one result
-- [1,2,3] can be viewed as a computation that can't decide, so gives all the
-- possible results. (+) <$> [1,2,3] <*> [4,5,6] is like adding two
-- non-deterministic computations and getting something back that is even
-- less sure about the result :)

[x*y | x <- [2,5,10], y <- [8,10,11]]
-- [16,20,22,40,50,55,80,100,110]
(*) <$> [2,5,10] <*> [8,10,11]
-- [16,20,22,40,50,55,80,100,110]
filter (>50) $ (*) <$> [2,5,10] <*> [8,10,11]
-- [55,80,100,110]


-- IO Is An Applicative Functor, Too

instance Applicative IO where
    pure    = return
    a <*> b = do f <- a
                 x <- b
                 return (f x)

-- consider
myAction :: IO String
myAction = do a <- getLine
              b <- getLine
              return $ a ++ b

-- or in applicative style
myAction :: IO String
myAction = (++) <$> getLine <*> getLine

-- because myAction is a completely normal IO action, you can do:
main = do
    a <- (++) <$> getLine <*> getLine
    putStrLn $ "The two lines concatenated turn out to be: " ++ a


-- Functions As Applicatives
instance Applicative ((->) r) where
    pure x = (\_ -> x)
    f <*> g = \x -> f x (g x)
-- wrapping a value in pure: the result it yields is that value because of
-- minimal default context. It ignores any parameter given to it and always
-- returns the value
(pure 3) "blah"
-- 3
pure 3 "blah"
-- 3 (because of left associativity

-- The implementation for <*> is interesting so let's see how to use it
:t (+) <$> (+3) <*> (*100)
-- (+) <$> (+3) <*> (*100) :: (Num a) => a -> a
(+) <$> (+3) <*> (*100) $ 5
-- 508
-- To work it out remember for (->) r:
-- fmap f g = (\x -> f (g x))
-- f <*> g  = f x (g x)
(+) <$> (+3) <*> (*100) $ 5
(fmap (+) (+3)) <*> (*100) $ 5
(\x -> (+) ((+3) x)) <*> (*100) $ 5
(\x -> (\y -> (+) ((+3) y)) x ((*100) x)) $ 5
(+) ((+3) 5) ((*100) 5)
(+) (8) (500)
(+) 8 500
-- 508

(\x y -> x + y) <$> (+3) <*> (*100) $ 5
-- 508
(\x y z -> [x,y,z]) <$> (+3) <*> (*2) <*> (/2) $ 5
-- [8.0,10.0,2.5]
-- we create a function that will call the lambda with the eventual results
-- from (+3), (*2) and (/2). The value 5 is given to each of the three
-- functions and the result is put in a list


-- Zip Lists

-- Recall from earlier that [(+3),(*2)] <*> [1,2] Applied both functions to
-- both values to produce [4,5,2,4]. But you could interpret it as:
-- [1+3,2*2].
--
-- Because one type can't have two instances for the same type class, ZipList
-- was introduced. It has one constructor, ZipList, and one field, a list.
instance Applicative ZipList where
    pure x = ZipList (repeat x)
    ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> f x) fs xs)
-- pure has to create an infinite list because <*> applies each function to
-- each value and we don't necessarily know how long the other list will be.
-- It also satisfies the law: pure f <*> xs == fmap f xs
--
-- ZipList type has no Show instance, so have to show with getZipList
getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [100,100,100]
-- [101,102,103]
getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [100,100..]
-- [101,102,103]
getZipList $ max <$> ZipList [1,2,3,4,5,3] <*> ZipList [5,3,1,2]
-- [5,3,3,4]
getZipList $ (,,) <$> ZipList "dog" <*> ZipList "cat" <*> ZipList "rat"
-- [('d','c','r'),('o','a','a'),('g','t','t')]
-- The (,,) function is the same as \x y z -> (x,y,z). And (,) is \x y -> (x,y)

-- The standard library not only has zipWith, but zipWith3, zipWith4, .. up to
-- zipWith7. zipWith3 takes a function with 3 parameters and zips 3 lists
-- together with it, etc.
--
-- using ZipList with an applicative style, we don't need those separate
-- functions and we can go up to an arbitrary amount of lists with a function.


-- Applicative Laws
--
-- like normal functors, applicative functors have some laws.
--
-- The rest are
pure id <*> v = v                             -- Identity
pure (.) <*> u <*> v <*> w = u <*> (v <*> w)  -- Composition
pure f <*> pure x = pure (f x)                -- Homomorphism
u <*> pure y = pure ($ y) <*> u               -- Interchange

-- These laws imply:

fmap f x = pure f <*> x


-- if interested go through these laws with some instances to show they hold.


-- Useful Functions For Applicatives
--
-- Control.Applicative defines liftA2 which has the type
liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = f <$> a <*> b
-- a conveient way of writing applicative style.

-- Although it shows off the power of applicative functors over ordinary
-- functors. Ordinary functors can only map functions over one functor value.
-- Applicative functors can apply a function between several functor values
--
-- Also (a -> b -> c) -> (f a -> f b -> f c) says that liftA2 takes a binary
-- function and promotes it into a function that operates on two applicatives.

-- We can take two applicative values and combine them into one applicative
-- value that has inside it the results of those two applicative values in a
-- list.
fmap (\x -> [x]) (Just 4)
-- Just [4].             fmap (\x -> [x]) Just 4 == [Just 4] :)

-- we have Just 3 and Just [4], how do we get Just [3,4]
liftA2 (:) (Just 3) (Just [4])
-- Just [3,4]
(:) <$> Just 3 <*> Just [4]
-- Just [3,4]

-- Let's try implementing a function that takes a list of applicative values
-- and returns an applicative value that has a list as its result value.
-- We'll call it sequenceA
sequenceA :: Applicative f => [f a] -> f [a]
sequenceA [] = pure []
sequenceA (x:xs) = (:) <$> x <*> sequenceA xs
-- load 11_sequenceA.hs

sequenceA [Just 1, Just 2]
-- Just [1,2]

-- or as a foldr
sequenceA :: Applicative f => [f a] -> f [a]
sequenceA = foldr (liftA2 (:)) (pure [])
-- load 11_sequenceA_foldr.hs

sequenceA [Just 3, Just 2, Just 1]
-- Just [3,2,1]
sequenceA [Just 3, Nothing, Just 1]
-- Nothing
sequenceA [(+3),(+2),(+1)] 3
-- [6,5,4]
sequenceA [[1,2,3],[4,5,6]]
-- [[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]
sequenceA [[1,2,3],[4,5,6],[3,4,4],[]]
-- []

-- When used on Maybe values, sequenceA creates a Maybe value with all the
-- results inside it as a list. If one of the values is Nothing, then the
-- result is also Nothing.
--
-- When used on functions, sequenceA takes a list of functions and returns a
-- function that resturns a list. Above we made a function that takes a
-- number and applies it to each function in the list and returns the list of
-- results.
--
-- When used with [], sequenceA takes a list of lists and returns a list of
-- lists. It creates lists that have all possible combinations of their
-- elements.
sequenceA [[1,2,3],[4,5,6]]
-- [[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]
[[x,y] | x <- [1,2,3], y <- [4,5,6]]
-- [[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]
sequenceA [[1,2],[3,4]]
-- [[1,3],[1,4],[2,3],[2,4]]
[[x,y] | x <- [1,2], y <- [3,4]]
-- [[1,3],[1,4],[2,3],[2,4]]
sequenceA [[1,2],[3,4],[5,6]]
-- [[1,3,5],[1,3,6],[1,4,5],[1,4,6],[2,3,5],[2,3,6],[2,4,5],[2,4,6]]
[[x,y,z] | x <- [1,2], y <- [3,4], z <- [5,6]]
-- [[1,3,5],[1,3,6],[1,4,5],[1,4,6],[2,3,5],[2,3,6],[2,4,5],[2,4,6]]

-- When used with IO actions, sequence is the same thing as sequence. It takes
-- a list of IO actions and returns an IO action that will perform each of
-- those actions and have as its result a list of the results of those IO
-- actions, i.e. [IO a] -> IO [a]
sequenceA [getLine, getLine, getLine]
-- heyh
-- ho
-- woo
-- ["heyh","ho","woo"]

-- Applicative functors allow us to combine different computations by using the
-- applicative style. Using <$> and <*> wen can employ normal functions to
-- operate on any number of applicative functors and take advantages of the
-- semantics of each one.
