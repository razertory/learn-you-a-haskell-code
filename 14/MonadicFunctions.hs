module MonadicFunctions where

import Control.Applicative
import Control.Monad
import Control.Monad.State

mapM_' :: (Monad m, Foldable t) => (a -> m b) -> t a -> m ()
mapM_' f = foldr (\a mb -> f a >> mb) (return ())

foldM' :: (Monad m, Foldable t) => (b -> a -> m b) -> b -> t a -> m b
foldM' f z = foldl (\mb a -> mb >>= (\b -> f b a)) (return z)

filterM' :: (Applicative m) => (a -> m Bool) -> [a] -> m [a]
filterM' p = foldr (\x -> liftA2 (\px -> if px then (x:) else id) (p x)) (pure [])

{-
filterM' :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
filterM' p [] = return []
filterM' p (x:xs) = p x >>= (\px -> if px
                                      then liftM2 (:) (return x) (filterM' p xs)
                                      else filterM' p xs)

filterM' p [] = return []
filterM' p (x:xs) = p x >>= (\px -> if px
                                       then filterM' p xs >>= (\xs -> return (x:xs))
                                       else filterM' p xs)

filterM' p [] = return []
filterM' p (x:xs) = p x >>= (\px -> if px
                                      then filterM' p xs >>= (\xs -> return (x:xs))
                                      else filterM' p xs >>= (\xs -> return (xs)))

filterM' p [] = return []
filterM' p (x:xs) = p x >>= (\px -> filterM' p xs >>= return . (if px then (x:) else id))

filterM' p = foldr (\x r -> p x >>= (\px -> r >>= return . (if px then (x:) else id))) (return [])

filterM' p = foldr (\x r -> p x >>= (\px -> fmap (if px then (x:) else id) r)) (return [])

filterM' p = foldr (\x r -> liftM2 (\px r -> (if px then (x:) else id) r) (p x) r) (return [])

From here on we only need an Applicative:

filterM' :: (Applicative m) => (a -> m Bool) -> [a] -> m [a]
filterM' p = foldr (\x r -> liftA2 (\px r -> (if px then (x:) else id) r) (p x) r) (pure [])

filterM' p = foldr (\x r -> liftA2 (\px -> if px then (x:) else id) (p x) r) (pure [])

filterM' p = foldr (\x -> liftA2 (\px -> if px then (x:) else id) (p x)) (pure [])
-}

alternate :: a -> State Bool Bool
alternate _ = state $ \b -> (b, not b)

oneInOneOut :: Bool -> [a] -> [a]
oneInOneOut b as = fst $ runState $ filterM' alternate as b
