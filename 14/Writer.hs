module Writer where

newtype Writer w a = Writer  { runWriter :: (a, w) }

instance Functor (Writer w) where
  fmap f (Writer (x, v)) = Writer (f x, v)
  
instance Monoid w => Applicative (Writer w) where
  pure x = Writer (x, mempty)
  Writer (f, v) <*> Writer (x, v') = Writer (f x, v `mappend` v')

instance Monoid w => Monad (Writer w) where
  return x = Writer (x, mempty)
  Writer (x, v) >>= f = let Writer (y, v') = f x
                        in Writer (y, v `mappend` v')
