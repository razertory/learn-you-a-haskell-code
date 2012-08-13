import Control.Monad.Writer

gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
    | b == 0 = do
        tell ["Finished with " ++ show a]
        return a
    | otherwise = do
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
        gcd' b (a `mod` b)

-- fst $ runWriter (gcd' 8 3)
-- 1
-- mapM_ putStrLn $ snd $ runWriter (gcd' 8 3)
-- 8 mod 3 = 2
-- 3 mod 2 = 1
-- 2 mod 1 = 0
-- Finished with 1