{-# LANGUAGE BangPatterns #-}

sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum' xs

overflow' = sum' [1..100000000]

sum'' :: Int -> [Int] -> Int
sum'' acc [] = acc
sum'' acc (x:xs) = sum'' (acc+x) xs

-- Eats virtual memory with thunks until dies
-- (believe me or try yourselves)

overflow'' = sum'' 0 [1..100000000]

-- Forcing evaluation:

-- Using seq

sum''' :: Int -> [Int] -> Int
sum''' acc [] = acc
sum''' acc (x:xs) = let s = acc + x
                    in s `seq` sum''' s xs

overflow''' = sum''' 0 [1..100000000]

-- Bang Patterns (needs extension)

sum'''' :: Int -> [Int] -> Int
sum'''' acc [] = acc
sum'''' !acc (x:xs) = sum'''' (acc + x) xs

overflow'''' = sum'''' 0 [1..100000000]
