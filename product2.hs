module Product where

import Test.QuickCheck

x :: [Int] -> [Int] -> Int;
x [] [] = 1;
x [] [n] = n;
x [] (n:ns) = n * x [] ns; 
x [n] [] = n; 
x (n:ns) [] = n * x ns []; 
x [n] [m] = n * m; 
x (n:ns) (m:ms) = n * m * x ns ms;

primes = filterPrime [2..]
  where filterPrime (p:xs) =
          p : filterPrime [x | x <- xs, x `mod` p /= 0]

data Nat = Zero | Succ Nat
    deriving Show

power :: Float -> Nat -> Float
power x Zero = 1.0
power x (Succ n) = x * power x n

p :: [(Int,Int)] -> Bool
p [] = False
p xs = sum [a^2 | (a,b) <- xs] > product [b | (a,b) <- xs, odd b]

q :: [(Int,Int)] -> Bool
q [] = False
q xs = q1 xs > q2 xs

q1 :: [(Int, Int)] -> Int
q1 [] = 0
q1 ((x,_):xs) = x^2 + q1 xs

q2 :: [(Int, Int)] -> Int
q2 [] = 1
q2 ((_,y):xs)
  | odd y = y * q2 xs
  | otherwise = q2 xs

prop_pq :: [(Int, Int)] -> Bool
prop_pq xs = p xs == q xs

r :: [(Int,Int)] -> Bool
r xs = (foldr (+) 0 $ map (fst) xs) > (product . filter (odd) $ map (snd) xs )

done :: IO ()
done = return() 

-- sq :: IO () -> IO () -> IO ()
-- sq cm1 cm2 = putChar cm1 >> putStr cm2
main :: IO()
main = putStr' "?!\n"

putStr' :: String -> IO()
putStr' [] = done
putStr' (x:xs) = putChar x >> putStr' xs

tp :: [Int] -> [(Int,Int)]
--tp xs = [(a,b) |(a,b) <- zip [a |(a,b) <- zip xs [0..], even b] [a |(a,b) <- zip xs [0..], odd b]]
tp xs = [(xs!!(2*i),xs!!(2*i+1))|i<-[0..length xs `div` 2 - 1]]
--tp xs = zip [x|(x,i)<-zip xs [0..],even i] [x|(x,i)<-zip xs [0..],odd i]
--tp xs = [ (a,b) | ((a,b), i) <- zip (zip xs (tail xs)) [0..], even i ]

