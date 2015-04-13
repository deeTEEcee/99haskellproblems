{-
-- mod vs rem? (no idea but only worry about this case if you ever have to deal with negative numbers with division)
-}
import System.Environment
import Data.List
import Data.Maybe

isPrime ::  Int -> Bool
isPrime 2 = True
isPrime x
  | x `rem` 2 == 0 = False
  | otherwise = isNotDivisibleBy [3..half]
  where
    half = x `quot` 2
    isNotDivisibleBy [] = True
    isNotDivisibleBy (y:ys) = (x `mod` y /= 0) && isNotDivisibleBy ys

gcd' :: Int -> Int -> Int
gcd' a b = if b > a
  then gcd' b a
  else if (a `rem` b) == 0
    then b
    else gcd' b (a `rem` b)

coprime' :: Int -> Int -> Bool
coprime' a b = (gcd' a b) == 1

totient :: Int -> Int
totient n = length $ filter (coprime' n) [1..n-1]

primeFactors :: Int -> [Int]
primeFactors n = helper n (potentialPrimeFactors n) []
  where
    potentialPrimeFactors n = filter (\x -> n `mod` x == 0 && isPrime x) [3..n-1]
    helper n2 factors saved
      | n2 == 1 = saved
      | otherwise = helper (n2 `quot` div) factors (div:saved)
      where
        div = fromJust (find (\x -> n2 `mod` x == 0) factors)

main :: IO ()
main = do
  putStrLn ("99 Haskell Problems: Arithmetic")
  print $ isPrime 5
  print $ isPrime 3
  print $ isPrime 7
  print $ isPrime 105
  print $ primeFactors 315
  --print $ gcd' 38 8
