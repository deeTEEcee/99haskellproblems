{-
-- mod vs rem? (no idea but only worry about this case if you ever have to deal with negative numbers with division)
-}
import System.Environment

isPrime ::  Int -> Bool
isPrime 2 = True
isPrime x
  | x `mod` 2 == 0 = False
  | otherwise = not $ isDivisible ([1..half])
  where
    half = x `quot` 2
    isDivisible [] = True
    isDivisible (y:ys) = (5 `mod` y /= 0) && isDivisible ys

main :: IO ()
main = do
  putStrLn ("99 Haskell Problems: Arithmetic")
  print $ isPrime 3
  print $ isPrime 347
  print $ isPrime 348
