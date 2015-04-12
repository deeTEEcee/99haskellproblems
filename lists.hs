{- Notes:
http://stackoverflow.com/questions/10548170/what-characters-are-permitted-for-haskell-operators
//
-- print $ last [1,2,3]
is the same as
-- print (last [1,2,3])
used for grouping expressions, just a matter of perference

// ???
Monads: use Maybe+Nothing for non-error return cases (need to understand more about this) but then we get ambiguous a0 error

// type vs data keyword (type is for synonyms where data lets you define new types)

-}
import System.Environment
import Data.List

---- Mine

-- Order does not matter
myPlus' :: [a] -> [a] -> [a]
myPlus' xs [] = xs
myPlus' [] ys = ys
myPlus' (x:xs) (y:ys) = x:y:myPlus' xs ys

-- Order matters
(++~) :: [a] -> [a] -> [a]
(++~) [] ys = ys
(++~) (x:xs) ys = x: xs ++~ ys

---- #1-10: Lists
-- #1: get last element in a list
last' :: [a] -> a
last' [] = error "empty array"
last' [x] = x
last' (_:xs) = last' xs

-- #2: (*) Find the last but one element of a list.
butLast' = last . init

butLast'' :: [a] -> a
butLast'' [] = error "No elements"
butLast'' [x] = error "Only one element"
butLast'' x = reverse x !! 1

butLast''' :: [a] -> a
butLast''' [] = error "No elements"
butLast''' [x] = error "Only one element"
butLast''' (x:(_:[])) = x
butLast''' (_:xs) = butLast''' xs

-- #3 element index
eleAt' :: Int -> [a] -> a
eleAt' 0 [x] = x
eleAt' x (_:ys) = eleAt' (x-1) ys

-- #4 length of list
length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

-- #5 reverse list
reverse' :: [a] -> [a]
reverse' list = reverse'' list []
  where
    reverse'' [] reversed = reversed
    reverse'' (x:xs) reversed = reverse'' xs (x:reversed)

-- This one requires understanding foldl and more about how you can reuse functions with only few params used
--reverse''          :: [a] -> [a]
--reverse''          =  foldl (flip (:)) []

-- #6 is a palindrome
isPali :: (Eq a) => [a] -> Bool
isPali list = list == reverse list

isPali' :: (Eq a) => [a] -> Bool
isPali' [] = True
isPali' [x] = True
isPali' list = if (head list == last list) then isPali' (tail (init list)) else False

-- #7 flatten list (not really sure what concat map is...)
--data NestedList a = Elem a | [NestedList a]

--flatten' :: NestedList a -> [a]
--flatten'

-- #8　Eliminate consecutive duplicates of list elements.
compress' :: Eq a => [a] -> [a]
compress' = map head . group

compress'' :: Eq a => [a] -> [a]
compress'' [] = []
compress'' (x:xs) = x:(compress'' $ dropWhile (==x) xs)

-- Remove all duplicates (keep the last)
rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs)   | x `elem` xs   = rmdups xs
                | otherwise     = x : rmdups xs

-- Remove all duplicates (keep the first)
rmdups' :: Eq a => [a] -> [a]
rmdups' = rdHelper []
    where rdHelper seen [] = seen
          rdHelper seen (x:xs)
              | x `elem` seen = rdHelper seen xs
              | otherwise = rdHelper (seen ++ [x]) xs

-- #9 Put duplicates into sublists

-- # requires duplicates in the same order
pack' [] = []
pack' list@(x:_) =
  let (first,rest) = span (==x) list
  in first:pack' rest

--pack'' :: [a] -> [[a]]
--pack''


-- #10 length-encoding of a list
encode' :: Eq a => [a] -> [(Int, a)]
encode' [] = []
encode' list@(x:_) =
  let (first,rest) = span (==x) list
  in (length first, x):encode' rest

-- #11 length-encoding modified (it can be a multiple or a single)
data Item a = Single a | Multiple Int a deriving Show

encodeMod :: Eq a => [a] -> [Item a]
encodeMod [] = []
encodeMod list@(x:_) =
  let (first,rest) = span (==x) list in
  let len = length first in
    if len == 1
      then (Single x):encodeMod rest
      else (Multiple len x):encodeMod rest

-- #12: decode and undo #11
decodeMod :: [Item a] -> [a]
decodeMod [] = []
decodeMod ((Single x):xs) = [x] ++ decodeMod xs
decodeMod ((Multiple n x):xs) = replicate n x ++ decodeMod xs

decodeMod' :: [Item a] -> [a]
decodeMod' = concatMap helper
  where helper (Single x) = [x]
        helper (Multiple n x) = replicate n x

  --for #10: decode :: [Item a] -> [a]
decode :: [(Int, a)] -> [a]
decode = concatMap (uncurry replicate)

-- #13:

main :: IO ()
main = do
  putStrLn ("99 Haskell Problems: Lists")
  print $ decodeMod' $ encodeMod [1,1,1,2,4,2,2,3,3,3]
  print $ decodeMod' $ encodeMod [1,2,3,2,1,3,3]
  print $ decodeMod' $ encodeMod "aaaazzzaaabbbb"


