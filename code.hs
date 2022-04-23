module Code where

factorial :: Int -> Int
factorial 0 = 1
factorial x = x * factorial (x - 1)

fib :: Int -> Int
fib 1 = 1
fib 2 = 1
fib n = fib (n - 1) + fib (n - 2)

solve 
  :: Int -- number of discs
  -> Peg -- from
  -> Peg
  -> Peg -- to
  -> [Move]
-- target: move all discs from A to C
solve 1 x y z = [(x, z)]
solve n x y z = 
  -- Move all of the discs except the 
  -- last one from peg a to peg b
  solve (n - 1) x z y
  -- Move the last disc 
  -- from peg a to peg c
  ++ [(x, z)]
  -- Move all of the 
  -- discs on peg b to peg c
  ++ solve (n - 1) y x z

data Peg = PegA | PegB | PegC
  deriving (Show)
type Move = (Peg, Peg)

my_map :: (a -> b) -> [a] -> [b]
my_map f [] = []
my_map f (x:xs) = f x : map f xs

-- String = [Char]
my_replicate :: Int -> a -> [a]
--my_replicate :: Int -> Char -> [Char]
my_replicate 0 _ = []
my_replicate n c = c : replicate (n - 1) c

-- String = [Char]
-- filter (\x -> x > 3) [5, 4, 3, 6, 7, 8]
-- = [5,4,6,7,8]
-- filter odd [5, 4, 3, 6, 7, 8]
-- = [5,3,7]
my_filter :: (a -> Bool) -> [a] -> [a]
my_filter _ [] = []
my_filter f (x:xs) =
  if f x
    then x : filter f xs
    else filter f xs
quicksort :: [Int] -> [Int]
quicksort [] = []    
quicksort (x:xs) =     
  let small = quicksort (filter (\a -> a <= x) xs)  
      big = quicksort (filter (\a -> a > x) xs)   
  in  small ++ [x] ++ big
isPrime :: Int -> Int -> Bool
isPrime d n = 
  if d >= n - 1
    then True
    else ((mod n d) /= 0) && (isPrime (d + 1) n)

primesUnder100 = filter (\x -> isPrime 2 x) [1..100]
sums = [x + y | x <- primesUnder100, y <- primesUnder100]

