import Data.Char

--------------------
sum100 = sum [x ^ 2 | x <- [1 .. 100]]

--------------------
replicate1 n a = [a | _ <- [1 .. n]]

--------------------
pyths n
  = [(x, y, z) | x <- [1 .. n], y <- [1 .. n], z <- [1..n],
    x ^ 2 + y ^ 2 == z ^ 2]

--------------------
factors n = [i | i <- [1 .. n], n `mod` i == 0]
-- perfects n = [i | i <- [1 .. n], sum (factors i) == i * 2]
perfects n = [x | x <- [1 .. n], isPerfect x]
  where isPerfect num = sum (init(factors num)) == num

--------------------
find k t = [v | (k', v) <- t, k == k']
positions x xs = find x (zip xs [0..n])
  where n = length xs - 1

--------------------
scalarproduct xs ys = sum [x * y | (x, y) <- xs `zip` ys]

--------------------
let2int :: Char -> Char -> Int
let2int c a = ord c - ord a

int2let :: Int -> Char -> Char
int2let n a = chr (ord a + n)

shift :: Int -> Char -> Char
shift n c
  | isLower c = int2let ((let2int c 'a' + n) `mod` 26) 'a'
  | isUpper c = int2let ((let2int c 'A' + n) `mod` 26) 'A'
  | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

--------------------
riffle xs ys = concat [[x, y] | (x, y) <- xs `zip` ys]

--------------------
divides x y = x `mod` y == 0
divisors x = [d | d <- [1 .. x], x `divides` d]
