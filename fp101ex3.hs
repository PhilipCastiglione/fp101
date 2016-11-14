-- import Prelude hiding ((||))
-- import Prelude hiding ((&&))

-- *****0
-- halve1 xs = (take n xs, drop n xs)
--   where n = length xs / 2
-- int / 2 is an error, produces fraction?

halve2 xs = splitAt (length xs `div` 2) xs

halve3 xs = (take (n `div` 2) xs, drop (n `div` 2) xs)
  where n = length xs

-- halve4 xs = splitAt (length xs `div` 2)
-- splitAt needs a second argument

-- halve5 xs = (take n xs, drop (n + 1) xs)
--   where n = length xs `div` 2
-- missing an item from the list due to n + 1

halve6 xs = splitAt (div (length xs) 2) xs

-- halve7 xs = splitAt (length xs / 2) xs
-- int / 2 is an error, produces fraction?

halve8 xs = (take n xs, drop n xs)
  where n = length xs `div` 2

-- *****1
safetail1 xs = if null xs then [] else tail xs

safetail2 [] = []
safetail2 (_ : xs) = xs

-- safetail3 (_ : xs)
--   | null xs = []
--   | otherwise = tail xs
-- operating only on the tail already

safetail4 xs
  | null xs = []
  | otherwise = tail xs

-- safetail5 xs = tail xs
-- safetail5 [] = []
-- attempts to tail [] before empty pattern matches

safetail6 [] = []
safetail6 xs = tail xs

-- safetail7 [x] = [x]
-- safetail7 (_ : xs) = xs
-- doesn't handle []

safetail8
  = \ xs ->
      case xs of
          [] -> []
          (_ : xs) -> xs

-- *****2
-- ez

-- *****3
-- ez

-- *****4
mult x y z = x * y * z
