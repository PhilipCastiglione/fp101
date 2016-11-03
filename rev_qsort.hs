rqsort1 [] = []
rqsort1 (x : xs) = rqsort1 larger ++ [x] ++ rqsort1 smaller
  where smaller = [a | a <- xs, a <= x]
        larger = [b | b <- xs, b > x]

rqsort2 [] = []
rqsort2 xs = x : rqsort2 larger ++ rqsort2 smaller
  where x = maximum xs
        smaller = [a | a <- xs, a < x]
        larger = [b | b <- xs, b >= x]
