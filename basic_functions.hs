sayHello :: String -> IO ()
sayHello x = putStrLn ("Hello, " ++ x ++ "!")

triple x = x * 3

-- lambdas! this expression
-- (\f -> (1, 2 + f)) 2
-- reduces to WHNF (weak head normal form)
-- (1, 2 + 2)
-- complete normal form isn't evaluated until necessary
-- (1, 4)

circle_area x = pi * (x * x)
