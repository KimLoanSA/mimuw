module HelloWorld where

helloWorld :: String
helloWorld = "Hello World!"

greeting :: String -> String
greeting x = "Hello, " ++ x ++ "! How are you?"

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n-1)

fact2 n = product [1..n]
