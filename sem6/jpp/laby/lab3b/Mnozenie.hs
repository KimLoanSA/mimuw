import           Text.Read


main = do
  putStrLn "Hejka!"

  results <- mapM executeQuestion [(a, b) | a <- [1..10], b <- [1..10]]
  let correct = length $ filter id results

  putStrLn $ concat ["Twój wynik to: ", show correct]


executeQuestion :: (Int, Int) -> IO Bool
executeQuestion (a, b) = do
  putStrLn $ getQuestion (a, b)
  line <- getLine

  let answer = readMaybe line
  let result = checkAnswer (a, b) answer

  putStrLn $ getOutput (a, b) result

  pure result


getQuestion :: (Int, Int) -> String
getQuestion (a, b) = concat [show a, " * ", show b, " ="]


getOutput :: (Int, Int) -> Bool -> String
getOutput (a, b) True  = "git"
getOutput (a, b) False = concat ["Źle (", show a, "*", show b, "=", show (a * b), ")"]


checkAnswer :: (Int, Int) -> Maybe Int -> Bool
checkAnswer (a, b) (Just answer) = a * b == answer
checkAnswer _      _             = False
