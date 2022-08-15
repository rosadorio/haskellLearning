main = do
  putStrLn "Welcome To Batch 64 Tic-TacToe Game!"
  putStr "Write your name Player 1: "   --don't add LF/CR
  p1name <- getLine
  p1token <- validToken 
  putStr "Write your name Player 2: "   
  p2name <- getLine
  putStrLn ("Player 1 : " ++ p1name ++ " token : " ++ p1token )
  putStrLn ("Player 2 : " ++ p2name ++ " token : " ++ tokenSelector p1token)
  

tokenSelector :: String -> String
tokenSelector token
 | token == "X"  = "O"
 | token == "O"  = "X"
 | otherwise     = error "Wrong token!"

validToken :: IO String
validToken = do
              putStr "Choose your piece (X or O): "
              input <- getLine
              if input == "X" || input == "O"
              then return input
              else validToken

