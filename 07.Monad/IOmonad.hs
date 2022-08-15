-- IO
-- getLine :: IO String
-- putStrLn :: String -> IO ()   -- () is also called Unit
-- (>>=) :: IO String -> (String -> IO b) -> IO b
--                        sort of callback function in javascript 
-- in the following example the callback function is putStrLn

main = do
  getLine >>= (\name -> putStrLn name)
  getLine >>= putStrLn 


-- putStrLn :: IO a -> (a -> IO b) -> IO b
main2 =
  getLine >>= (\s -> putStrLn ("Your name is "++s))

main3 =
  getLine >>= (\s -> getLine >>= (\t -> putStrLn ("Your name is "++s++t)))
--same as
main31 =
  putStrLn "Enter your first name" >>= \_ ->
  getLine >>= \s -> 
  putStrLn "Enter your last name" >>= \_ ->
  getLine >>= \t -> 
  let greeting = "Hello, "++s++" "++t in putStrLn greeting 


getInteger :: IO Int 
getInteger = getLine >>= \s -> return (read s)

add :: IO ()
add = 
  getInteger >>= \a ->
  getInteger >>= \b ->
  print (a+b)