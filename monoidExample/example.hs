import Data.Monoid


promptName :: IO(IO())
promptName = do putStrLn "Enter your name:"
                name <- getLine
                return $ putStrLn ("Your name is: " ++ name) 

promptAge :: IO(IO())
promptAge = do putStrLn "Enter your age: "
               age <- getLine
               return $ putStrLn ("Your age is: " ++ age)

promptColor :: IO(IO())
promptColor = do putStrLn "Enter your favorite color: "
                 color <- getLine
                 return $ putStrLn ("Your favorite color is: " ++ color)


foo = do meh <- promptAge <> promptName
         meh

bar = do meh <- promptName <> promptColor
         meh

baz = do meh <- promptColor <> promptAge
         meh


dupInput = (getLine <> getLine) 
dupOutput = (putStrLn <> putStrLn) "neat"
dupMerge = ((<>"meh") <> (<>"neat")) "wee"

promptName' :: IO [String]
promptName' = do putStrLn "Enter your name:"
                 name <- getLine
                 return $ [name]

promptAge' :: IO [String]
promptAge' = do putStrLn "Enter your age: "
                age <- getLine
                return $ [age]

promptColor' :: IO [String]
promptColor' = do putStrLn "Enter your favorite color: "
                  color <- getLine
                  return $ [color]

neat = promptColor' <> promptAge' <> promptName'


meh = promptColor' <> promptColor' <> promptColor'