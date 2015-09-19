module Main where

greet :: String -> String
greet name = "Hello " ++ name ++ "!"

main :: IO ()
main = putStrLn "hello world"
