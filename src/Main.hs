module Main where


import System.Environment
import Commands


data Command = Help
             | List
             | Freeze
             | Install
             | Uninstall
             | Error

instance Show Command where
    show Help = "Help"
    show List = "List"
    show Freeze = "Freeze"
    show Install = "Install"
    show Uninstall = "Uninstall"
    show Error = "Error"

getCommand :: String -> Command
getCommand "help" = Help
getCommand "list" = List
getCommand "freeze" = Freeze
getCommand "install" = Install
getCommand "uninstall" = Uninstall
getCommand _ = Error

dispatch :: Command -> [String] -> IO ()
dispatch Help = help
dispatch List = list
dispatch Freeze = freeze
dispatch Install = install
dispatch Uninstall = uninstall
dispatch _ = errorCmd

main :: IO ()
main = do
    (name:args) <- getArgs
    dispatch (getCommand name) args
