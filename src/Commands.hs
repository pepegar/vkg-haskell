{-# LANGUAGE OverloadedStrings #-}
module Commands
( help
, list
, freeze
, install
, uninstall
, errorCmd) where

import System.Directory (getHomeDirectory, getDirectoryContents)
import Control.Monad (mzero)
import Control.Applicative ((<$>), (<*>))
import Data.Aeson 
import Data.Aeson.Encode.Pretty
import Data.Text (Text, pack)


-- TYPES && INSTANCES

newtype PluginList = PluginList{pluginList :: [Plugin]}

instance FromJSON PluginList where
    parseJSON (Object o) = PluginList <$> o .: "plugins"
    parseJSON _ = mzero

instance ToJSON PluginList where
    toJSON (PluginList pluginList) = object ["plugins" .= pluginList]

data Plugin = Plugin
    { repository :: Text
    , branch :: Text
    } deriving Show

instance FromJSON Plugin where
    parseJSON (Object v) = Plugin <$>
                           v .: "repository" <*>
                           v .: "branch"
    parseJSON _ = mzero

instance ToJSON Plugin where
    toJSON (Plugin repository branch) = object ["repository" .= repository, "branch" .= branch]


-- HELPER FUNCTIONS


deleteDotAndDotDot :: [String] -> [String]
deleteDotAndDotDot strings = filter (\p -> p /= "." && p /= "..") strings

getInstalledPlugins :: IO [String]
getInstalledPlugins = do
    home <- getHomeDirectory
    getDirectoryContents $ home ++ "/.vim/bundle/"


toPlugin :: String -> Plugin
toPlugin path = Plugin{repository = pack path, branch = "master"}


getJsonString :: [String] -> String
getJsonString pluginPaths = let
    plugins = PluginList { pluginList = map toPlugin pluginPaths }
    in show $ encodePretty plugins
    


-- COMMANDS

help :: [String] -> IO ()
help _ = putStrLn "help"


-- list all plugins in the plugin
list :: [String] -> IO ()
list _ = do
    plugins <- getInstalledPlugins
    mapM_ putStrLn
        $ deleteDotAndDotDot plugins


freeze :: [String] -> IO ()
freeze _ = do
    plugins <- getInstalledPlugins
    putStrLn $ read $ getJsonString $ deleteDotAndDotDot plugins


install :: [String] -> IO ()
install _ = putStrLn "install"


uninstall :: [String] -> IO ()
uninstall _ = putStrLn "uninstall"

errorCmd :: [String] -> IO ()
errorCmd _ = putStrLn "errorCmd"
