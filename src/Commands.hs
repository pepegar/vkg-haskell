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
import System.Process
import System.IO


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
deleteDotAndDotDot = filter (\p -> p /= "." && p /= "..")

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


getRepoUrl :: String -> String -> IO String
getRepoUrl basePath name = do
    (_, Just handle, _, _) <- createProcess (proc "git" ["config", "--get", "remote.origin.url"]) {
        cwd = Just $ basePath ++ "/.vim/bundle/" ++ name,
        std_out = CreatePipe}
    hGetContents handle


-- COMMANDS

help :: [String] -> IO ()
help _ = putStrLn "help"


-- list all plugins in the bundle directory
list :: [String] -> IO ()
list _ = do
    plugins <- getInstalledPlugins
    mapM_ putStrLn
        $ deleteDotAndDotDot plugins

freeze :: [String] -> IO ()
freeze _ = do
    home <- getHomeDirectory
    pluginNames <- getInstalledPlugins
    githubRepos <- sequenceA $ map (getRepoUrl home) pluginNames
    putStrLn $ read $ getJsonString githubRepos


install :: [String] -> IO ()
install _ = putStrLn "install"


uninstall :: [String] -> IO ()
uninstall _ = putStrLn "uninstall"

errorCmd :: [String] -> IO ()
errorCmd _ = putStrLn "errorCmd"
