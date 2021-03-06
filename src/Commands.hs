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
    toJSON (PluginList pl) = object ["plugins" .= pl]

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
    toJSON (Plugin r b) = object ["repository" .= r, "branch" .= b]


-- HELPER FUNCTIONS

deleteDotAndDotDot :: [String] -> [String]
deleteDotAndDotDot = filter (\p -> p /= "." && p /= "..")

getInstalledPlugins :: IO [String]
getInstalledPlugins = do
    home <- getHomeDirectory
    getDirectoryContents $ home ++ "/.vim/bundle/"


toPlugin :: String -> Plugin
toPlugin path = Plugin{repository = pack path, branch = "master"}


data PrintingMode = Pretty
          | Normal


getJsonString :: PrintingMode -> [String] -> String
getJsonString Pretty pluginPaths = let
    plugins = PluginList { pluginList = map toPlugin pluginPaths }
    in show $ encodePretty plugins
getJsonString Normal pluginPaths = let
    plugins = PluginList { pluginList = map toPlugin pluginPaths }
    in show $ encode plugins


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

getPrintingMode :: String -> PrintingMode
getPrintingMode "--pretty" = Pretty
getPrintingMode _ = Normal

freeze :: [String] -> IO ()
freeze [] = do
    githubRepos <- getGithubRepos
    putStrLn $ read $ getJsonString Normal githubRepos
freeze params = do
    let mode = getPrintingMode $ params !! 0
    githubRepos <- getGithubRepos
    putStrLn $ read $ getJsonString mode githubRepos

getGithubRepos :: IO [String]
getGithubRepos = do
    home <- getHomeDirectory
    pluginNames <- getInstalledPlugins
    sequenceA $ map (getRepoUrl home) pluginNames


install :: [String] -> IO ()
install _ = putStrLn "install"


uninstall :: [String] -> IO ()
uninstall _ = putStrLn "uninstall"

errorCmd :: [String] -> IO ()
errorCmd _ = putStrLn "errorCmd"
