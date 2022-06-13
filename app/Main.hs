module Main where

import Lib
import Tablebot.Internal.Types
import Data.Default
import Tablebot.Plugins.Ping
import Plugins.Netrunner

main :: IO ()
main = runTablebotWithEnv [pingPlugin, netrunnerPlugin] def
