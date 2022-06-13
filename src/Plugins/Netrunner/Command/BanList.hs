{-# LANGUAGE DuplicateRecordFields #-}

-- |
-- Module      : Plugins.Netrunner.Netrunner
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- Backend for the banHistory and banList commands.
module Plugins.Netrunner.Command.BanList (nrBanList) where

import Control.Monad.Trans.Reader (ask)
import Data.Text (Text, toLower, unpack)
import Discord.Types
import Plugins.Netrunner.Type.BanList (BanList (name))
import Plugins.Netrunner.Type.NrApi (NrApi (..))
import Plugins.Netrunner.Utility.BanList
import Plugins.Netrunner.Utility.Print (embedBanList, embedBanLists)
import Tablebot.Utility hiding (name)
import Tablebot.Utility.Search (FuzzyCosts (..), closestValueWithCosts)
import Tablebot.Utility.SmartParser (PComm (parseComm), RestOfInput (ROI))

-- | @nrBanList@ is a command listing all cards affected by a banlist.
nrBanList :: EnvCommand NrApi
nrBanList = Command "banlist" (parseComm banListComm) []
  where
    banListComm ::
      Either () (RestOfInput Text) ->
      Message ->
      EnvDatabaseDiscord NrApi ()
    banListComm (Left ()) = embedBanLists
    banListComm (Right (ROI q)) = sendEmbed q
    sendEmbed :: Text -> Message -> EnvDatabaseDiscord NrApi ()
    sendEmbed query m = do
      api <- ask
      embedBanList (queryBanList api query) m

-- | @queryBanList@ matches the input to the banlist with the closest name.
queryBanList :: NrApi -> Text -> BanList
queryBanList api query =
  let bls = ("active", activeBanList api) : ("latest", latestBanList api) : (zip (map (unpack . toLower . name) $ banLists api) $ banLists api)
   in closestValueWithCosts editCosts bls $ unpack $ toLower query
  where
    editCosts =
      FuzzyCosts
        { deletion = 100,
          insertion = 1,
          substitution = 100,
          transposition = 100
        }
