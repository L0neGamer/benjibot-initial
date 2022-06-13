-- |
-- Module      : Plugins.Netrunner.Type.NrApi
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- The NrApi type.
module Plugins.Netrunner.Type.NrApi where

import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)
import Plugins.Netrunner.Type.BanList (BanList)
import Plugins.Netrunner.Type.Blacklist (Blacklist)
import Plugins.Netrunner.Type.Card (Card)
import Plugins.Netrunner.Type.Cycle (Cycle)
import Plugins.Netrunner.Type.Faction (Faction)
import Plugins.Netrunner.Type.Glossary (Glossary)
import Plugins.Netrunner.Type.Pack (Pack)
import Plugins.Netrunner.Type.Type (Type)

-- | @NrApi@ represents all required Netrunner data collected in one record.
data NrApi = NrApi
  { cards :: [Card],
    imageTemplate :: Text,
    types :: [Type],
    factions :: [Faction],
    cycles :: [Cycle],
    packs :: [Pack],
    banLists :: [BanList],
    cardAliases :: Map Text Text,
    blacklist :: Blacklist,
    glossary :: Glossary
  }
  deriving (Eq, Show, Generic)
