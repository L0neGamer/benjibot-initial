-- |
-- Module      : Plugins.Netrunner.Plugin
-- Description : A plugin for finding Netrunner cards from Discord.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- Commands for interfacing with NetrunnerDB.
module Plugins.Netrunner.Plugin (netrunnerPlugin) where

import Control.Monad.IO.Class
import Tablebot.Internal.Handler.Command ()
import Plugins.Netrunner.Command.BanList
import Plugins.Netrunner.Command.Find (nrInline, nrInlineBanHistory, nrInlineFlavour, nrInlineImg)
import Plugins.Netrunner.Command.Glossary (nrGlossary)
import Plugins.Netrunner.Command.Help (helpPageRoots)
import Plugins.Netrunner.Command.Horoscope (nrHoroscope)
import Plugins.Netrunner.Command.Ram (nrRam)
import Plugins.Netrunner.Command.Search (nrRandom, nrSearch)
import Plugins.Netrunner.Command.Sets (nrCycles, nrSets)
import Plugins.Netrunner.Type.NrApi (NrApi (..))
import Plugins.Netrunner.Utility.NrApi (getNrApi)
import Tablebot.Utility
import Tablebot.Utility.Types ()

-- | @netrunnerStartUp@ loads the NetrunnerDB api once at start up
netrunnerStartUp :: StartUp NrApi
netrunnerStartUp = StartUp $ liftIO getNrApi

-- | @welcomePlugin@ assembles these commands into a plugin.
netrunnerPlugin :: EnvPlugin NrApi
netrunnerPlugin =
  (envPlug "netrunner" netrunnerStartUp)
    { commands =
        [ nrSearch,
          nrRandom,
          nrSets,
          nrCycles,
          nrBanList,
          commandAlias "bl" nrBanList,
          commandAlias "mwl" nrBanList,
          nrGlossary,
          commandAlias "g" nrGlossary,
          nrRam,
          nrHoroscope
        ],
      inlineCommands = [nrInline, nrInlineImg, nrInlineFlavour, nrInlineBanHistory],
      helpPages = helpPageRoots
    }
