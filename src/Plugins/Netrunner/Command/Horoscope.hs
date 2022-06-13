{-# LANGUAGE DuplicateRecordFields #-}

-- |
-- Module      : Plugins.Netrunner.Netrunner
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- Backend for the horoscope command.
module Plugins.Netrunner.Command.Horoscope (nrHoroscope) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader (ask)
import Data.Maybe (mapMaybe)
import Data.Text (Text, isInfixOf)
import Data.Text.ICU.Replace (replaceAll)
import Data.Time.Calendar
import Data.Time.Clock
import Discord.Types
import Tablebot.Internal.Handler.Command ()
import Plugins.Netrunner.Type.Blacklist (Blacklist (..))
import Plugins.Netrunner.Type.Card (Card (flavour, title))
import Plugins.Netrunner.Type.NrApi (NrApi (..))
import Plugins.Netrunner.Utility.Embed
import Plugins.Netrunner.Utility.Format (formatText)
import Tablebot.Utility
import Tablebot.Utility.Discord (sendEmbedMessage)
import Tablebot.Utility.Embed (addColour)
import Tablebot.Utility.Random (chooseOneSeeded)
import Tablebot.Utility.Types ()
import Text.RawString.QQ (r)

-- | @nrHoroscope@ gets a random piece of flavour text from the card pool,
-- seeded by the current date.
nrHoroscope :: EnvCommand NrApi
nrHoroscope = Command "horoscope" horoscopePars []
  where
    horoscopePars :: Parser (Message -> EnvDatabaseDiscord NrApi ())
    horoscopePars = return $ \m -> do
      api <- ask
      let fs = filterFlavours (blacklist api) (cards api)
      seed <- liftIO $ getCurrentTime >>= return . fromIntegral . toModifiedJulianDay . utctDay
      f <- liftIO $ chooseOneSeeded seed fs
      f' <- formatText f
      sendEmbedMessage m "" $ addColour (RGB 170 141 216) $ embedText ":crystal_ball: Horoscope :crystal_ball:" $ replaceAll [r|"(.*?)"[.\S\s]*|] "$1" f'
    filterFlavours :: Blacklist -> [Card] -> [Text]
    filterFlavours Blacklist {badSubstrings = badSubstrings, badCards = badCards} cards =
      let flavoured = filter ((Nothing /=) . flavour) cards
          withoutBadCards = filter (\c -> all (\b -> Just b /= title c) badCards) flavoured
       in filter (\c -> not $ any (`isInfixOf` c) badSubstrings) $ mapMaybe flavour withoutBadCards -- Without bad substrings
