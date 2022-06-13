-- |
-- Module      : Sahasrara.Plugins.Netrunner.Type.Cycle
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- The Cycle and Cycles types.
module Sahasrara.Plugins.Netrunner.Type.Cycle where

import Data.Aeson (FromJSON, parseJSON, withObject, (.:))
import Data.Text (Text)
import GHC.Generics (Generic)

-- | @Cycle@ represents a single cycle of packs in the NetrunnerDB API.
data Cycle = Cycle
  { code :: !Text,
    name :: !Text,
    position :: !Int,
    size :: !Int,
    rotated :: !Bool
  }
  deriving (Eq, Show, Generic)

instance FromJSON Cycle where
  parseJSON = withObject "Cycle" $ \o ->
    Cycle <$> o .: "code"
      <*> o .: "name"
      <*> o .: "position"
      <*> o .: "size"
      <*> o .: "rotated"
