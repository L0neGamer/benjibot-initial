-- |
-- Module      : Sahasrara.Plugins.Netrunner.Utility.Search
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- Converts plaintext to Netrunner search queries and vice versa.
module Sahasrara.Plugins.Netrunner.Utility.Search
  ( Query,
    QueryComp,
    searchCards,
    fixSearch,
    pairsToQuery,
    pairsToNrdb,
    shorthands,
  )
where

import Data.List (findIndex, nubBy)
import Data.Map (Map, fromList)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text, intercalate, isInfixOf, pack, replace, toLower, unpack, unwords)
import Data.Text.Read (decimal)
import Sahasrara.Plugins.Netrunner.Type.BanList (BanList)
import qualified Sahasrara.Plugins.Netrunner.Type.BanList as BanList
import Sahasrara.Plugins.Netrunner.Type.Card as Card
import qualified Sahasrara.Plugins.Netrunner.Type.Cycle as Cycle
import qualified Sahasrara.Plugins.Netrunner.Type.Faction as Faction
import Sahasrara.Plugins.Netrunner.Type.NrApi (NrApi (..))
import qualified Sahasrara.Plugins.Netrunner.Type.Type as Type
import Sahasrara.Plugins.Netrunner.Utility.BanList (activeBanList, isBanned, latestBanList)
import Sahasrara.Plugins.Netrunner.Utility.Card (toCycle)
import Sahasrara.Utility.Search (autocomplete, closestMatch, closestPair, closestValue)
import Sahasrara.Utility.Utils (standardise)
import Text.Read (readMaybe)
import Prelude hiding (unwords)

-- | @Query@ represents a single search query with its arguments.
data Query
  = QText Text QueryComp (Card -> Maybe Text) [Text]
  | QInt Text QueryComp (Card -> Maybe Int) [Text]
  | QBool Text QueryComp (Card -> Maybe Bool) [Text] -- TODO: make this a single Text?
  | QBan Text QueryComp (Text, BanList)

-- | @QueryComp@ represents the types of comparison queries might take
data QueryComp = QEQ | QNE | QGT | QLT

-- | @searchCards@ looks for all cards that match a set of criteria.
searchCards :: NrApi -> [Query] -> Maybe [Card]
searchCards _ [] = Nothing
searchCards api pairs = Just $ nubBy cardEq $ foldr filterCards (cards api) pairs
  where
    cardEq :: Card -> Card -> Bool
    cardEq a b = title a == title b
    filterCards :: Query -> [Card] -> [Card]
    filterCards (QText _ sep f xs) = filterText sep f xs
    filterCards (QInt _ sep f xs) = filterInt sep f xs
    filterCards (QBool _ sep f xs) = filterBool sep f xs
    filterCards (QBan _ sep x) = filterBan sep x
    filterText :: QueryComp -> (Card -> Maybe Text) -> [Text] -> ([Card] -> [Card])
    filterText sep f xs =
      let check c x = isInfixOf (standardise x) $ standardise $ fromMaybe "" $ f c
          fil c = any (check c) xs
       in case sep of
            QEQ -> filter fil
            QNE -> filter (not . fil)
            _ -> id
    filterInt :: QueryComp -> (Card -> Maybe Int) -> [Text] -> ([Card] -> [Card])
    filterInt sep f xs =
      let check c x = fromMaybe False $ do
            a <- readMaybe $ unpack x
            b <- f c
            return $ (compFunc sep) a b
          fil c = any (check c) xs
       in filter fil
    compFunc :: QueryComp -> Int -> Int -> Bool
    compFunc QEQ = (==)
    compFunc QNE = (/=)
    compFunc QGT = (<)
    compFunc QLT = (>)
    filterBool :: QueryComp -> (Card -> Maybe Bool) -> a -> ([Card] -> [Card])
    filterBool QEQ f _ = filter (fromMaybe False . f)
    filterBool QNE f _ = filter (maybe True not . f)
    filterBool _ _ _ = id
    filterBan :: QueryComp -> (Text, BanList) -> ([Card] -> [Card])
    filterBan _ (_, b) = filter (not . (isBanned api b))

-- | @fixSearch@ takes a list of key/value pairs, formats them, and
-- repairs damaged queries to ensure they are valid for NetrunnerDB.
fixSearch :: NrApi -> [(String, Char, [String])] -> [Query]
fixSearch api = mapMaybe fix
  where
    fix :: (String, Char, [String]) -> Maybe (Query)
    fix pair = do
      a <- setComp pair
      b <- format $ packValues a
      c <- checkComp b
      return c
    setComp :: (String, Char, [String]) -> Maybe (String, QueryComp, [String])
    setComp (k, ':', v) = Just (k, QEQ, v)
    setComp (k, '!', v) = Just (k, QNE, v)
    setComp (k, '>', v) = Just (k, QGT, v)
    setComp (k, '<', v) = Just (k, QLT, v)
    setComp _ = Nothing
    packValues :: (String, QueryComp, [String]) -> (String, QueryComp, [Text])
    packValues (k, sep, v) = (k, sep, map pack v)
    format :: (String, QueryComp, [Text]) -> Maybe (Query)
    format ("_", sep, v) = Just $ QText "_" sep strippedTitle v
    format ("x", sep, v) = Just $ QText "x" sep strippedText v
    format ("a", sep, v) = Just $ QText "a" sep flavour v
    format ("e", sep, v) = Just $ QText "e" sep packCode v
    format ("c", sep, v) = Just $ QInt "c" sep cycleIndex $ map fixCycle v
    format ("t", sep, v) = Just $ QText "t" sep typeCode $ map fixType v
    format ("f", sep, v) = Just $ QText "f" sep factionCode $ concat $ map fixFaction v
    format ("s", sep, v) = Just $ QText "s" sep keywords v
    format ("d", sep, v) = Just $ QText "d" sep sideCode $ map fixSide v
    format ("i", sep, v) = Just $ QText "i" sep illustrator v
    format ("o", sep, v) = Just $ QInt "o" sep cost v
    format ("g", sep, v) = Just $ QInt "g" sep advancementCost v
    format ("m", sep, v) = Just $ QInt "m" sep memoryCost v
    format ("n", sep, v) = Just $ QInt "n" sep factionCost v
    format ("p", sep, v) = Just $ QInt "p" sep strength v
    format ("v", sep, v) = Just $ QInt "v" sep agendaPoints v
    format ("h", sep, v) = Just $ QInt "h" sep trashCost v
    -- format ("r", sep, v) =
    format ("u", sep, v) = Just $ QBool "u" sep uniqueness v
    format ("b", _, []) = Nothing
    format ("b", sep, v) = Just $ QBan "b" sep $ fixBan $ head v
    -- format ("z", sep, v) =
    format _ = Nothing
    cycleIndex :: Card -> Maybe Int
    cycleIndex card =
      let matchCycle c = Just (Cycle.code c) == (Cycle.code <$> toCycle api card)
       in Just $ case findIndex matchCycle $ cycles api of
            Nothing -> 0
            Just i -> i
    fixCycle :: Text -> Text -- Turns the name of a cycle into its index
    fixCycle c =
      let names = zip (map unpack cNames) [0 ..]
          codes = zip (map unpack cCodes) [0 ..]
       in pack $
            show $ case decimal c of
              Right x -> fst x
              Left _ -> case autocomplete (map toLower cNames) $ toLower c of
                Just c' -> fromMaybe 0 $ findIndex (== c') (map toLower cNames)
                Nothing -> closestValue (names ++ codes) $ unpack c
    fixFaction :: Text -> [Text] -- Turns certain aliases into multiple queries
    fixFaction "a" = ["anarch"]
    fixFaction "hb" = ["haas-bioroid"]
    fixFaction "mini" = ["adam", "apex", "sunny-lebeau"]
    fixFaction "d" = ["adam"]
    fixFaction "p" = ["apex"]
    fixFaction "u" = ["sunny-lebeau"]
    fixFaction "neutral" = ["neutral-corp", "neutral-runner"]
    fixFaction "-" = ["neutral-corp", "neutral-runner"]
    fixFaction "nc" = ["neutral-corp"]
    fixFaction "nr" = ["neutral-runner"]
    fixFaction f = case autocomplete fNames f of
      Just f' -> [f']
      Nothing -> [pack $ closestMatch (map unpack fNames) $ unpack f]
    fixType :: Text -> Text
    fixType t = case autocomplete tNames t of
      Just t' -> t'
      Nothing -> pack $ closestMatch (map unpack tNames) $ unpack t
    fixSide :: Text -> Text
    fixSide = pack . closestValue [("r", "runner"), ("c", "corp"), ("runner", "runner"), ("corp", "corp")] . unpack
    fixBan :: Text -> (Text, BanList)
    fixBan b =
      let bls = banLists api
          active = ("active", activeBanList api)
          latest = ("latest", latestBanList api)
          blsPairs = active : latest : (zip (map (unpack . BanList.name) bls) bls)
       in (\(x, y) -> (formatBanListName $ pack x, y)) $ closestPair blsPairs $ unpack b
    formatBanListName :: Text -> Text
    formatBanListName = toLower . (replace " " "-") . (replace "." "-")
    cNames :: [Text]
    cNames = map Cycle.name $ cycles api
    cCodes :: [Text]
    cCodes = map Cycle.code $ cycles api
    fNames :: [Text]
    fNames = map Faction.code $ factions api
    tNames :: [Text]
    tNames = map (toLower . Type.name) $ filter (not . Type.is_subtype) $ types api
    checkComp :: Query -> Maybe Query
    checkComp (QText _ QGT _ _) = Nothing
    checkComp (QText _ QLT _ _) = Nothing
    checkComp (QText k sep f s) = Just $ QText k sep f s
    checkComp (QInt k QGT f s) = if length s == 1 then Just (QInt k QGT f s) else Nothing
    checkComp (QInt k QLT f s) = if length s == 1 then Just (QInt k QLT f s) else Nothing
    checkComp (QInt k sep f s) = Just $ QInt k sep f s
    checkComp (QBool _ QGT _ _) = Nothing
    checkComp (QBool _ QLT _ _) = Nothing
    checkComp (QBool k sep f s) = Just $ QBool k sep f s
    -- NRDB allows QNE but it's functionally the same as QEQ
    -- Also note that for type reasons the length of QBans' arguments are fixed in format
    checkComp (QBan k _ s) = Just $ QBan k QEQ s

-- | @pairsToQuery@ takes a set of search query pairs ands turns it into a link
-- to an equivalent search on NetrunnerDB.
pairsToQuery :: [Query] -> Text
pairsToQuery pairs =
  let query = replace "_" "\\_" $ replace " " "+" $ pairsToNrdb pairs
   in "<https://netrunnerdb.com/find/?q=" <> query <> ">"

-- | @pairsToNrdb@ takes a set of search query pairs and formats it into a valid
-- plaintext search query for NetrunnerDB.
pairsToNrdb :: [Query] -> Text
pairsToNrdb pairs = unwords queries
  where
    queries :: [Text]
    queries = map format pairs
    format :: Query -> Text
    format (QText k sep _ vs) = format' k sep vs
    format (QInt k sep _ vs) = format' k sep vs
    format (QBool k sep _ vs) = format' k sep vs
    format (QBan k sep v) = format' k sep [fst v]
    format' :: Text -> QueryComp -> [Text] -> Text
    format' k sep vs =
      let v = intercalate "|" $ map formatValue vs
       in k <> fromComp sep <> v
    formatValue :: Text -> Text
    formatValue v =
      if " " `isInfixOf` v
        then "\"" <> v <> "\""
        else v
    fromComp :: QueryComp -> Text
    fromComp QEQ = ":"
    fromComp QNE = "!"
    fromComp QGT = ">"
    fromComp QLT = "<"

-- | @shorthand@ maps plaintext shortcuts to explicit queries.
shorthands :: Map String (String, Char, [String])
shorthands =
  fromList
    [ ("corp", ("d", ':', ["corp"])),
      ("runner", ("d", ':', ["runner"])),
      ("identity", ("t", ':', ["identity"])),
      ("agenda", ("t", ':', ["agenda"])),
      ("asset", ("t", ':', ["asset"])),
      ("ice", ("t", ':', ["ice"])),
      ("operation", ("t", ':', ["operation"])),
      ("upgrade", ("t", ':', ["upgrade"])),
      ("event", ("t", ':', ["event"])),
      ("hardware", ("t", ':', ["hardware"])),
      ("program", ("t", ':', ["program"])),
      ("resource", ("t", ':', ["resource"])),
      ("hb", ("f", ':', ["haas-bioroid"])),
      ("haas-bioroid", ("f", ':', ["haas-bioroid"])),
      ("jinteki", ("f", ':', ["jinteki"])),
      ("nbn", ("f", ':', ["nbn"])),
      ("weyland", ("f", ':', ["weyland-consortium"])),
      ("weyland-consortium", ("f", ':', ["weyland-consortium"])),
      ("anarch", ("f", ':', ["anarch"])),
      ("criminal", ("f", ':', ["criminal"])),
      ("shaper", ("f", ':', ["shaper"])),
      ("adam", ("f", ':', ["adam"])),
      ("apex", ("f", ':', ["apex"])),
      ("sunny", ("f", ':', ["sunny-lebeau"])),
      ("sunny-lebeau", ("f", ':', ["sunny-lebeau"])),
      ("mini", ("f", ':', ["mini"])),
      ("neutral", ("f", ':', ["neutral-corp", "neutral-runner"])),
      ("free", ("o", ':', ["0"])),
      ("premium", ("o", '>', ["0"])),
      ("cheap", ("o", '<', ["3"])),
      ("expensive", ("o", '>', ["6"])),
      ("ambush", ("s", ':', ["ambush"])),
      ("bioroid", ("s", ':', ["bioroid"])),
      ("companion", ("s", ':', ["companion"])),
      ("friend", ("s", ':', ["companion"])),
      ("connection", ("s", ':', ["connection"])),
      ("console", ("s", ':', ["console"])),
      ("transaction", ("s", ':', ["transaction"])),
      ("icebreaker", ("s", ':', ["icebreaker"])),
      ("fracter", ("s", ':', ["fracter"])),
      ("decoder", ("s", ':', ["decoder"])),
      ("sentry", ("s", ':', ["sentry"])),
      ("ai", ("s", ':', ["ai"])),
      ("barrier", ("s", ':', ["barrier"])),
      ("code-gate", ("s", ':', ["code gate"])),
      ("sentry", ("s", ':', ["sentry"])),
      ("mythic", ("s", ':', ["mythic"])),
      ("grail", ("s", ':', ["grail"]))
    ]
