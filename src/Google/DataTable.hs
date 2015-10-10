{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}

-- | Types for providing JSON data for the Google Charts API. See:  
-- https://developers.google.com/chart/interactive/docs/reference#dataparam
module Google.DataTable
    ( Cell (..) 
    , Column (..)
    , DataTable (..)
    ) where

import Data.Aeson (ToJSON (..), (.=), object)
import Data.Aeson.Types (Pair)
import Data.Time ( Day, LocalTime (..), TimeOfDay (..), UTCTime (..)
                 , toGregorian, utc, utcToLocalTime)
import Data.Text (Text)
import Text.Printf (printf)
import qualified Data.Aeson as A
import qualified Data.Text as T

data Type = BooleanT
          | NumberT
          | StringT
          | DateT
          | DateTimeT
          | TimeOfDayT
    deriving Show

data Value = Boolean {-# UNPACK #-} !Bool
           | Integral {-# UNPACK #-} !Int
           | Float {-# UNPACK #-} !Double
           | String !Text
           | Date !Day
           | DateTime !UTCTime
    deriving Show

-- | Column is metadata as type and id of a column.
data Column =
    Column { type_ :: !Type
           -- ^ Data type of the data in the column.
           , id_   :: !(Maybe Text)
           -- ^ String id of the column.
           , label :: !(Maybe Text)
           -- ^ String value that some visualizations display for
           -- this column.
           }
    deriving Show

data Cell = forall a. ToJSON a =>
    Cell { v :: !(Maybe Value)
         , f :: !(Maybe Text)
         , p :: !(Maybe a)
         }

data DataTable =
    DataTable { cols :: ![Column] }
    deriving Show

-- | ToJSON instance for type.
instance ToJSON Type where
    toJSON BooleanT   = A.String "boolean"
    toJSON NumberT    = A.String "number"
    toJSON StringT    = A.String "string"
    toJSON DateT      = A.String "date"
    toJSON DateTimeT  = A.String "datetime"
    toJSON TimeOfDayT = A.String "timeofday"

instance ToJSON Value where
    toJSON (Boolean x)  = toJSON x
    toJSON (Integral x) = toJSON x
    toJSON (Float x)    = toJSON x
    toJSON (String x)   = toJSON x
    toJSON (Date x)     = toJSON (toJSDate x)
    toJSON (DateTime x) = toJSON (toJSDateTime x)

instance ToJSON Column where
    toJSON col =
        let xs = maybeAddAs "type" (Just $ type_ col) $
                 maybeAddAs "id" (id_ col)              $
                 maybeAddAs "label" (label col) []
        in object xs

instance ToJSON Cell where
    toJSON cell = object (maybeAddAs "v" (v cell) [])

maybeAddAs :: ToJSON a => Text -> Maybe a -> [Pair] -> [Pair]
maybeAddAs name (Just val) xs = name .= val : xs
maybeAddAs _ Nothing xs       = xs

toJSDate :: Day -> Text
toJSDate day =
    let (y, m, d) = toGregorian day
        str       = printf "Date(%ld, %d, %d)" y (m - 1) d
    in T.pack str

toJSDateTime :: UTCTime -> Text
toJSDateTime time = 
    let localTime  = utcToLocalTime utc time
        (y, m, d)  = toGregorian $ localDay localTime
        tod        = localTimeOfDay localTime
        (h, mi, s) = (todHour tod, todMin tod, (truncate $ todSec tod) :: Int)
        str        = printf "Date(%ld, %d, %d, %d, %d, %d)" 
                           y (m - 1) d h mi s
    in T.pack str
