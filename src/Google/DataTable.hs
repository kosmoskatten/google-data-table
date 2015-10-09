{-# LANGUAGE OverloadedStrings #-}

-- | Types for providing JSON data for the Google Charts API. See:  
-- https://developers.google.com/chart/interactive/docs/reference#dataparam
module Google.DataTable
    ( Column (..)
    , DataTable (..)
    ) where

import Data.Aeson (ToJSON, (.=), object)
import Data.Aeson.Types (Pair)
import Data.Text (Text)
import qualified Data.Aeson as A

data Type = Boolean
          | Number
          | String
          | Date
          | DateTime
          | TimeOfDay
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

data DataTable =
    DataTable { cols :: ![Column] }
    deriving Show

-- | ToJSON instance for type.
instance ToJSON Type where
    toJSON Boolean   = A.String "boolean"
    toJSON Number    = A.String "number"
    toJSON String    = A.String "string"
    toJSON Date      = A.String "date"
    toJSON DateTime  = A.String "datetime"
    toJSON TimeOfDay = A.String "timeofday"

instance ToJSON Column where
    toJSON col =
        let xs = maybeAddAs "type" (Just $ type_ col) $
                 maybeAddAs "id" (id_ col)              $
                 maybeAddAs "label" (label col) []
        in object xs

maybeAddAs :: ToJSON a => Text -> Maybe a -> [Pair] -> [Pair]
maybeAddAs name (Just v) xs = name .= v : xs
maybeAddAs _ Nothing xs     = xs
