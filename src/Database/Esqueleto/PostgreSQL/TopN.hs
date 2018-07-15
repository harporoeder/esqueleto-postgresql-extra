{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

-- | This module expects the PostgreSQL TopN extension
--
-- > CREATE EXTENSION topn;
-- The extension may be found at <https://github.com/citusdata/postgresql-topn>.

module Database.Esqueleto.PostgreSQL.TopN
  ( TopN
  , TopNEntry(..)
  , getTopN
  , unionAggregateTopN
  , addAggregateTopN
  , addStringTopN
  , unionTopN
  ) where

import Database.Persist.Sql
import Database.Esqueleto.Internal.Language
import Database.Esqueleto.Internal.Sql
import qualified Database.Persist.Postgresql.JSON as PJ
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import Data.Aeson hiding (Value)
import qualified Data.Text as T
import GHC.Generics (Generic)

newtype TopN = TopN { unTopN :: PJ.Value } deriving (Show)

data TopNEntry = TopNEntry
  { item      :: Text   -- ^ String inserted into TopN
  , frequency :: Double -- ^ Frequency estimation
  } deriving (Generic, Show)

-- | (@topn@) Get most popular strings from aggregated TopN.
-- This may return fewer results than requested.
getTopN ::
     SqlExpr (Value TopN)      -- ^ TopN structure
  -> SqlExpr (Value Int)       -- ^ How many to list
  -> SqlExpr (Value TopNEntry) -- ^ List of estimated most frequent items

getTopN subject count =
  unsafeSqlFunction "row_to_json" $ unsafeSqlFunction "topn" (subject, count)

-- | (@topn_add_agg@) Produce TopN instance from text column.
addAggregateTopN :: SqlExpr (Value Text) -> SqlExpr (Value TopN)
addAggregateTopN = unsafeSqlFunction "topn_add_agg"

-- | (@topn_union_agg@) Combine TopN instances column.
unionAggregateTopN :: SqlExpr (Value TopN) -> SqlExpr (Value TopN)
unionAggregateTopN = unsafeSqlFunction "topn_union_agg"

-- | (@topn_add@) Add string to TopN count structure.
addStringTopN ::
     SqlString s
  => SqlExpr (Value TopN)
  -> SqlExpr (Value s)
  -> SqlExpr (Value TopN)

addStringTopN acc text = unsafeSqlFunction "topn_add" (acc, text)

-- | (@topn_union@) Combine two TopN count instances.
unionTopN ::
     SqlExpr (Value TopN)
  -> SqlExpr (Value TopN)
  -> SqlExpr (Value TopN)
  
unionTopN left right = unsafeSqlFunction "topn_union" (left, right)

instance PersistField TopN where
  toPersistValue = toPersistValue . unTopN

  fromPersistValue t = fmap TopN $ fromPersistValue t

instance PersistFieldSql TopN where
  sqlType _ = SqlOther "jsonb"

instance ToJSON   TopNEntry
instance FromJSON TopNEntry

instance PersistField TopNEntry where
  toPersistValue = PersistDbSpecific . BL.toStrict . encode

  fromPersistValue (PersistByteString bytes) =
    case decode (BL.fromStrict bytes) of
      Nothing -> Left "Failed to deserialize TopNRecord"
      Just x  -> Right x

  fromPersistValue a = Left $ T.pack $ show a

instance PersistFieldSql TopNEntry where
  sqlType _ = SqlOther "jsonb"
