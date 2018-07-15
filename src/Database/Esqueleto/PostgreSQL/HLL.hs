{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

-- | This module expects the PostgreSQL HyperLogLog extension
--
-- > CREATE EXTENSION hll;
-- The extension may be found at <https://github.com/citusdata/postgresql-hll>.

module Database.Esqueleto.PostgreSQL.HLL
  ( HLL
  , addHashHLL
  , cardinalityHLL
  , unionHLL
  , emptyHLL
    -- * Hash functions
  , hashInt16HLL
  , hashInt32HLL
  , hashInt64HLL
  , hashIntHLL
  , hashBytesHLL
  , hashStringHLL
  , unsafeHashHLL
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Database.Persist.Sql
import Database.Esqueleto.Internal.Language
import Database.Esqueleto.Internal.Sql
import Text.Read
import qualified Data.Text as T
import GHC.Int (Int16, Int32, Int64)
import qualified Data.ByteString.Base16 as B16

newtype HLL = HLL { unHLL :: ByteString }

newtype HLLHash = HLLHash { unHLLHash :: Int64 } deriving (Show)

-- | (@hll_add_agg@) Add already hashed value to HLL structure
addHashHLL :: SqlExpr (Value HLLHash) -> SqlExpr (Value HLL)
addHashHLL = unsafeSqlFunction "hll_add_agg"

-- | (@hll_cardinality@) Calculate estimated cardinality of HLL
cardinalityHLL :: SqlExpr (Value HLL) -> SqlExpr (Value (Maybe Double))
cardinalityHLL = unsafeSqlFunction "hll_cardinality" where

-- | (@hll_union@) Associative composition of HLL structures
unionHLL :: SqlExpr (Value HLL) -> SqlExpr (Value HLL) -> SqlExpr (Value HLL)
unionHLL left right = unsafeSqlFunction "hll_union" (left, right)

-- | (@hll_empty@) Default cardinality 0 HLL structure
emptyHLL :: SqlExpr (Value HLL)
emptyHLL = unsafeSqlValue "hll_empty()"

-- | (@hll_hash_smallint@) Specialized hash for PostgreSQL SmallInt
hashInt16HLL :: SqlExpr (Value Int16) -> SqlExpr (Value HLLHash)
hashInt16HLL = unsafeSqlFunction "hll_hash_smallint"

-- | (@hll_hash_integer@) Specialized hash for PostgreSQL Integer
hashInt32HLL :: SqlExpr (Value Int32) -> SqlExpr (Value HLLHash)
hashInt32HLL = unsafeSqlFunction "hll_hash_integer"

-- | (@hll_hash_bigint@) Specialized hash for PostgreSQL BigInt
hashInt64HLL :: SqlExpr (Value Int64) -> SqlExpr (Value HLLHash)
hashInt64HLL = unsafeSqlFunction "hll_hash_bigint"

-- | (@hll_hash_bigint@) While int may be 32 bit on the Haskell side Persistent
-- sets int to 64 bit for Postgres in the entity specification language.
hashIntHLL :: SqlExpr (Value Int) -> SqlExpr (Value HLLHash)
hashIntHLL = unsafeSqlFunction "hll_hash_bigint"

-- | (@hll_hash_bytea@) Specialized hash for PostgreSQL Bytea
hashBytesHLL :: SqlExpr (Value ByteString) -> SqlExpr (Value HLLHash)
hashBytesHLL = unsafeSqlFunction "hll_hash_bytea"

-- | (@hll_hash_text@) Specialized hash for PostgreSQL Text
hashStringHLL :: SqlString s => SqlExpr (Value s) -> SqlExpr (Value HLLHash)
hashStringHLL = unsafeSqlFunction "hll_hash_text"

-- | (@hll_hash_any@) Dynamically dispatched hashing function. This is
-- both slower than specialized versions, and may produce run-time errors.
unsafeHashHLL :: SqlExpr (Value x) -> SqlExpr (Value HLLHash)
unsafeHashHLL = unsafeSqlFunction "hll_hash_any"

instance PersistField HLL where
  toPersistValue = PersistDbSpecific . unHLL

  fromPersistValue (PersistDbSpecific bytes) = if C8.length bytes < 2
    then Left "PostgreSQL HLL String Wrong Length"
    else Right $ HLL $ fst $ B16.decode $ C8.drop 2 bytes

  fromPersistValue a = Left $ T.pack $ show a

instance PersistField HLLHash where
  toPersistValue = PersistDbSpecific . C8.pack . show . unHLLHash

  fromPersistValue (PersistDbSpecific bytes) =
    case readEither (C8.unpack bytes) of
      Left err -> Left $ T.pack err
      Right v  -> Right $ HLLHash v

  fromPersistValue a = Left $ T.pack $ show a

instance PersistFieldSql HLL where
  sqlType _ = SqlOther "hll"
