{-# LANGUAGE OverloadedStrings #-}

-- | This module expects the PostgreSQL UUID extension
--
-- > CREATE EXTENSION uuid-ossp;
-- The extension is distributed by default but not enabled
-- <https://www.postgresql.org/docs/9.5/static/uuid-ossp.html>.

module Database.Esqueleto.PostgreSQL.UUID
  ( module Data.UUID
  , makeNilUUID
    -- * Namespaces
  , makeNSDNS
  , makeNSURL
  , makeNSOID
  , makeNSX500
    -- * Versions
  , generateUUIDV1
  , generateUUIDV1MC
  , generateUUIDV4
  , generateUUIDV3
  , generateUUIDV5
  ) where

import Database.Persist.Sql
import Database.Esqueleto.Internal.Language
import Database.Esqueleto.Internal.Sql
import Data.UUID (UUID)
import qualified Data.Text as T
import qualified Data.UUID as U
import qualified Data.ByteString.Char8 as C8

-- | (@uuid_nil@) All zero UUID 00000000-0000-0000-0000-000000000000.
makeNilUUID :: SqlExpr (Value UUID)
makeNilUUID = unsafeSqlValue "uuid_nil()"

-- | (@uuid_ns_dns@) v3/v5 DNS UUID namespace.
makeNSDNS :: SqlExpr (Value UUID)
makeNSDNS = unsafeSqlValue "uuid_ns_dns()"

-- | (@uuid_ns_url@) v3/v5 Url UUID namespace.
makeNSURL :: SqlExpr (Value UUID)
makeNSURL = unsafeSqlValue "uuid_ns_url()"

-- | (@uuid_ns_oid@) v3/v5 OID UUID namespace. No relation to PostgreSQL OIDs.
makeNSOID :: SqlExpr (Value UUID)
makeNSOID = unsafeSqlValue "uuid_ns_oid()"

-- | (@uuid_ns_x500@) v3/v5 X.500 namespace.
makeNSX500 :: SqlExpr (Value UUID)
makeNSX500 = unsafeSqlValue "uuid_ns_x500()"

-- | (@uuid_generate_v1@) Make UUIDV1. Takes the MAC address + the time.
generateUUIDV1 :: SqlExpr (Value UUID)
generateUUIDV1 = unsafeSqlValue "uuid_generate_v1()"

-- | (@uuid_generate_v1mc@) Same as UUIDV1 except using a random MAC address.
generateUUIDV1MC :: SqlExpr (Value UUID)
generateUUIDV1MC = unsafeSqlValue "uuid_generate_v1mc()"

-- | (@uuid_generate_v4@) Randomly generated UUID4.
generateUUIDV4 :: SqlExpr (Value UUID)
generateUUIDV4 = unsafeSqlValue "uuid_generate_v4()"

-- | (@uuid_generate_v3@) MD5 hash of text + namespace.
generateUUIDV3 ::
     SqlString s
  => SqlExpr (Value UUID)
  -> SqlExpr (Value s)
  -> SqlExpr (Value UUID)
  
generateUUIDV3 namespace text =
  unsafeSqlFunction "uuid_generate_v3" (namespace, text)

-- | (@uuid_generate_v5@) Same as v3 but with SHA1 instead.
generateUUIDV5 ::
     SqlString s
  => SqlExpr (Value UUID)
  -> SqlExpr (Value s)
  -> SqlExpr (Value UUID)

generateUUIDV5 namespace text =
  unsafeSqlFunction "uuid_generate_v5" (namespace, text)

instance PersistField UUID where
  toPersistValue = PersistDbSpecific . C8.pack . U.toString

  fromPersistValue (PersistDbSpecific bytes) =
    case U.fromString (C8.unpack bytes) of
      Nothing -> Left "Failed to deserialize UUID"
      Just x  -> Right x

  fromPersistValue a = Left $ T.pack $ show a

instance PersistFieldSql UUID where
  sqlType _ = SqlOther "uuid"
