{-# LANGUAGE OverloadedStrings #-}

-- | This module expects the PostgreSQL PGCrypto extension
--
-- > CREATE EXTENSION pgcrypto;
-- The extension is distributed by default but not enabled
-- <https://www.postgresql.org/docs/9.5/static/pgcrypto.html>

module Database.Esqueleto.PostgreSQL.Crypto
  ( HashAlgo(..)
  , CryptAlgo(..)
  , secureRandomBytes
  , hash
  , hmac
  , genCryptSalt
  , crypt
  ) where

import Data.Text.Lazy.Builder (fromString)
import Database.Esqueleto.Internal.Language
import Database.Esqueleto.Internal.Sql
import Data.ByteString (ByteString)
import Data.Semigroup

data HashAlgo =
    HashAlgoMD5    -- ^ 16 byte result
  | HashAlgoSHA1   -- ^ 20 byte result
  | HashAlgoSHA224 -- ^ 28 byte result
  | HashAlgoSHA256 -- ^ 32 byte result
  | HashAlgoSHA384 -- ^ 48 byte result
  | HashAlgoSHA512 -- ^ 64 byte result
  deriving (Show)

addQuotes :: String -> String
addQuotes s = "'" <> s <> "'"

hashToSQL :: HashAlgo -> SqlExpr (Value String)
hashToSQL v = unsafeSqlValue $ fromString $ addQuotes $ case v of
  HashAlgoMD5    -> "md5"
  HashAlgoSHA1   -> "sha1"
  HashAlgoSHA224 -> "sha224"
  HashAlgoSHA256 -> "sha256"
  HashAlgoSHA384 -> "sha384"
  HashAlgoSHA512 -> "sha512"

data CryptAlgo =
    CryptAlgoBF   -- ^ 128 bit salt, 60 byte result
  | CryptAlgoMD5  -- ^ 48 bit salt, 34 byte result
  | CryptAlgoXDES -- ^ 24 bit salt, 20 byte result
  | CryptAlgoDES  -- ^ 12 bit salt, 13 byte result
  deriving (Show)

cryptToSQL :: CryptAlgo -> SqlExpr (Value String)
cryptToSQL v = unsafeSqlValue $ fromString $ addQuotes $ case v of
  CryptAlgoBF   -> "bf"
  CryptAlgoMD5  -> "md5"
  CryptAlgoXDES -> "xdes"
  CryptAlgoDES  -> "des"

-- |(@gen_random_bytes@) Generate N random bytes with a limit of 1024 bytes at
-- a time.
secureRandomBytes :: SqlExpr (Value Int) -> SqlExpr (Value ByteString)
secureRandomBytes = unsafeSqlFunction "gen_random_bytes"

-- |(@digest@) Hash bytes with given algorithm.
hash ::
     SqlString s
  => SqlExpr (Value s)
  -> HashAlgo
  -> SqlExpr (Value ByteString)
  
hash text algo = unsafeSqlFunction "digest" (text, hashToSQL algo)

-- | (@hmac@) Compute a keyed hash based message authentication code.
hmac ::
     (SqlString s1, SqlString s2)
  => SqlExpr (Value s1)         -- ^ Subject text
  -> SqlExpr (Value s2)         -- ^ MAC key
  -> HashAlgo                   -- ^ Algorithm
  -> SqlExpr (Value ByteString) -- ^ Hashed MAC

hmac text key algo = unsafeSqlFunction "hmac" (text, key, hashToSQL algo)

-- |(@gen_salt@) Generate random password salt for specific algorithm.
genCryptSalt :: SqlString s => CryptAlgo -> SqlExpr (Value s)
genCryptSalt algo = unsafeSqlFunction "gen_salt" $ cryptToSQL algo

-- |(@crypt@) Password hashing and checking.
crypt ::
     SqlString s
  => SqlExpr (Value s) -- ^ Actual password
  -> SqlExpr (Value s) -- ^ Password salt / old hash
  -> SqlExpr (Value s) -- ^ Hashed password

crypt text salt = unsafeSqlFunction "crypt" (text, salt)
