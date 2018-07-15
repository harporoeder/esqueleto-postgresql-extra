{-# LANGUAGE OverloadedStrings #-}

-- | This module expects the PostgreSQL FuzzyStringMatch extension
--
-- > CREATE EXTENSION fuzzystrmatch;
-- The extension is distributed by default but not enabled
-- <https://www.postgresql.org/docs/9.5/static/fuzzystrmatch.html>.

module Database.Esqueleto.PostgreSQL.Fuzzy
  ( -- * Soundex
    soundexCode
  , soundexSimilarity
    -- * Levenshtein
  , levenshteinDistance
  , levenshteinDistanceLE
    -- * Metaphone
  , metaphoneCode
  , doubleMetaphoneCode
  , doubleMetaphoneCodeAlt
  ) where

import Database.Esqueleto.Internal.Language
import Database.Esqueleto.Internal.Sql

-- | (@soundex@) Calculate a 4 character Soundex code from a given string
--
-- > > select $ return $ (soundexCode $ val "John", soundexCode $ val "Bob")
-- > ("J500", "B100")
soundexCode ::
     SqlString s
  => SqlExpr (Value s) -- ^ Some name
  -> SqlExpr (Value s) -- ^ Soundex code

soundexCode = unsafeSqlFunction "soundex"

-- | (@difference@) Calculates how similar two names are. A value of 4 indicates
-- the same Soundex code. A value of 0 indicates a totally different name.
--
-- > > select $ return $ soundexSimilarity (val "John") (val "Bob")
-- > 2
soundexSimilarity ::
     SqlString s
  => SqlExpr (Value s)   -- ^ Name one
  -> SqlExpr (Value s)   -- ^ Name two
  -> SqlExpr (Value Int) -- ^ Match count 0-4

soundexSimilarity left right = unsafeSqlFunction "difference" (left, right)

-- | (@levenshtein@) Calculate Levenshtein distance between two strings.
--
-- > > select $ return $ levenshteinDistance (val "Cat") (val "Bat")
-- > 2
levenshteinDistance ::
     SqlString s
  => SqlExpr (Value s)   -- ^ Source where length < 255
  -> SqlExpr (Value s)   -- ^ Target where length < 255
  -> SqlExpr (Value Int) -- ^ Distance

levenshteinDistance source target =
  unsafeSqlFunction "levenshtein" (source, target)

-- | (@levenshtein_less_equal@) Same as (@levenshteinDistance@) except
-- optimized for short strings. If the difference is < Max then the difference
-- result will be exact, otherwise the result will be approximate.
levenshteinDistanceLE ::
     SqlString s
  => SqlExpr (Value s)   -- ^ Source where length < 255
  -> SqlExpr (Value s)   -- ^ Target where length < 255
  -> SqlExpr (Value Int) -- ^ Max where N < 255
  -> SqlExpr (Value Int) -- ^ Distance

levenshteinDistanceLE source target maxd =
  unsafeSqlFunction "levenshtein_less_equal" (source, target, maxd)

-- | (@metaphone@) Modern alternative to Soundex.
--
-- > select $ return $ (metaphoneCode (val "John") (val 8), metaphoneCode (val "Bob") (val 8))
-- > ("JN", "BB")
metaphoneCode ::
     SqlString s
  => SqlExpr (Value s)   -- ^ A word
  -> SqlExpr (Value Int) -- ^ Max length
  -> SqlExpr (Value s)   -- ^ Metaphone code

metaphoneCode text maxlen = unsafeSqlFunction "metaphone" (text, maxlen)

-- | (@dmetaphone@) Similar to metaphone but instead uses two codes, a primary
-- and alternate. This is used in conjunction with (@doubleMetaphoneCodeAlt@).
-- This algorithm is designed to additionally support non English languages.
doubleMetaphoneCode :: SqlString s => SqlExpr (Value s) -> SqlExpr (Value s)
doubleMetaphoneCode = unsafeSqlFunction "dmetaphone"

-- | (@dmetaphone_alt@) Alternative doubleMetaphone used in conjunction with the
-- (@doubleMetaphoneCode@) function.
doubleMetaphoneCodeAlt :: SqlString s => SqlExpr (Value s) -> SqlExpr (Value s)
doubleMetaphoneCodeAlt = unsafeSqlFunction "dmetaphone_alt"
