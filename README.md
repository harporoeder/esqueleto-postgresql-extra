# Esqueleto PostgreSQL Extra

This package provides Esqueleto support for several PostgreSQL specific extensions. These extensions are not enabled by default in Postgres and may need to be explicitly enabled, or additionally compiled.

## Extensions / Modules

* FuzzyStrMatch [https://www.postgresql.org/docs/9.5/static/fuzzystrmatch.html]
* PGCrypto [https://www.postgresql.org/docs/9.5/static/pgcrypto.html]
* UUID-OSSP [https://www.postgresql.org/docs/9.5/static/uuid-ossp.html]
* PostgreSQL-HLL [https://github.com/citusdata/postgresql-hll]
* PostgreSQL-TopN [https://github.com/citusdata/postgresql-topn]

## Additional Persist Fields

Several extra Persistent field types are defined for integration with the various modules.

```
import Database.Esqueleto.PostgreSQL.HLL (HLL)
import Database.Esqueleto.PostgreSQL.TopN (TopN)
import Database.Esqueleto.PostgreSQL.UUID (UUID)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
MyTable
  myCount HLL  -- HyperLogLog
  myRank  TopN -- Sketch TopN
  myId    UUID -- Standard UUID
|]
```
