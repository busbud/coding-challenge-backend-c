-- Migration: add_index_on_cities_name
-- Created at: 2019-05-05 13:33:35
-- ====  UP  ====

BEGIN;

CREATE INDEX idx_cities_lower_name ON cities(LOWER(name));

COMMIT;

-- ==== DOWN ====

BEGIN;

DROP INDEX idx_cities_lower_name;

COMMIT;
