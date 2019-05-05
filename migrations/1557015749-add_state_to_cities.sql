-- Migration: add_state_to_cities
-- Created at: 2019-05-04 21:22:29
-- ====  UP  ====

BEGIN;

ALTER TABLE cities
  ADD COLUMN state VARCHAR(200);

UPDATE cities SET state = '';

ALTER TABLE cities
  ALTER COLUMN state SET NOT NULL;

COMMIT;

-- ==== DOWN ====

BEGIN;

ALTER TABLE cities
  DROP COLUMN state;

COMMIT;
