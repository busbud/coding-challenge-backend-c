-- Create index table and set weights to vector.

ALTER TABLE cities ADD COLUMN cities_indexed_name tsvector
GENERATED ALWAYS AS
    (setweight(to_tsvector('english', coalesce(name, '')), 'A') ||
        setweight(to_tsvector('english', coalesce(ascii_name, '')), 'B') ||
        setweight(to_tsvector('english', coalesce(alternate_name, '')), 'C') ||
        setweight(to_tsvector('english', coalesce(state, '')), 'D') ||
        setweight(to_tsvector('english', coalesce(country, '')), 'D'))
STORED;