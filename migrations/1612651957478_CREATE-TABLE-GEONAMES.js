/* eslint-disable camelcase */

exports.shorthands = undefined;

exports.up = pgm => {
	const SQL = `
		create table IF NOT EXISTS "geo_names"
			(
				id bigint not null
					constraint "PK_geo_names"
						primary key,
				name varchar(200) NOT NULL,
				ascii_name varchar(200) NOT NULL,
				country varchar(10) NOT NULL,
				alternate_names varchar(5000),
				location point NOT NULL,
				search_tokens tsvector,
			);
			
			CREATE INDEX IF NOT EXISTS idx_location ON geo_names USING GIST(location);
			CREATE INDEX IF NOT EXISTS idx_search_tokens ON geo_names USING GIN(search_tokens);
	`;
	pgm.sql(SQL);
};

exports.down = pgm => {
	const SQL = `
      DROP TABLE IF EXISTS "geo_names"; 
		`;
	pgm.sql(SQL);
};


