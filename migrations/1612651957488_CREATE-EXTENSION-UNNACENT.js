/* eslint-disable camelcase */

exports.shorthands = undefined;

exports.up = pgm => {
	const SQL = `
		CREATE EXTENSION IF NOT EXISTS unaccent;
    `;
	pgm.sql(SQL);
};

exports.down = pgm => {
	const SQL = `
		DROP EXTENSION IF EXISTS unaccent;
    `;
	pgm.sql(SQL);
};
