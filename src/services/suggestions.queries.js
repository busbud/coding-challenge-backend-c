const { Pool } = require('pg');

const pool = new Pool({
  connectionString: process.env.DATABASE_URL,
  ssl: {
    rejectUnauthorized: false
  }
});

exports.getSuggestions = async ({ q, latitude, longitude }) => {
  const hasLocation = !!latitude && !!longitude;

  const suggestions = hasLocation
    ? await getSuggestionsByNameAndLocation(q, latitude, longitude)
    : await getSuggestionsByName(q);

  return suggestions.map(formatRow);
};

function formatRow({ name, country, state, lat, long, score }) {
  return {
    name: country === 'US' ? `${name}, ${state}, USA` : `${name}, Canada`,
    latitude: lat.toFixed(5),
    longitude: long.toFixed(5),
    score: Math.round(score * 10) / 10
  };
}

function getQuery(hasLocation) {
  const locationWeight = hasLocation
    ? ' * 1 / (1 + calculate_distance(c.lat, c.long, $2, $3) / 100)'
    : '';

  return `
    SELECT c.name, c.country, case when c.country = 'US' then c.admin1 else '' end as state,
    c.lat, c.long, SIMILARITY(c.name,$1)${locationWeight} as score
    FROM cities c
    WHERE c.population >= 5000 and c.country in ('CA', 'US')
    ORDER BY SIMILARITY(c.name,$1)${locationWeight} DESC
    LIMIT 10;`;
}

async function getSuggestionsByNameAndLocation(q, latitude, longitude) {
  const query = getQuery(true);
  const { rows } = await pool.query(query, [q, latitude, longitude]);

  return rows;
}

async function getSuggestionsByName(q) {
  const query = getQuery(false);

  const { rows } = await pool.query(query, [q]);

  return rows;
}
