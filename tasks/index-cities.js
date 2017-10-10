#!/usr/bin/env node

const db = require('../services/db');
const createElasticsearchClient = require('../services/elasticsearch');

(async () => {
  try {
    const esClient = createElasticsearchClient();

    let indexExists;
    try {
      await esClient.indices.get({ index: 'cities' });
      indexExists = true;
    } catch (err) {
      if (err.status === 404) {
        indexExists = false;
      } else {
        throw err;
      }
    }

    if (indexExists) {
      await esClient.indices.delete({ index: 'cities' });
    }

    await esClient.indices.create({
      index: 'cities',
      body: {
        settings: {
          analysis: {
            analyzer: {
              city_name_analyzer: {
                tokenizer: 'standard',
                filter: ['standard', 'lowercase', 'asciifolding'],
              },
            },
          },
        },
        mappings: {
          city: {
            properties: {
              name: {
                type: 'text',
                analyzer: 'city_name_analyzer',
                search_analyzer: 'city_name_analyzer',
              },
              alternate_names: {
                type: 'text',
                analyzer: 'city_name_analyzer',
                search_analyzer: 'city_name_analyzer',
              },
              admin_code: {
                type: 'string',
                index: 'not_analyzed',
              },
              country_code: {
                type: 'string',
                index: 'not_analyzed',
              },
              location: { type: 'geo_point' },
            },
          },
        },
      },
    });

    db.sequelize.transaction(async (t) => {
      await db.sequelize.query(
        'DECLARE cities_cur CURSOR FOR SELECT id, name, alternate_names, admin_code, country_code, longitude, latitude FROM cities',
        {
          transaction: t,
        },
      );

      let indexed = 0;

      async function indexMore(n) {
        const [results] = await db.sequelize.query('FETCH 10 FROM cities_cur', { transaction: t });

        if (results.length === 0) {
          console.log(`${indexed} cities indexed.`);
          process.exit(0);
        }

        await esClient.bulk({
          body: [].concat(...results.map(city => [
            { create: { _index: 'cities', _type: 'city', _id: city.id } },
            {
              name: city.name,
              alternate_names: city.alternate_names,
              location: { lat: city.latitude, lon: city.longitude },
              admin_code: city.admin_code,
              country_code: city.country_code,
            },
          ])),
        });

        indexed += n;

        await indexMore(n);
      }

      await indexMore(10);
    });
  } catch (err) {
    console.log(err);
    process.exit(1);
  }
})();
