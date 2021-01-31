const path = require("path");
const { getClient } = require(".");
const db = require(".");
const { readFile } = require("../utils/fileReader");
const { tsv2JSON } = require("../utils/tsvJson");

const TVS_FILE_PATH = path.resolve(process.cwd(), "data/cities_canada-usa.tsv");
/** Initialize locations table if not created before
 * @module db/createLocationsTable
 */

module.exports = {
  async createLocationsTable() {
    return new Promise((resolve, reject) => {
      try {
        (async function () {
          const createdBefore = await checkDataExists();

          if (createdBefore) {
            resolve(true);
            return;
          }

          const locationCreateTableFields = `
            id bigint,
            name varchar(256),
            ascii  varchar(256),
            alt_name text,
            lat float,
            long float,
            feat_class  varchar(20),
            feat_code  varchar(256),
            country  varchar(100),
            cc2 integer,
            admin1  varchar(256),
            admin2  varchar(256),
            admin3  varchar(256),
            admin4  varchar(256),
            population bigint,
            elevation float,
            dem integer,
            timezone  varchar(100),
            modified_at date`;

          try {
            await db.query("DROP TYPE json_locations");
          } catch (err) {
            console.warn("DROP json_locations error", err);
          }

          await db.query(`CREATE TYPE json_locations AS (${locationCreateTableFields})`);

          await db.query(`CREATE TABLE locations (${locationCreateTableFields})`);

          const locations = await getLocations();

          await insertLocations(locations);

          await db.query(`ALTER TABLE locations add COLUMN tsv tsvector`);

          await db.query(`UPDATE locations l SET tsv = to_tsvector('english',  CONCAT(l.name,',',l.alt_name))`);

          await db.query(`CREATE INDEX documents_tsv_tsvector_index ON locations USING GIN (tsv)`);

          resolve(true);
        })();
      } catch (err) {
        reject(err);
      }
    });
  },
};

async function checkDataExists() {
  try {
    const result = await db.query("SELECT COUNT(l.id) FROM locations l");
    return result.rows[0].count > 0;
  } catch (err) {
    return false;
  }
}

async function getLocations() {
  const tsvFile = await readFile(TVS_FILE_PATH);
  const jsonArr = await tsv2JSON(tsvFile);
  fixTypes(jsonArr);
  return jsonArr;
}

async function insertLocations(rows) {
  const fields = [
    "id",
    "name",
    "ascii",
    "alt_name",
    "lat",
    "long",
    "feat_class",
    "feat_code",
    "country",
    "cc2",
    "admin1",
    "admin2",
    "admin3",
    "admin4",
    "population",
    "elevation",
    "dem",
    "timezone",
    "modified_at",
  ];

  const columnsSql = fields.join(",");
  const client = await getClient();
  client.query(
    `INSERT INTO locations (${columnsSql}) 
        SELECT m.* FROM json_populate_recordset(null::json_locations, $1) AS m`,
    [JSON.stringify(rows)],
    function (err, result) {
      if (err) {
        throw err;
      }
      console.log(result.rows.length + " records added!");
    }
  );
}

function fixTypes(rows) {
  rows.forEach((row) => {
    row.id = ~~row.id;
    row.lat = parseFloat(row.lat);
    row.long = parseFloat(row.long);
    row.elevation = parseFloat(row.elevation);
    row.cc2 = ~~row.cc2;
    row.dem = ~~row.dem;
    row.population = ~~row.population;
    row.modified_at = row.modified_at ? new Date(row.modified_at).toISOString() : null;
  });
}
