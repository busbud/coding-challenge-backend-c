require("dotenv").config();
import elasticsearch from "elasticsearch";
import fs from "fs";
import readLine from "readline";
import PQueue from "p-queue";

const fieldsMapping: any = {
  0: "geonameid",
  1: "name",
  2: "asciiname",
  3: "alternatenames",
  4: "lat",
  5: "lon",
  6: "feature_class",
  7: "feature_code",
  8: "country_code",
  9: "cc2",
  10: "admin1_code",
  11: "admin2_code",
  12: "admin3_code",
  13: "admin4_code",
  14: "population",
  15: "elevation",
  16: "dem",
  17: "timezone",
  18: "modification_date",
};

const EsReindex = () => {
  const filePath = process.argv[2] || "./src/assets/data/cities_canada-usa.tsv";

  const log = { errors: 0, success: 0 };
  const rowsWithError = [];

  //** Instantiate ES client
  const client = new elasticsearch.Client({
    hosts: [`${process.env.ELASTIC_SEARCH_URL}`],
    maxRetries: 0,
  });

  //** Instantiate queue manager
  const queue = new PQueue({ concurrency: 150 });

  //** Check if elasticsearch is running
  const ping = () =>
    new Promise<void>((resolve, reject) => {
      client.ping({ requestTimeout: 3000 }, (error: any) => {
        if (error) {
          const error = new Error("Elastic search is down!");
          return reject(error);
        }
        console.info("Elastic search is running...");
        resolve();
      });
    });

  // ** Create elastic search indexes if needed
  const addESIndexes = () =>
    new Promise<void>((resolve, reject) => {
      console.info("Creating indexes for elastic search");
      client.indices.create({ index: "cities_index", includeTypeName: true, body: {
        settings: {
          "analysis" : {
            "analyzer" : {
              "folding" : {
                "tokenizer": "standard",
                "filter": [
                  "lowercase",
                  "asciifolding"
                ]
              }
            }
          }
        },
        mappings: {
          "city_record": {
          properties: {
            "admin1_code": {
              "type": "text",
              "fields": {
                "keyword": {
                  "type": "keyword",
                  "ignore_above": 256
                }
              }
            },
            "admin2_code": {
              "type": "text",
              "fields": {
                "keyword": {
                  "type": "keyword",
                  "ignore_above": 256
                }
              }
            },
            "admin3_code": {
              "type": "text",
              "fields": {
                "keyword": {
                  "type": "keyword",
                  "ignore_above": 256
                }
              }
            },
            "admin4_code": {
              "type": "text",
              "fields": {
                "keyword": {
                  "type": "keyword",
                  "ignore_above": 256
                }
              }
            },
            "alternatenames": {
              "type": "text",
              "fields": {
                "keyword": {
                  "type": "keyword",
                  "ignore_above": 256
                }
              }
            },
            "asciiname": {
              "type": "text",
              "fields": {
                "keyword": {
                  "type": "keyword",
                  "ignore_above": 256
                }
              }
            },
            "cc2": {
              "type": "text",
              "fields": {
                "keyword": {
                  "type": "keyword",
                  "ignore_above": 256
                }
              }
            },
            "country_code": {
              "type": "text",
              "fields": {
                "keyword": {
                  "type": "keyword",
                  "ignore_above": 256
                }
              }
            },
            "dem": {
              "type": "text",
              "fields": {
                "keyword": {
                  "type": "keyword",
                  "ignore_above": 256
                }
              }
            },
            "elevation": {
              "type": "text",
              "fields": {
                "keyword": {
                  "type": "keyword",
                  "ignore_above": 256
                }
              }
            },
            "feature_class": {
              "type": "text",
              "fields": {
                "keyword": {
                  "type": "keyword",
                  "ignore_above": 256
                }
              }
            },
            "feature_code": {
              "type": "text",
              "fields": {
                "keyword": {
                  "type": "keyword",
                  "ignore_above": 256
                }
              }
            },
            "geonameid": {
              "type": "text",
              "fields": {
                "keyword": {
                  "type": "keyword",
                  "ignore_above": 256
                }
              }
            },
            "modification_date": {
              "type": "date"
            },
            "name": {
              "type": "text",
              "analyzer": "folding",
              "fields": {
                "keyword": {
                  "type": "keyword",
                  "ignore_above": 256
                }
              }
            },
            "population": {
              "type": "rank_feature",
            },
            "timezone": {
              "type": "text",
              "fields": {
                "keyword": {
                  "type": "keyword",
                  "ignore_above": 256
                }
              }
            },
            "location": {
              "type": "geo_point"
            },
          }
        }
      }
      } },
        (error, response) => {
          if (error) {
            return reject(error);
          }
          console.info(`ElasticSearch indexes created successfully: ${response}`);
          resolve();
        }
      );
    });

  //** Transform row into city object
  const parseObject = (values: string[]) =>
    values.reduce((prev, value, index) => {
      const objectKey = fieldsMapping[index];
      if (objectKey === "lat" || objectKey === "lon") {
        if (!prev["location"]) {
          prev["location"] = {};
        }
        prev["location"][objectKey] = parseFloat(value);
      } else {
        prev[objectKey] = value;
      }
      return prev;
    }, {} as any);

  // ** Extract/Transform/Load data
  const runETL = () =>
    new Promise<void>((resolve, reject) => {
      console.info("Processing records...");

      const stream = fs.createReadStream(filePath, { encoding: "utf8" });
      const lineReader = readLine.createInterface({ input: stream });
      let isFirstLine = true;

      // Get line from file and parse into object
      lineReader.on("line", (line) => {
        // Remove header
        if (isFirstLine) {
          isFirstLine = false;
          return;
        }

        const fields = line.split("\t");
        const record = parseObject(fields);
        lineReader.emit("extracted", record);
      });

      // Do all the necessary transformations if needed
      lineReader.on("extracted", (record) => {
        /** ... */
        lineReader.emit("transformed", record);
      });

      // Load record into ElasticSearch
      lineReader.on("transformed", (record) => {
        const task = () =>
          new Promise((resolve, reject) => {
            client.index(
              { index: "cities_index", type: "city_record", body: record },
              (err) => {
                if (err) {
                  reject(record);
                }
                resolve(record);
              }
            );
          })
            .then(() => {
              log.success += 1;
            })
            .catch((error) => {
              log.errors += 1;
              rowsWithError.push({
                record,
                error,
              });
            });
        queue.add(task);
      });

      queue.on("idle", () => {
        const total = log.success + log.errors;
        console.info("\nAll records have been processed.");
        console.info(`- Success: ${log.success}`);
        console.info(`- Error: ${log.errors}`);
        console.info(`- Total: ${total}`);
        resolve();
      });
    });

  //** Bootstrap ReIndex script
  const bootstrap = async () => {
    await ping();

    try {
      await addESIndexes();
    } catch (e) {
      console.log(e);
    }

    await runETL();
  };

  return { bootstrap };
};

export default EsReindex().bootstrap();
