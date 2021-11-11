require("dotenv").config();
import elasticsearch from "elasticsearch";
import fs from "fs";
import readLine from "readline";
import PQueue from "p-queue";
import { fieldsMapping, fieldsConfig } from "./cluster-fields";

/**
 * Seed class for elasticsearch
 * Usage: Run `yarn seed` at the root of the project
 * @returns {Promise<void>}
 */
const EsSeed = () => {
  /* Get command line argument or use the default path */
  const filePath = process.argv[2] || "../../src/data/cities_canada-usa.tsv";

  /* Generate log report */
  const log = {
    errors: 0,
    success: 0,
    erroredRecords: [],
  } as any;

  /* Instantiate ES client */
  const client = new elasticsearch.Client({
    hosts: [`${process.env.ELASTIC_SEARCH_URL}`],
    maxRetries: 0,
  });

  /* Instantiate queue manager */
  const queue = new PQueue({ concurrency: 150 });

  /**
   * Check if elasticsearch is running
   * @returns Promise<void>
   * @throw Error
   */
  const ping = () =>
    new Promise<void>((resolve, reject) => {
      client.ping({ requestTimeout: 3000 }, (error: Error) => {
        if (error) {
          const error = new Error("Elastic search is down!");
          return reject(error);
        }
        console.info("Elastic search is running...");
        resolve();
      });
    });

  /**
   * Parse the TSV file to elastic search mappings
   * @returns Promise<any[]>
   */
  const parseRecord = (values: string[]) =>
    values.reduce((prev, value, index) => {
      const key = index as keyof typeof fieldsMapping;
      const objectKey = fieldsMapping[key];
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

  /**
   * Create elastic search indexes if needed
   * @returns Promise<void>
   * @throw Error
   */
  const addESIndexes = () =>
    new Promise<void>((resolve, reject) => {
      console.info("Creating indexes for elastic search");
      client.indices.create(
        { index: "cities_index", includeTypeName: true, body: fieldsConfig },
        (error, response) => {
          if (error) {
            return reject(error);
          }
          console.info(
            `ElasticSearch indexes created successfully: ${response}`
          );
          resolve();
        }
      );
    });

  /**
   * Create elastic search record
   * @param record<any>
   * @returns
   */
  const addEsRecord = (record: unknown) => {
    return new Promise((resolve, reject) => {
      client.index(
        { index: "cities_index", type: "city_record", body: record },
        (err) => {
          if (err) {
            reject(record);
          }
          resolve(record);
        }
      );
    });
  };

  /**
   * Main ETL Job (Extract - Transform - Load)
   * @returns Promise<void>
   * @throw Error
   */
  const runETL = () =>
    new Promise<void>((resolve) => {
      console.info("Processing records...");

      const stream = fs.createReadStream(filePath, { encoding: "utf8" });
      const lineReader = readLine.createInterface({ input: stream });
      let isFirstLine = true;

      const handleExtractLine = (line: string) => {
        // Remove header
        if (isFirstLine) {
          isFirstLine = false;
          return;
        }

        const fields = line.split("\t");
        const record = parseRecord(fields);
        lineReader.emit("extracted", record);
      };

      const handleTransformLine = (record: unknown) => {
        /** ... */
        lineReader.emit("transformed", record);
      };

      const handleLoadLine = (record: unknown) => {
        const task = async () => {
          try {
            await addEsRecord(record);
            log.success += 1;
          } catch (error) {
            log.errors += 1;
            log.erroredRecords.push({ record, error });
          }
        };
        queue.add(task);
      };

      // Get line from file and parse into object
      lineReader.on("line", handleExtractLine);

      // Do all the necessary transformations if needed
      lineReader.on("extracted", handleTransformLine);

      // Load record into ElasticSearch
      lineReader.on("transformed", handleLoadLine);

      // When job is finished, prints the log results
      queue.on("idle", () => {
        const total = log.success + log.errors;
        console.info("\nAll records have been processed.");
        console.info(`- Success: ${log.success}`);
        console.info(`- Error: ${log.errors}`);
        console.info(`- Total: ${total}`);
        resolve();
      });
    });

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

export default EsSeed().bootstrap();
