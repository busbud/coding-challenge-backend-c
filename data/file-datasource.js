import { parse } from "csv";
import fs from "fs";
import DatasourceInterface from "./datasource.js";

/**
 * FileDatasource: implementation of DatasourceInterface to extract data from a file path.
 */
export default class FileDatasource extends DatasourceInterface {
  constructor(path) {
    super();
    this.path = path;
  }

  /**
   * getData: parses the data from a tsv file.
   * @returns Promise<Array>
   */
  getData() {
    return new Promise((resolve, reject) => {
      const parser = parse(
        { columns: true, delimiter: "\t", quote: "" },
        (err, records) => {
          if (err) {
            reject(err);
          }

          resolve(records);
        }
      );
      fs.createReadStream(this.path).pipe(parser);
    });
  }
}
