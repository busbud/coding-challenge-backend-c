import { parse } from "csv";
import fs from "fs";
import DatasourceInterface from "./datasource.js";

class FileDatasource extends DatasourceInterface {
  constructor(path) {
    super();
    this.path = path;
  }

  getData() {
    return new Promise((resolve, reject) => {
      const parser = parse(
        { columns: true, delimiter: "\t", relaxQuotes: true },
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

export default FileDatasource;
