import fs from 'fs';
import readline from 'readline';
import { Observable } from 'rxjs';

export default class TsvParser {
  header = [];

  constructor ({ filePath }) {
    this.filePath = filePath;
  }

  /*
  * Parse and convert it to object
  * @returns Observable<Object>
  * */
  parse () {
    return Observable.create(observer => {
      const inStream = fs.createReadStream(this.filePath);
      const rl = readline.createInterface({ input: inStream });
      let row = 0;
      rl.on('line', (line) => {
        const datum = line.trim().split('\t');
        if (row === 0) {
          this.header = datum;
        } else {
          const cityObj = this._convertToObject(datum);
          observer.next(cityObj);
        }
        row++;
      });
      rl.on('close', function () {
        observer.complete();
      });
    });
  }

  _convertToObject (datum) {
    const obj = {};
    const header = this.header;
    for (let i = 0; i < header.length; i++) {
      obj[header[i]] = datum[i];
    }
    return obj;
  }
}
