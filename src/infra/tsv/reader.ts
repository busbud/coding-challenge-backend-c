import Papa from 'papaparse';
import fs from 'fs';

export default class Reader {
    public read(path: string): unknown[] {
        const fileContent = fs.readFileSync(path, 'utf8');
        let records: unknown[];

        Papa.parse(fileContent, {
            delimiter: '\t',
            header: true,
            complete: function(results: Papa.ParseResult<unknown>) {
                records = results.data;
            },
            /*error: function(err, file, inputElem, reason) {
                // executed if an error occurs while loading the file,
                // or if before callback aborted for some reason
            }*/
        });
        //@todo: error management

        return records;
    }
}
