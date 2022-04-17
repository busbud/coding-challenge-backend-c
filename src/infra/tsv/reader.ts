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
            }
        });

        return records;
    }
}
