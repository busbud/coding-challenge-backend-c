import Papa from 'papaparse';
import fs from 'fs';

export default class Reader {
    public init(path: string, fields: string[]): void {
        const fileContent = fs.readFileSync(path, 'utf8');

        Papa.parse(fileContent, {
            delimiter: '\t',
            header: true,
            //fields: ['id','name', 'ascii', 'alt_name', 'lat', 'long', 'feat_class', 'feat_code', 'country', 'cc2', 'admin1', 'admin2', 'admin3', 'admin4', 'population', 'elevation', 'dem', 'tz', 'modified_at'],
            step: function(results) {
                console.log("Row data:", results.data);
            },
            complete: function(results) {
                //console.log("Finished:", results.data);
            }
        });
    }
}
