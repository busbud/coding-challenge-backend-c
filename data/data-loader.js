import * as fs from 'fs';
import * as path from 'path';
import * as csv from 'fast-csv';

const DATA_FILENAME = 'cities_canada-usa.tsv';
const MIN_POPULATION = 5001;

export default function loadData() {
  return new Promise((resolve, reject) => {
    const stream = fs.createReadStream(path.join(__dirname, DATA_FILENAME));
    let count = 0;
    csv
      .fromStream(stream, { headers : true, delimiter: '\t', quote: null })
      .on("data", function(data){
				const { name, ascii, alt_name, lat, long, admin1, country, population } = data;
				if (population >= MIN_POPULATION) {
					const allNames = [name];
					if (ascii) {
						allNames.push(ascii);
					}
					if (alt_name) {
						allNames.push(getAlternateNames(alt_name));
					}

					count += allNames.length;
				}
      })
      .on("end", function(){
				console.log(`${count} city names with above ${MIN_POPULATION-1} population loaded.`);
				return resolve();
      });
  });
}

function getAlternateNames(alternateNamesString) {
	return alternateNamesString.split(',').map((name) => name.trim());
}