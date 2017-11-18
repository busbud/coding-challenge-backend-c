import * as fs from 'fs';
import * as path from 'path';
import * as csv from 'fast-csv';
import PrefixTree from "./prefix-tree";

const DATA_FILENAME = 'cities_canada-usa.tsv';
const MIN_POPULATION = 5001;

export default function loadData() {
  return new Promise((resolve, reject) => {
    const stream = fs.createReadStream(path.join(__dirname, DATA_FILENAME));
    let totalCity = 0;
    let maxCharCount = 0;
    let longestName = null;
    let moreThan20CharNameCount = 0;
    const cityTree = new PrefixTree();

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
						allNames.push(...getAlternateNames(alt_name));
					}

					totalCity += allNames.length;
					({maxCharCount, longestName} = getMaxCharCount(allNames, maxCharCount, longestName));
					allNames.forEach((name) => {
						if ([...name].length > 20) {
							moreThan20CharNameCount++;
						}
					});

					allNames.forEach((name) => {
						cityTree.add(name.toLowerCase(), {
							name,
							lat,
							long,
							state: admin1,
							country,
						});
					});
				}
      })
      .on("end", function(){
				console.log(`Total ${totalCity} city names (including ASCII and alternate names) with above ${MIN_POPULATION-1} population.`);
				console.log(`Only ${moreThan20CharNameCount} names have more than 20 characters.`);
				console.log(`Longest name \"${longestName}\" has ${maxCharCount} characters`);
				return resolve(cityTree);
      });
  });
}

// maxCharCount: Current maximum number of characters in a name
// longestName: Name with current maximum number of characters
function getMaxCharCount(allNames, maxCharCount, longestName) {
	allNames.forEach((name) => {
		const charCount = [...name].length;
		if (charCount > maxCharCount) {
			maxCharCount = charCount;
			longestName = name;
		}
	});

	return {maxCharCount, longestName};
}

// tsv file loader loads list of alternate names as a string.
// The alternate names in the list are comma separated.
// Given such a string, returns an array of alternate names.
function getAlternateNames(alternateNamesString) {
	// Some alternate name has an unnecessary space character at the beginning.
	// That's why names are trimmed.
	return alternateNamesString.split(',').map((name) => name.trim());
}