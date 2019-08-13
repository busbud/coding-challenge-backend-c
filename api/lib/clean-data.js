const fs = require('fs');
const readline = require('readline');


const canadianProvinces = {
  '01': 'AB',
  '02': 'BC',
  '03': 'MB',
  '04': 'NB',
  '05': 'NL',
  '07': 'NS',
  '08': 'ON',
  '09': 'PE',
  10: 'QC',
  11: 'SK',
  12: 'YT',
  13: 'NT',
  14: 'NU'
};
/* eslint no-tabs: ["error", { allowIndentationTabs: true }] */
const headerLine = 'id\tname\tatitude\tlongitude\tcountry\tprov_state\n';

const columnNames = {
  id: 0,
  name: 1,
  latitude: 4,
  longitude: 5,
  country: 8,
  provState: 10
};


const srcCityData = readline.createInterface({
  terminal: false,
  input: fs.createReadStream('./data/cities_canada-usa.tsv')
});



let lineCount = 0;
srcCityData.on('line', function(line) {
  lineCount++;
  if (lineCount === 1) {
    writeToFile(headerLine);
    return;
  }

  console.log(line);
  const columnData = line.split('\t');
  const provState = (columnData[columnNames.country] === 'CA') ? canadianProvinces[columnData[columnNames.provState]] : columnData[columnNames.provState];
  let cityRow = `${columnData[columnNames.id]}\t${columnData[columnNames.name]}\t${columnData[columnNames.latitude]}`;
  cityRow += `\t${columnData[columnNames.longitude]}\t${columnData[columnNames.country]}\t${provState}\n`;
  writeToFile(cityRow);
});

const writeToFile = function(line) {
  fs.writeFile('./data/cities_canada-usa-clean.tsv', line, { flag: 'a' }, (err) => {
    // throws an error, you could also catch it here
    if (err) throw err;

    // success case, the file was saved
  });
};
