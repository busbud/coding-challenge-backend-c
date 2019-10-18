const fs = require('fs');
const readLine = require('readline');

module.exports = { readTsvAsJson };

async function readTsvAsJson(filePath) {
  const cityDataStream = fs.createReadStream(filePath, { encoding: 'utf-8' });
  const lineReader = readLine.createInterface({
    input: cityDataStream,
    // Used in order to recognize CR LF as a single line break
    crlfDelay: Infinity
  });
  let isFirstLine = true;
  const cityHeaders = [];
  const citiesData = [];
  for await (const line of lineReader) {
    const lineParts = line.split('\t');
    if (isFirstLine) {
      cityHeaders.push(...lineParts);
      isFirstLine = false;
    } else if (cityHeaders.length !== lineParts.length) {
      console.warn(`Invalid data package ${JSON.stringify(lineParts)}`);
    } else {
      citiesData.push(parseRow(cityHeaders, lineParts));
    }
  }

  return citiesData;
}

function parseRow(dataHeaders, data) {
  const cityData = {};
  if (data.length !== dataHeaders.length) {
    throw new Error('Data headers and data package are not');
  }
  for (let i = 0; i < dataHeaders.length; i++) {
    const field = dataHeaders[i];
    cityData[field] = data[i];
  }

  return cityData;
}