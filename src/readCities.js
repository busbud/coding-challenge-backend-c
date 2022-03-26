const fs = require('fs');

module.exports.readCities = function (filePath) {
    const valuesMatrix = fs
        .readFileSync(filePath)
        .toString()
        .split('\r\n')
        .map((line) => line.split('\t')); //TSV is for tab-separated-values

    const keyDefinitions = valuesMatrix[0];

    const dataRows = valuesMatrix.slice(1);

    const rowParser = buildRowParser(keyDefinitions);

    const cities = dataRows
        .filter((row) => row.length == keyDefinitions.length)
        .map(rowParser);

    return cities;
};

function buildRowParser(keyDefinitions) {
    return (row) =>
        row
            .map((entry, index) => [keyDefinitions[index], entry])
            .reduce(
                (city, valueTuple) => ({
                    ...city,
                    [valueTuple[0]]: valueTuple[1],
                }),
                {}
            );
}
