const validOptions = ['name', 'ascii', 'lat', 'long'];

function jsonFromTsv(data) {
    let rows = data.split("\n");
    let result = [];
    let columns = rows[0].split("\t");

    for (let i = 1; i < rows.length; i++) {
        let row = {};
        let currentLine = rows[i].split("\t");

        for (let [key, column] of columns.entries()) {
            if ((validOptions.includes(column))) {
                row[column] = currentLine[key];
            }
        }

        result.push(row);
    }

    return result;
}

function optionsFromParams(params) {
    let options = ['ascii'];

    const lat = params.latitude;
    const long = params.longitude;

    if (lat && long) {
        options.push('lat');
        options.push('long');
    }

    return options;
}

module.exports = {jsonFromTsv, optionsFromParams};