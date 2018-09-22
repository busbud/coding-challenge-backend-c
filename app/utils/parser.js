/**
 *
 * @param {String} tsv
 * @param {Function} validator Optional function to validate lines to be added
 * @param {Array<String>} header Optional headers
 * @return {Object}
 */
const tsvToJson = (tsv, validator, header) => {
    const lines = tsv.split('\n');
    header = header || lines.shift().split('\t');
    const json = [];

    lines.forEach((line) => {
        // Line may be empty, like the last line of a file
        if (!line || !line.length) {
            return;
        }
        const res = {};
        line
            .split('\t')
            .forEach((elem, idx) => {
                res[header[idx]] = elem;
            });

        if (!validator || (typeof validator === 'function' && validator(res))) {
            json.push(res);
        }
    });

    return json;
};

module.exports = {
    tsvToJson,
};
