import fs from "fs";
import readLine from "readline";

const parse = line => {
    const parameters = line.split("\t");
    parameters[15] = parameters[15] === "" ? null : parameters[15];
    return parameters;
}

const fetchData = (callback, finalCallback) => {
    let firstLine = true;
    const lineReader = readLine.createInterface({
        input: fs.createReadStream(__dirname + "/../data/cities_canada-usa.tsv"),
    });
    lineReader.on("line", line => {
        if (firstLine) {
            firstLine = false;
        } else {
            callback(parse(line));
        }
    });
    lineReader.on("close", finalCallback);
}

export default fetchData;
