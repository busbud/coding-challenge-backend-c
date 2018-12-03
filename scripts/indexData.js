const { indexCities } = require("../utils/cities");
/** the arguments provided */

const argv = process.argv;
const fs = require("fs");

// UTILS FUNCTION
/**
 * display help message
 */
function help(message = "") {
  console.log(
    "USAGE node indexData inputTsvFilePath outputJsonIndexPath \n",
    message !== "" ? `Error: ${message}` : ""
  );
  process.exit(-1);
}

/** retrieve args information from cli */
function getArgsFromCommand() {
  if (argv.length < 4) help();
  const input = argv[2];
  const output = argv[3];
  return [input, output];
}

function checkData(data) {
  return data.cities.every(item =>
    ["latitude", "longitude"].every(key => !isNaN(parseFloat(item[key])))
  );
}

/// ------------------- MAIN

// some good pythonic memories :=-)
const [input, output] = getArgsFromCommand();

indexCities(input).then(data => {
  // write the database back to file
  if (checkData(data)) {
    console.log("Data has been checked");
  }
  fs.writeFileSync(output, JSON.stringify(data, null, 4));
  console.log("Indexed ", Object.keys(data.objects).length, " cities");
});
