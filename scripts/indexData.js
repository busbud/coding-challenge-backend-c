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

/// ------------------- MAIN

// some good pythonic memories :=-)
const [input, output] = getArgsFromCommand();

console.log(input, output);
indexCities(input).then(data => {
  // write the database back to file
  fs.writeFileSync(output, JSON.stringify(data, null, 4));
  console.log("DONE");
});
