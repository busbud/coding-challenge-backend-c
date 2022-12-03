const argv = require("yargs").parse();
const fs = require("fs");
const ora = require("ora");
const fsExtra = require("fs-extra");

const { p: newUnitPath } = argv;

const [section, endpoint, unit] = newUnitPath.split("/");

const spinner = ora(`Adding new unit to ${section}/${endpoint}`).start();

const path = `controllers/${section}/${endpoint}`;

// make sure section exists
if (!fs.existsSync(`controllers/${section}`)) {
  spinner.fail("This section does not exist");
  return;
}

// make sure endpoint exists
if (!fs.existsSync(`controllers/${section}/${endpoint}`)) {
  spinner.fail("This endpoint does not exist");
  return;
}

if (fs.existsSync(`${path}/units/${unit}.js`)) {
  spinner.fail(`This unit already exists in ${section}/${endpoint}`);
  return;
}

//copy the endpoint template
fsExtra
  .copy("templates/endpoint/units/endpoint.js", `${path}/units/${unit}.js`)
  .then(() => {
    spinner.text = "Configuring the new unit";

    // update the units index file
    fs.appendFileSync(
      `${path}/units/index.js`,
      `\nmodule.exports.${unit} = require("./${unit}.js")`,
      "utf8"
    );

    spinner.succeed(
      `Successfully added new unit ${unit} to ${section}/${endpoint}`
    );
  })
  .catch((err) => console.log(err));
