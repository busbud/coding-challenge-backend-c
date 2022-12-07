const argv = require("yargs").parse();
const fs = require("fs");
const ora = require("ora");
const fsExtra = require("fs-extra");
const beautify = require("js-beautify").js;

const { post, p: endpointPath, public } = argv;

const [section, endpoint] = endpointPath.split("/");

const spinner = ora(`Adding new Endpoint to ${section}`).start();

const path = `endpoints/${section}/${endpoint}`;

if (fs.existsSync(path)) {
  spinner.fail("This endpoint already exists");
  return;
}

//copy the endpoint template
fsExtra
  .copy("templates/endpoint/", path)
  .then(() => {
    spinner.text = "Configuring the new endpoint";
    // get the empty config json
    const config = JSON.parse(fs.readFileSync(`${path}/config.json`));

    config.Method = (post && "POST") || "GET";

    // rewrites the config with the endpoint config
    fs.writeFileSync(
      `${path}/config.json`,
      JSON.stringify(config, null, 2),
      "utf8"
    );

    // rename the main unit by the endpoint name
    fsExtra.moveSync(
      `${path}/units/endpoint.js`,
      `${path}/units/${endpoint}.js`
    );

    // update the units index file
    fs.writeFileSync(
      `${path}/units/index.js`,
      `module.exports.${endpoint} = require("./${endpoint}.js")`,
      "utf8"
    );

    // update the main file
    // if (!public)
    //   fs.writeFileSync(
    //     `${path}/index.js`,
    //     beautify(`const { ${endpoint} } = require("./units");
    //   const auth = require("_app/auth");

    //   module.exports = async ({req,res}) => {
    //     return await auth({req}, async ({ userId, accessToken, app }) => {
    //       return await ${endpoint}();
    //     });
    //   };`),
    //     "utf8"
    //   );
    // else
    //   fs.writeFileSync(
    //     `${path}/index.js`,
    //     beautify(`const { ${endpoint} } = require("./units");

    //   module.exports = async ({ req, res}) => {
    //       return await ${endpoint}();
    //   };`),
    //     "utf8"
    //   );

    fs.writeFileSync(
      `${path}/index.js`,
      beautify(`const { ${endpoint} } = require("./units");
    
    module.exports = async ({ req, res}) => {
        return await ${endpoint}();
    };`),
      "utf8"
    );

    spinner.succeed(`Successfully added endpoint ${endpoint} to ${section}`);
  })
  .catch((err) => console.log(err));
