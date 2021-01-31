const test = require("ava");
const path = require("path");
const { readFile } = require("../../src/utils/fileReader");

test.serial("should readFile", async (t) => {
  const result = await readFile(path.resolve(__dirname, "readFile.txt"));
  t.is(result, "Hi!");
});
