const test = require("ava");
const path = require("path");
const { readFile } = require("../../src/utils/fileReader");
const { tsv2JSON } = require("../../src/utils/tsvJson");

test.serial("should tsv2JSON", async (t) => {
  const tsvContent = await readFile(path.resolve(__dirname, "tsvFile.tsv"));
  const result = await tsv2JSON(tsvContent);
  t.is(result.length, 3);
  t.is(result[1].id, "5882142");
});
