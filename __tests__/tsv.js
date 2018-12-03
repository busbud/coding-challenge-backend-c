const lineToMap = require("../utils/tsv").lineToMap;

describe.only("TSV utils", () => {
  it("lineToMap should transform a line to an object according to the headers list, the input transformer, and a delimiter", () => {
    const headers = ["id", "a", "b"];
    const line = "00001  valueOfA\tvalueOfb";
    const twoSpacesToTab = feat => feat.replace(/ {2}/g, "\t");
    expect(lineToMap(twoSpacesToTab, "\t", headers, line)).toEqual({
      id: "00001",
      a: "valueOfA",
      b: "valueOfb"
    });
  });
});
