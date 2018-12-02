const lineToMap = require("../utils/tsv").lineToMap;

describe.only("TSV utils", () => {
  it("lineToMap should transform a line to an object according to the headers list", () => {
    const headers = ["id", "a", "b"];
    const line = "00001   valueOfA  valueOfb";
    expect(lineToMap(headers, line)).toEqual({
      id: "00001",
      a: "valueOfA",
      b: "valueOfb"
    });
  });
});
