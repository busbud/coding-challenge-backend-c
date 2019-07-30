var { expect } = require("chai");
const { Readable } = require("stream");
const { streamFilter, streamMap } = require("../src/transforms");
const { composeStream } = require("../src/fp");

describe("Test transforms", () => {
  const getMockReadStream = () => {
    const read = new Readable({
      objectMode: true,
      read() {}
    });
    read.push({ a: 1 });
    read.push(null);
    return read;
  };

  describe("Test rowFilter", () => {
    it("apply a filter function that keep the row when the function return true", () => {
      const result = [];
      const filterFn = row => row.a === 1;
      getMockReadStream()
        .pipe(streamFilter(filterFn))
        .on("data", data => {
          result.push(data);
        })
        .on("end", () => {
          expect(result.length).to.equal(1);
        });
    });

    it("apply a filter function that discard the row when the function return false", () => {
      const result = [];
      const filterFn = row => row.a === 2;
      getMockReadStream()
        .pipe(streamFilter(filterFn))
        .on("data", data => {
          result.push(data);
        })
        .on("end", () => {
          expect(result.length).to.equal(0);
        });
    });
  });

  describe("Test rowMap", () => {
    it("replace the chunk with the returned value from the callback", () => {
      let result;
      const mapFn = row => {
        row.b = 2;
        return row;
      };
      getMockReadStream()
        .pipe(streamMap(mapFn))
        .on("data", data => (result = data))
        .on("end", () => expect(result.b).to.equal(2));
    });
  });

  describe("Test pipeTransforms", () => {
    it("pipes n1 n2 n... transform streams", () => {
      let result;
      const fn1 = row => {
        row.a += 1;
        return row;
      };
      const fn2 = row => {
        row.a += 2;
        return row;
      };
      composeStream(streamMap(fn1), streamMap(fn2))(getMockReadStream())
        .on("data", data => (result = data))
        .on("end", () => {
          expect(result.a).to.equal(4);
        });
    });
  });
});
