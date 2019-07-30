const expect = require("chai").expect;
const {
  composeStream,
  composeObjectReduce,
  composeMap
} = require("./../src/fp");
const { Transform, Readable } = require("stream");

const getMockReadStream = objectMode => {
  const read = new Readable({
    objectMode: objectMode,
    read() {}
  });
  read.push("a");
  read.push(null);
  return read;
};

const appendLetter = letter => {
  return new Transform({
    transform(chunk, encoding, cb) {
      cb(null, chunk.toString() + letter);
    }
  });
};

describe("Test abstract functinal programing", () => {
  describe("Test composeStream", () => {
    it("applies all transforms", () => {
      const promise = new Promise(resolve => {
        composeStream(appendLetter("b"), appendLetter("c"))(
          getMockReadStream(false)
        ).on("data", data => {
          resolve(data.toString());
        });
      });

      return promise.then(data => {
        expect(data.toString()).to.eql("abc");
      });
    });

    describe("test composeObjectReduce", () => {
      it("applies all functions", () => {
        const fn = key => (acc, obj) => acc + obj[key];
        const result = composeObjectReduce(fn("a"), fn("b"))({ a: 1, b: 2 })(3);
        expect(result).to.eql(6);
      });
    });

    describe("test composeMap", () => {
      it("process the array through n functions pass as parameter", () => {
        const array = [1, 2];
        const add = x => value => value + x;

        result = composeMap(add(1), add(2))(array);

        expect(result).to.eql([4, 5]);
      });

      it("process the array of object through n functions pass as parameter", () => {
        const array = [
          {
            firstname: "Yvon",
            lastName: "Gagné"
          },
          {
            firstname: "Nicole",
            lastName: "Tremblay"
          }
        ];

        const fullName = obj => {
          obj.fullName = obj.firstname + " " + obj.lastName;
          return obj;
        };

        const toUpper = obj => {
          obj.fullName = obj.fullName.toUpperCase();
          return obj;
        };

        result = composeMap(fullName, toUpper)(array);

        expect(result[0].fullName).to.equal("yvon gagné".toUpperCase());
        expect(result[1].fullName).to.equal("nicole tremblay".toUpperCase());
      });
    });
  });
});
