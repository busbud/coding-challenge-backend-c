jest.mock("line-reader", () => {
  const lineWalker = data => {
    return (fpath, cb) => {
      let index = 0;
      for (const line of data) {
        const last = index === data.length - 1;
        cb(line, last);
        index++;
      }
    };
  };
  const exampleTsvData = [
    "id	name	ascii	alt_name	lat	long	feat_class	feat_code	country	cc2	admin1	admin2	admin3	admin4	population	elevation	dem	tz	modified_at",
    "5882142	Acton Vale	Acton Vale		45.65007	-72.56582	P	PPL	CA		10	16			5135		90	America/Montreal	2008-04-11",
    "5889745	Baie-Comeau	Baie-Comeau	Baie-Comeau,Be Komo,Be-Komo,YBC,bay kamyw,bh-kwmw,byh kwmw, kybk,Бе Комо,Бе-Комо,بائ کامیو,به-کومو,بيه كومو، كيبك	49.21679	-68.14894	P	PPL	CA		10	09			29808		76	America/Montreal	2013-08-17"
  ];
  return {
    eachLine: lineWalker(exampleTsvData)
  };
});
const { sanitizeString, indexCities, isCityValid } = require("../utils/cities");

describe("isCityValid", () => {
  it("should return true if the population exceed the threshold", () => {
    expect(isCityValid({ population: "4900" }, 5000)).toBeFalsy();
    expect(isCityValid({ population: "5100" }, 5000)).toBeTruthy();
    expect(isCityValid({ population: "54x9" }, 5000)).toBeFalsy();
  });
});

describe("sanitizeString", () => {
  it("should remove all whitespaces", () => {
    expect(sanitizeString("à is véry    goôd to bî ù")).toEqual(
      "aisverygoodtobiu"
    );
  });
});

describe("indexData", () => {
  it("should transform a tsv file to an array of objects", () => {
    return indexCities("/path/to").then(data => {
      expect(data).toEqual({
        cities: [
          {
            canonicalName: "actonvale",
            latitude: "45.65007",
            longitude: "-72.56582",
            name: "Acton Vale, QC, CA",
            onlyName: "Acton Vale"
          },
          {
            canonicalName: "baiecomeau",
            latitude: "49.21679",
            longitude: "-68.14894",
            name: "Baie-Comeau, QC, CA",
            onlyName: "Baie-Comeau"
          }
        ],
        index: {},
        objects: {
          "5882142": {
            canonicalName: "actonvale",
            latitude: "45.65007",
            longitude: "-72.56582",
            name: "Acton Vale, QC, CA",
            onlyName: "Acton Vale"
          },
          "5889745": {
            canonicalName: "baiecomeau",
            latitude: "49.21679",
            longitude: "-68.14894",
            name: "Baie-Comeau, QC, CA",
            onlyName: "Baie-Comeau"
          }
        }
      });
    });
  });
});
