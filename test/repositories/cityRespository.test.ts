import { stub } from "sinon";
import { expect } from "chai";
import * as fs from "fs";
import { CityRepository } from "../../repositories/cityRepository";

const mockValidFile = `8605040	Hot Springs National Park	Hot Springs National Park		34.51342	-93.05429	P	PPL	US		AR	051			35193		212	America/Chicago	2013-09-10
8605041	Dixiana	Dixiana		33.74021	-86.64938	P	PPL	US		AL	073			22940		222	America/Chicago	2013-09-10
8643098	Cranberry Township	Cranberry Township		40.68496	-80.10714	P	PPL	US		PA	019			28098		317	America/New_York	2013-11-19`;

const mockInvalidFile = `8605040	Hot Springs National Park	Hot Springs National Park		34.51342	-93.05429	P	PPL	US		AR	051			35193		212	America/Chicago	2013-09-10

8643098	Cranberry Township	Cranberry Township		40.68496	-80.10714	P	PPL	US		PA	019			28098		317	America/New_York	2013-11-19`;

const expectedParsedArray = [
  {
    admin1: "AR",
    admin2: "051",
    admin3: "",
    admin4: "",
    alt_name: "",
    ascii: "Hot Springs National Park",
    cc2: "",
    country: "US",
    dem: "212",
    elevation: 0,
    feat_class: "P",
    feat_code: "PPL",
    id: 8605040,
    lat: 34.51342,
    long: -93.05429,
    modified_at: new Date("2013-09-10T00:00:00.000Z"),
    name: "Hot Springs National Park",
    population: 35193,
    tz: "America/Chicago"
  },
  {
    admin1: "AL",
    admin2: "073",
    admin3: "",
    admin4: "",
    alt_name: "",
    ascii: "Dixiana",
    cc2: "",
    country: "US",
    dem: "222",
    elevation: 0,
    feat_class: "P",
    feat_code: "PPL",
    id: 8605041,
    lat: 33.74021,
    long: -86.64938,
    modified_at: new Date("2013-09-10T00:00:00.000Z"),
    name: "Dixiana",
    population: 22940,
    tz: "America/Chicago"
  },
  {
    admin1: "PA",
    admin2: "019",
    admin3: "",
    admin4: "",
    alt_name: "",
    ascii: "Cranberry Township",
    cc2: "",
    country: "US",
    dem: "317",
    elevation: 0,
    feat_class: "P",
    feat_code: "PPL",
    id: 8643098,
    lat: 40.68496,
    long: -80.10714,
    modified_at: new Date("2013-11-19T00:00:00.000Z"),
    name: "Cranberry Township",
    population: 28098,
    tz: "America/New_York"
  }
];

describe("CityRepository", function() {
  describe("When called with a valid file", () => {
    let stubFs;
    let cityRepository;

    before(() => {
      stubFs = stub(fs, "readFileSync").returns(mockValidFile);
      cityRepository = new CityRepository();
    });

    after(() => {
      stubFs.restore();
    });

    it("returns the correct suggestions", () => {
      expect(cityRepository.getAll()).to.deep.equal(expectedParsedArray);
    });
  });

  describe("When called with an invalid file", () => {
    let stubFs;
    let cityRepository;

    before(() => {
      stubFs = stub(fs, "readFileSync").returns(mockInvalidFile);
      cityRepository = new CityRepository();
    });

    after(() => {
      stubFs.restore();
    });

    it("throws", () => {
      expect(() => cityRepository.getAll()).to.throw(
        /A city row in the file cities_canada-usa.tsv has an invalid number of columns./
      );
    });
  });
});
