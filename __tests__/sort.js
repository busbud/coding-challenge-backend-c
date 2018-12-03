const { sortByNumericalField } = require("../utils/sort");

describe("sortByNumericalField", () => {
  it("should return an array of predicates sorted by the value of field at the given key", () => {
    const startData = [{ a: 3 }, { a: 1 }, { a: 2 }];
    const ascSorted = [{ a: 1 }, { a: 2 }, { a: 3 }];
    const dscSorted = [{ a: 3 }, { a: 2 }, { a: 1 }];
    const natSorted = [{ a: 3 }, { a: 1 }, { a: 2 }];

    expect(sortByNumericalField("ASC", "a", startData)).toEqual(ascSorted);
    expect(sortByNumericalField("DSC", "a", startData)).toEqual(dscSorted);
    expect(sortByNumericalField(null, "a", startData)).toEqual(natSorted);
  });
});
