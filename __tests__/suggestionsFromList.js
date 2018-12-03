const omit = require("lodash/fp/omit");
const { suggestFromList } = require("../utils/suggestions/fromList");

const sampleDb = {
  cities: [
    {
      id: "1",
      canonicalName: "london",
      onlyName: "London",
      name: "London, OH, USA",
      latitude: "39.88645",
      longitude: "-83.44825"
    },
    {
      id: "2",
      canonicalName: "london",
      onlyName: "London",
      name: "London, ON, Canada",
      latitude: "42.98339",
      longitude: "-81.23304"
    },

    {
      id: "3",
      canonicalName: "london",
      onlyName: "London",
      name: "London, KY, USA",
      latitude: "37.12898",
      longitude: "-84.08326"
    },
    {
      id: "4",
      canonicalName: "londontowne",
      onlyName: "Londontowne",
      name: "Londontowne, MD, USA",
      latitude: "38.93345",
      longitude: "-76.54941"
    }
  ]
};

describe("Suggestions without index", () => {
  it("should return cities matching the begin of the query, sorted by levenstein => no geo reference involved", () => {
    const suggestions = suggestFromList(sampleDb, "London").suggestions;
    const scores = suggestions.map(suggestion => suggestion.score);

    expect(scores).toEqual([1, 1, 1, 0.17]);
    expect(suggestions.map(omit("score"))).toEqual([
      {
        id: "1",
        canonicalName: "london",
        onlyName: "London",
        name: "London, OH, USA",
        latitude: "39.88645",
        longitude: "-83.44825"
      },
      {
        id: "2",
        canonicalName: "london",
        onlyName: "London",
        name: "London, ON, Canada",
        latitude: "42.98339",
        longitude: "-81.23304"
      },
      {
        id: "3",
        canonicalName: "london",
        onlyName: "London",
        name: "London, KY, USA",
        latitude: "37.12898",
        longitude: "-84.08326"
      },
      {
        id: "4",
        canonicalName: "londontowne",
        onlyName: "Londontowne",
        name: "Londontowne, MD, USA",
        latitude: "38.93345",
        longitude: "-76.54941"
      }
    ]);
  });
  it("should weight  near cities if a refence point is provided", () => {
    const suggestions = suggestFromList(sampleDb, "London", {
      latitude: "42.98",
      longitude: "-81.24"
    }).suggestions;
    const scores = suggestions.map(suggestion => suggestion.score);

    expect(scores).toEqual([0.75, 0.51, 0.44, 0.04]);
    expect(suggestions.map(omit("score"))).toEqual([
      {
        canonicalName: "london",
        id: "2",
        latitude: "42.98339",
        longitude: "-81.23304",
        name: "London, ON, Canada",
        onlyName: "London"
      },
      {
        canonicalName: "london",
        id: "1",
        latitude: "39.88645",
        longitude: "-83.44825",
        name: "London, OH, USA",
        onlyName: "London"
      },
      {
        canonicalName: "london",
        id: "3",
        latitude: "37.12898",
        longitude: "-84.08326",
        name: "London, KY, USA",
        onlyName: "London"
      },
      {
        canonicalName: "londontowne",
        id: "4",
        latitude: "38.93345",
        longitude: "-76.54941",
        name: "Londontowne, MD, USA",
        onlyName: "Londontowne"
      }
    ]);
  });
});
