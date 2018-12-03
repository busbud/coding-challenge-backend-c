const omit = require("lodash/fp/omit");
const { suggestFromObjectList } = require("../utils/suggestions/fromList");

const sampleDb = {
  list: [
    {
      id: "1",
      name: "London, OH, USA",
      latitude: "39.88645",
      longitude: "-83.44825"
    },
    {
      id: "2",
      name: "London, ON, Canada",
      latitude: "42.98339",
      longitude: "-81.23304"
    },

    {
      id: "3",
      name: "London, KY, USA",
      latitude: "37.12898",
      longitude: "-84.08326"
    },
    {
      id: "4",
      name: "Londontowne, MD, USA",
      latitude: "38.93345",
      longitude: "-76.54941"
    }
  ]
};

describe("Utilities to query city list", () => {
  const suggestions = suggestFromObjectList(sampleDb, "Montreal");
  const scores = suggestions.map(suggestion => suggestion.score);

  // scores must be sorted dsc
  expect(scores).toMatch(scores.sort((a, b) => b - a));

  expect(suggestions.map(omit("score"))).toMatch([
    { id: "2", name: "Montreal", latitude: "1.0", longitude: "2.0" },
    { id: "1", name: "Montreal", latitude: "3.0", longitude: "4.0" },
    {
      id: "3",
      name: "Montreal",
      latitude: "5.0",
      longitude: "6.0"
    }
  ]);
});
