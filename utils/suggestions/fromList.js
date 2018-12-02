/** first method of suggestion, run the regexp on city list */
function suggestFromObjectList(db, query) {
  if (query !== "") {
    const matches = db.cities.filter(city => {
      return new RegExp(query).test(city.name);
    });
    return {
      suggestions: matches
    };
  } else {
    return { suggestions: [] };
  }
}

module.exports = {
  suggestFromObjectList
};
