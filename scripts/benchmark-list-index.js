const index = require("../data/db.json");
const { suggestFromList } = require("../utils/suggestions/fromList");
const { suggestFromIndex } = require("../utils/suggestions/fromIndex");

/* istanbul ignore next */
function forceGC() {
  if (global.gc) {
    global.gc();
  } else {
    console.warn(
      "No GC hook! Start your program as `node --expose-gc file.js`."
    );
  }
}

function benchmark(suggester, id) {
  const letters = "abcdefghijklmnopqrstuvwxz".split("");
  const benchmarkId = "suggester-id-" + id;
  console.log("benchmark", benchmarkId);
  console.time(benchmarkId, id);
  for (const letter of [...letters, ...letters, ...letters, ...letters]) {
    suggester(index, letter);
  }
  console.timeEnd(benchmarkId, id);
}

benchmark(suggestFromList, "list");

forceGC();

benchmark(suggestFromIndex, "index");
