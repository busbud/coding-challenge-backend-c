/** main search function in index */
function matchesFor(query) {
  if (query === "") return [];
  const directResult = index.index[query];
  return directResult ? index.objects[directResult.id] : [];
}

/** the main suggest function */
function suggest(db, query) {
  if (query !== "") {
    const matches = matchesFor(query);
    return {
      suggestions: matches
    };
  } else {
    return { suggestions: [] };
  }
}
