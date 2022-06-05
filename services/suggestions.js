import IndexingService from "./indexing-service.js";
import ScoringService from "./scoring-service.js";

class Suggestions {
  indexedData = null;
  constructor(datasource) {
    this.datasource = datasource;
  }

  initialize = async () => {
    const data = await this.datasource.getData();
    const indexService = new IndexingService(data);
    this.indexedData = await indexService.index();
  };

  searchData = async (term, lat, long) => {
    if (!term) return [];
    if (!term.length) return [];

    const results = await this.indexedData.search(term);
    const scoreAndSort = new ScoringService();
    const scoredAndSorted = scoreAndSort.score(results, lat, long);

    return scoredAndSorted;
  };
}

export default Suggestions;
