import IndexingService from "./indexing-service.js";

class Suggestions {
  constructor(datasource) {
    this.datasource = datasource;
  }

  searchData = async (term, lat, long) => {
    if (!term) return [];
    if (!term.length) return [];
    this.data = await this.datasource.getData();
    const indexService = new IndexingService(this.data);
    const indexedData = await indexService.index();
    const results = await indexedData.search(term, lat, long);

    return results;
  };
}

export default Suggestions;
