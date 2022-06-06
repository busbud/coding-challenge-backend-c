import DatasourceInterface from "./datasource.js";

/**
 * InMemoryDatasource: implementation of DatasourceInterface to return data from in memory.
 */
export default class InMemoryDatasource extends DatasourceInterface {
  constructor(data) {
    super();
    this.data = data;
  }

  getData() {
    return this.data;
  }
}
