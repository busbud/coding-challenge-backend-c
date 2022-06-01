import DatasourceInterface from "./datasource.js";

class FileDatasource extends DatasourceInterface {
  constructor(data) {
    super();
    this.data = data;
  }

  getData() {
    return this.data;
  }
}

export default FileDatasource;
