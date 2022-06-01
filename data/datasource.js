class DatasourceInterface {
  constructor() {
    if (!this.getData) throw new Error("datasource must return data");
  }
}

export default DatasourceInterface;
