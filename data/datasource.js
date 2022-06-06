/**
 * DatasourceInterface intent is to mock an Interface to support different implementations for getting
 * data.
 */
class DatasourceInterface {
  constructor() {
    if (!this.getData) throw new Error("getData must be implemented");
  }
}

export default DatasourceInterface;
