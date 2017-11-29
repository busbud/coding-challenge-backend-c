import QueryBuilder from '../database/QueryBuilder';

export default class CitiesRepository {
  constructor(database) {
    this.database = database;
  }

  readStream = params => {
    const qb = new QueryBuilder();
    return this.database.runQuery(qb.build(params));
  };
}
