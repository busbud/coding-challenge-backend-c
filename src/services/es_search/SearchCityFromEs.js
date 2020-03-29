import _ from 'lodash';
import fs from 'fs';
import path from 'path';

import { DistanceHelper } from '../../utils';

import elasticSearchClient from './connection';
import BuildQueryProxy from './BuildQueryProxy';

function getFullName (obj) {
  const { name, admin1, country } = obj;
  return _.filter([name, admin1, country], (e) => !_.isEmpty(e.trim())).join(', ');
}

export default class SearchCityFromEs {
  constructor ({ indexName = 'canada_us_cities' } = {}) {
    this.indexName = indexName;
  }

  _processEsResult = (esRes) => {
    const { hits } = esRes.body.hits;
    const scores = _.map(hits, (h) => h._score);
    const maxScore = _.max(scores);
    return _.map(hits, (hit) => {
      const { _source, _score } = hit;
      const scaledScore = DistanceHelper.round2Decimal(_score / (maxScore * 1.2));
      const fullName = getFullName(_source);
      return {
        name: fullName,
        score: scaledScore,
        longitude: _source.lat,
        latitude: _source.long,
      };
    });
  };

  async createIndex () {
    const cityIndexBody = await this._loadCityIndexer();
    return elasticSearchClient.indices.create({
      index: this.indexName,
      body: cityIndexBody,
    });
  }

  async deleteIndex () {
    return await elasticSearchClient.indices.delete({
      index: this.indexName,
    });
  }

  async isIndexExists () {
    const res = await elasticSearchClient.indices.exists({ index: this.indexName });
    return res.statusCode === 200;
  }

  async _loadCityIndexer () {
    // TODO: Move this file to configuration
    const filehandle = await fs.promises.open(path.join(__dirname, './city_index.json'), 'r');
    const cityIndex = await filehandle.readFile();
    return JSON.parse((cityIndex));
  }

  async search ({ q: query, latitude, longitude }) {
    const q = query.trim();
    if (_.isEmpty(q)) return Promise.reject(new Error('q params is required'));
    let queryProxy = new BuildQueryProxy();
    queryProxy = queryProxy
      .withQuery(q)
      .withLocation({
        longitude,
        latitude,
      });
    const results = await elasticSearchClient.search({
      index: this.indexName,
      body: queryProxy.buildEsQuery(),
    })
      .then(this._processEsResult);
    return results;
  }
}
