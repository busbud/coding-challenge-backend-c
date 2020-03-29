export default class BuildQueryProxy {
  query = null;
  location = {};
  pivot = '10km';

  withQuery (newQuery) {
    this.query = newQuery;
    return this;
  }

  withLocation (newLocation) {
    this.location = newLocation;
    return this;
  }

  setPivot (newPivot) {
    this.pivot = newPivot;
    return this;
  }

  buildEsQuery () {
    return ({
      query: {
        bool: {
          must: {
            match: {
              ...this._searchName(),
            }
          },
          ...this._distanceFeature(),
        }
      },
      sort: [
        { _score: { order: 'desc' } },
      ],
    });
  }

  _distanceFeature () {
    const { latitude, longitude } = this.location;
    if (!latitude || !longitude) return {};
    return ({
      should: {
        distance_feature: {
          field: 'location',
          pivot: this.pivot,
          origin: `${latitude},${longitude}`,
        },
      },
    });
  }

  _searchName () {
    return ({
      name: {
        query: this.query,
        analyzer: 'pattern',
      },
    });
  }
}
