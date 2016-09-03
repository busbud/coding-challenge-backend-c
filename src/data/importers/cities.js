import redisCreateCity from '../../services/cities/redisCreateCity'

const DEFAULT_HEADERS = [
  'id',
  'name',
  'ascii',
  'alt_name',
  'lat',
  'long',
  'feat_class',
  'feat_code',
  'country',
  'cc2',
  'admin1',
  'admin2',
  'admin3',
  'admin4',
  'population',
  'elevation',
  'dem',
  'tz',
  'modified_at'
]

const extractCity = (chunk, headers = []) => {
  return headers.reduce((city, attr, i) => {
    city[attr] = chunk[i]
    return city
  }, {})
}

const same = (a, b) => a.every((v, i) => v === b[i])

export default {
  type: 'cities',
  headers: DEFAULT_HEADERS,

  createCity(city) {
    return redisCreateCity(city)
  },

  preProcess(importData) {
    const cities = []
    const {tsv} = importData

    if (!tsv) {
      return importData
    }

    tsv.forEach(({data}, k) => {
      if (Array.isArray(data) && same(this.headers, data[0])) {
        const l = data.length
        for (let i = 1; i < l; i++) {
          cities.push(extractCity(data[i], this.headers))
        }
        importData.tsv[k].type = this.type
        importData.tsv[k].data = cities
      }
    })

    return importData
  },

  doImport(importData) {
    const {tsv} = importData

    if (!tsv) {
      return Promise.resolve([])
    }

    const opts = tsv.map((tsvData, i) => {
      const {data, type} = tsvData
      if (type === 'cities') {
        return data.map((d) => this.createCity(d))
      }
    })

    return Promise.all([].concat.apply([], opts))
  }
}
