const fs = require('fs')
const readline = require('readline');
const {once} = require('events');
const {tsv2json} = require('tsv-json');

export type City = {
  score?: number
  distance?: number
  id: number
  name: string
  ascii: string
  alt_name: string
  lat: number
  long: number
  feat_class: string
  feat_code: string
  country: string
  cc2: string
  admin1: number
  admin2: number
  admin3: number
  admin4: number
  population: number
  elevation: number
  dem: number
  tz: string
  modified_at: string
}

enum COLUMN_TYPE_ID {
  ID,
  NAME,
  ASCII,
  ALT_NAME,
  LAT,
  LONG,
  FEAT_CLASS,
  FEAT_CODE,
  COUNTRY,
  CC2,
  ADMIN1,
  ADMIN2,
  ADMIN3,
  ADMIN4,
  POPULATION,
  ELEVATION,
  DEM,
  TZ,
  MODIFIED_AT
}

export class CityRepository {
  private _cities: Array<City>;

  constructor() {
    this._cities = [];
  }

  setCities(cities: Array<City>) {
    this._cities = cities
  }

  getCities() {
    return this._cities
  }

  /**
   * Read a file (TSV format) and load the cities
   * @param {string} filename TSV file
   * @returns {Promise<void>} List of cities
   */
  loadCities = async (filename: string): Promise<void> => {
    const cities: Array<City> = []
    const rd = readline.createInterface({
      input: fs.createReadStream(filename),
      console: false
    });

    let pos = 0;
    rd.on('line', (line: string) => {
      if (pos > 0) {
        let city = this.toCityModel(tsv2json(line)[0]);
        cities.push(city)
      }
      pos++
    });

    await once(rd, 'close');
    this.setCities(cities)
  }

  /**
   * Convert a list of chars to our data model
   * @param {string[]} line A string array that contains all data for our model
   * @returns {City} An object based
   */
  toCityModel = (line: string[]): City => {
    return {
      id: +line[COLUMN_TYPE_ID.ID],
      admin1: +line[COLUMN_TYPE_ID.ADMIN1],
      admin2: +line[COLUMN_TYPE_ID.ADMIN2],
      admin3: +line[COLUMN_TYPE_ID.ADMIN3],
      admin4: +line[COLUMN_TYPE_ID.ADMIN4],
      alt_name: line[COLUMN_TYPE_ID.ALT_NAME],
      ascii: line[COLUMN_TYPE_ID.ASCII],
      cc2: line[COLUMN_TYPE_ID.CC2],
      country: line[COLUMN_TYPE_ID.COUNTRY],
      dem: +line[COLUMN_TYPE_ID.DEM],
      elevation: +line[COLUMN_TYPE_ID.ELEVATION],
      feat_class: line[COLUMN_TYPE_ID.FEAT_CLASS],
      feat_code: line[COLUMN_TYPE_ID.FEAT_CODE],
      lat: +line[COLUMN_TYPE_ID.LAT],
      long: +line[COLUMN_TYPE_ID.LONG],
      modified_at: line[COLUMN_TYPE_ID.MODIFIED_AT],
      name: line[COLUMN_TYPE_ID.NAME],
      population: +line[COLUMN_TYPE_ID.POPULATION],
      tz: line[COLUMN_TYPE_ID.TZ],
    }
  }
}

export let cityRepository = new CityRepository();
