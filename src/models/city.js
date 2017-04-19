/* eslint-disable no-underscore-dangle */
const Document = require('camo').Document;
const isoCountries = require('i18n-iso-countries');
const latinize = require('latinize');
const path = require('path');
const databasePath = require('../database/reader').databasePath;
const canadaProvinceCodeMap = require('../utils/canada_province_codes').default;


const defaultFilters = { population: { $gt: 5000 }, country: { $in: ['CA', 'US'] } };

/*
 geonameid         : integer id of record in geonames database
 name              : name of geographical point (utf8) varchar(200)
 asciiname         : name of geographical point in plain ascii characters, varchar(200)
 alternatenames    : alternatenames, comma separated varchar(5000)
 latitude          : latitude in decimal degrees (wgs84)
 longitude         : longitude in decimal degrees (wgs84)
 feature class     : see http://www.geonames.org/export/codes.html, char(1)
 feature code      : see http://www.geonames.org/export/codes.html, varchar(10)
 country code      : ISO-3166 2-letter country code, 2 characters
 cc2               : alternate country codes, comma separated, ISO-3166 2-letter country code,
 60 characters
 admin1 code       : fipscode (subject to change to iso code), see exceptions below,
 see file admin1Codes.txt for display
 names of this code; varchar(20)
 admin2 code       : code for the second administrative division, a county in the US,
 see file admin2Codes.txt; varchar(80)
 admin3 code       : code for third level administrative division, varchar(20)
 admin4 code       : code for fourth level administrative division, varchar(20)
 population        : bigint (8 byte int)
 elevation         : in meters, integer
 dem               : digital elevation model, srtm3 or gtopo30,
 average elevation of 3''x3'' (ca 90mx90m) or 30''x30''
 (ca 900mx900m) area in meters, integer. srtm processed by cgiar/ciat.
 timezone          : the timezone id (see file timeZone.txt) varchar(40)
 modification date : date of last modification in yyyy-MM-dd format
 */

/**
 * Object representing a City instance inside the application.
 *
 * Describes the fields written to the 'cities' collection of the database.
 */
export default class City extends Document {
  constructor() {
    super();

    // NeDB Fields managed by 'camo'
    this.geoNameId = {
      type: Number,
      unique: true,
    };
    this.name = String;
    this.asciiName = String;
    this.alternateNames = [String];

    // In degrees
    this.latitude = {
      type: Number,
      min: -90,
      max: 90,
    };
    this.longitude = {
      type: Number,
      min: -180,
      max: 180,
    };

    this.featureClass = String;
    this.featureCode = String;
    this.country = String;
    this.alternateCountryCodes = [String];
    this.admin1 = String;
    this.admin2 = String;
    this.admin3 = String;
    this.admin4 = String;
    this.population = {
      type: Number,
      min: 0,
    };
    this.elevation = String;
    this.dem = Number;
    this.tz = String;
    this.modifiedAt = {
      type: Date,
    };

    // Additional fields populated at transform time by 'create_city_db_from_tsv' script
    this.searchableName = String;

    // Extra properties that will not be saved to DB by 'camo' due to the underscore in the name
    this._geoScoreWeight = 0.3;
    this._nameScoreWeight = 0.7;

    this._geoScore = null;
    this._nameScore = null;
  }

  get geoScore() {
    return this._geoScore;
  }

  set geoScore(newGeoScore) {
    this._geoScore = newGeoScore;
  }

  get nameScore() {
    return this._nameScore;
  }

  set nameScore(newNameScore) {
    this._nameScore = newNameScore;
  }

  get totalScore() {
    let totalScore = null;

    if (this.geoScore === null) {
      totalScore = this.nameScore;
    } else if (this.nameScore === null) {
      totalScore = this.geoScore;
    } else {
      const weightedGeoScore = this._geoScoreWeight * this.geoScore;
      const weightedNameScore = this._nameScoreWeight * this.nameScore;
      totalScore = (weightedGeoScore + weightedNameScore);
    }

    return Math.round(totalScore * 100) / 100;  // faster than using 'toFixed()'
  }

  get stateLabel() {
    let label = '';
    if (this.country === 'CA') {
      label = canadaProvinceCodeMap[this.admin1] || '';
    } else if (this.country === 'US') {
      label = this.admin1;
    }
    return label;
  }

  get countryLabel() {
    const label = isoCountries.getName(this.country, 'en');
    return label === undefined ? '' : label;
  }

  get displayLabel() {
    return [this.asciiName, this.stateLabel, this.countryLabel].filter(val => val).join(', ');
  }

  toApiResponseObject() {
    return {
      name: this.displayLabel,
      latitude: this.latitude.toString(),
      longitude: this.longitude.toString(),
      score: this.totalScore,
    };
  }

  static collectionName() {
    return 'cities';
  }

  static filePath() {
    return path.join(databasePath, `${this.collectionName()}.db`);
  }

  static findByNameStartingWith(nameQuery, applyDefaultFilters = true) {
    let filters = {};
    // Find city name LIKE nameQuery% (case-insensitive and diatric-insensitive)
    const nameToSearchWith = latinize(nameQuery).toUpperCase();
    filters.searchableName = { $regex: new RegExp(`^${nameToSearchWith}`) };
    if (applyDefaultFilters === true) {
      filters = Object.assign(filters, defaultFilters);  // Only cities in CA or US with pop > 5000
    }
    return this.find(filters);
  }

}
