/**
 * should be in database instead or with i18n management
 */
export default class GeoCoderHelper {
  constructor() {
    this.map = new Map();
    this.map.set('01', 'AB');
    this.map.set('02', 'BC');
    this.map.set('03', 'MB');
    this.map.set('04', 'NB');
    this.map.set('05', 'NL');
    this.map.set('06', '??');
    this.map.set('07', 'NS');
    this.map.set('08', 'ON');
    this.map.set('09', 'PE');
    this.map.set('10', 'QC');
    this.map.set('11', 'SK');
    this.map.set('12', 'YT');
    this.map.set('13', 'NT');
    this.map.set('14', 'NU');
  }

  getRegion = (country, code) => {
    switch (country) {
      case 'CA':
        return this.map.get(code);
      default:
        return code;
    }
  };

  getCountry = country => {
    switch (country) {
      case 'CA':
        return 'Canada';
      case 'US':
        return 'USA';
      default:
        return country;
    }
  };
}
