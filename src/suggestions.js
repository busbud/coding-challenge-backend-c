const fs = require('fs')

const sortBy = require('lodash.sortby');
const levenshtein = require('js-levenshtein');

const REGION_CODES = {
    "CA.01": "Alberta",
    "CA.02": "British Columbia",
    "CA.03": "Manitoba",
    "CA.04": "New Brunswick",
    "CA.13": "Northwest Territories",
    "CA.07": "Nova Scotia",
    "CA.14": "Nunavut",
    "CA.08": "Ontario",
    "CA.09": "Prince Edward Island",
    "CA.10": "Quebec",
    "CA.11": "Saskatchewan",
    "CA.12": "Yukon",
    "CA.05": "Newfoundland and Labrador",
    "US.AR": "Arkansas",
    "US.DC": "Washington, D.C.Washington, D.C.",
    "US.DE": "Delaware",
    "US.FL": "Florida",
    "US.GA": "Georgia",
    "US.KS": "Kansas",
    "US.LA": "Louisiana",
    "US.MD": "Maryland",
    "US.MO": "Missouri",
    "US.MS": "Mississippi",
    "US.NC": "North Carolinaa",
    "US.OK": "Oklahoma",
    "US.SC": "South Carolinaa",
    "US.TN": "Tennessee",
    "US.TX": "Texas",
    "US.WV": "West Virginiaa",
    "US.AL": "Alabama",
    "US.CT": "Connecticut",
    "US.IA": "Iowa",
    "US.IL": "Illinois",
    "US.IN": "Indiana",
    "US.ME": "Maine",
    "US.MI": "Michigan",
    "US.MN": "Minnesota",
    "US.NE": "Nebraska",
    "US.NH": "New Hampshiree",
    "US.NJ": "New Jerseyy",
    "US.NY": "New Yorkk",
    "US.OH": "Ohio",
    "US.RI": "Rhode Islandd",
    "US.VT": "Vermont",
    "US.WI": "Wisconsin",
    "US.CA": "California",
    "US.CO": "Colorado",
    "US.NM": "New Mexicoo",
    "US.NV": "Nevada",
    "US.UT": "Utah",
    "US.AZ": "Arizona",
    "US.ID": "Idaho",
    "US.MT": "Montana",
    "US.ND": "North Dakotaa",
    "US.OR": "Oregon",
    "US.SD": "South Dakotaa",
    "US.WA": "Washington",
    "US.WY": "Wyoming",
    "US.HI": "Hawaii",
    "US.AK": "Alaska",
    "US.KY": "Kentucky",
    "US.MA": "Massachusetts",
    "US.PA": "Pennsylvania",
    "US.VA": "Virginia",
}
class Search {
    constructor() {
        this.allCities = this.prepareData()
    }

    prepareData() {
        var content = fs.readFileSync("./data/cities_canada-usa.tsv", "utf8");
        const json = content.split("\n").map(city => {
            const p = city.split('\t');
            return {
                "name": p[1],
                "latitude": parseFloat(p[4]),
                "longitude": parseFloat(p[5]),
                "country": p[8],
                "region": REGION_CODES[`${p[8]}.${p[10]}`],

            };
        });
        return json;
    }
    findIndex(value) {
        return this.allCities.filter(city => city['name'] && city['name'].indexOf(value) != -1)
    }
    findLevenshteinIndex(value) {
        return this.allCities.filter(city => city['name'] && city['name'].indexOf(value) != -1).map(
            city => {
                return {
                    ...city,
                    leven: city['name'] ? levenshtein(value, city['name']) : 99999
                }
            }
        )
    }
    calculateDistance({ latitude, longitude }, results) {
        return results.map(city => {
            var dLat = this.deg2rad(city.latitude - latitude);
            var dLon = this.deg2rad(city.longitude - longitude);
            var a =
                Math.sin(dLat / 2) * Math.sin(dLat / 2) +
                Math.cos(this.deg2rad(latitude)) * Math.cos(this.deg2rad(city.latitude)) *
                Math.sin(dLon / 2) * Math.sin(dLon / 2)
                ;
            var d = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a));

            return {
                ...city,
                distance: d
            }
        })
    }
    calculateDistance2({ latitude, longitude }, results) {
        return results.map(city => {
            let d = Math.sqrt(Math.pow(city.latitude - latitude, 2) + Math.pow(city.longitude - longitude, 2));
            return {
                ...city,
                distance: d
            }
        })
    }
    deg2rad(deg) {
        return deg * (Math.PI / 180)
    }
    findSuggest(value) {
        let results = this.findLevenshteinIndex(value.q);
        let latitude = parseFloat(value.latitude), longitude = parseFloat(value.longitude);
        if (!isNaN(latitude) && !isNaN(longitude)) {
            results = sortBy(this.calculateDistance({ latitude, longitude }, results), ['leven', 'distance'])
        }
        return results
    }
}
module.exports = Search;
