"use strict";

function convertAdminCode(row) {
  if (row.countryCode === 'CA') {

    switch(row.admin1Code) {
    case '01':
      return {name: 'Alberta', shortName: 'AB'};
    case '02':
      return {name: 'British Columbia', shortName: 'BC'};
    case '03':
      return {name: 'Manitoba', shortName: 'MB'};
    case '04':
      return {name: 'New Brunswick', shortName: 'NB'};
    case '05':
      return {name: 'Newfoundland and Labrador', shortName: 'NL'};
    case '06':
      return undefined;
    case '07':
      return {name: 'Nova Scotia', shortName: 'NS'};
    case '08':
      return {name: 'Ontario', shortName: 'ON'};
    case '09':
      return {name: 'Prince Edward Island', shortName: 'PE'};
    case '10':
      return {name: 'Quebec', shortName: 'QC'};
    case '11':
      return {name: 'Saskatchewan', shortName: 'SK'};
    case '12':
      return {name: 'Yukon', shortName: 'YT'};
    case '13':
      return {name: 'Northwest Territories', shortName: 'NT'};
    case '14':
      return {name: 'Nunavut', shortName: 'NU'};
    }
  } else {

    // US
    switch(row.admin1Code) {
    case 'AR':
      return {name: 'Arkansas', shortName: row.admin1Code};
    case 'DC':
      return {name: 'Washington, D.C', shortName: row.admin1Code};
    case 'DE':
      return {name: 'Delaware', shortName: row.admin1Code};
    case 'FL':
      return {name: 'Florida', shortName: row.admin1Code};
    case 'GA':
      return {name: 'Georgia', shortName: row.admin1Code};
    case 'KS':
      return {name: 'Kansas', shortName: row.admin1Code};
    case 'LA':
      return {name: 'Louisiana', shortName: row.admin1Code};
    case 'MD':
      return {name: 'Maryland', shortName: row.admin1Code};
    case 'MO':
      return {name: 'Missouri', shortName: row.admin1Code};
    case 'MS':
      return {name: 'Mississippi', shortName: row.admin1Code};
    case 'NC':
      return {name: 'North Carolina', shortName: row.admin1Code};
    case 'OK':
      return {name: 'Oklahoma', shortName: row.admin1Code};
    case 'SC':
      return {name: 'South Carolina', shortName: row.admin1Code};
    case 'TN':
      return {name: 'Tennessee', shortName: row.admin1Code};
    case 'TX':
      return {name: 'Texas', shortName: row.admin1Code};
    case 'WV':
      return {name: 'West Virginia', shortName: row.admin1Code};
    case 'AL':
      return {name: 'Alabama', shortName: row.admin1Code};
    case 'CT':
      return {name: 'Connecticut', shortName: row.admin1Code};
    case 'IA':
      return {name: 'Iowa', shortName: row.admin1Code};
    case 'IL':
      return {name: 'Illinois', shortName: row.admin1Code};
    case 'IN':
      return {name: 'Indiana', shortName: row.admin1Code};
    case 'ME':
      return {name: 'Maine', shortName: row.admin1Code};
    case 'MI':
      return {name: 'Michigan', shortName: row.admin1Code};
    case 'MN':
      return {name: 'Minnesota', shortName: row.admin1Code};
    case 'NE':
      return {name: 'Nebraska', shortName: row.admin1Code};
    case 'NH':
      return {name: 'New Hampshire', shortName: row.admin1Code};
    case 'NJ':
      return {name: 'New Jersey', shortName: row.admin1Code};
    case 'NY':
      return {name: 'New York', shortName: row.admin1Code};
    case 'OH':
      return {name: 'Ohio', shortName: row.admin1Code};
    case 'RI':
      return {name: 'Rhode Island', shortName: row.admin1Code};
    case 'VT':
      return {name: 'Vermont', shortName: row.admin1Code};
    case 'WI':
      return {name: 'Wisconsin', shortName: row.admin1Code};
    case 'CA':
      return {name: 'California', shortName: row.admin1Code};
    case 'CO':
      return {name: 'Colorado', shortName: row.admin1Code};
    case 'NM':
      return {name: 'New Mexico', shortName: row.admin1Code};
    case 'UT':
      return {name: 'Utah', shortName: row.admin1Code};
    case 'AZ':
      return {name: 'Arizona', shortName: row.admin1Code};
    case 'ID':
      return {name: 'Idaho', shortName: row.admin1Code};
    case 'MT':
      return {name: 'Montana', shortName: row.admin1Code};
    case 'ND':
      return {name: 'North Dakota', shortName: row.admin1Code};
    case 'OR':
      return {name: 'Oregon', shortName: row.admin1Code};
    case 'South Dakota':
      return {name: 'South Dakota', shortName: row.admin1Code};
    case 'WA':
      return {name: 'Washington', shortName: row.admin1Code};
    case 'WY':
      return {name: 'Wyoming', shortName: row.admin1Code};
    case 'HI':
      return {name: 'Hawaii', shortName: row.admin1Code};
    case 'AK':
      return {name: 'Alaska', shortName: row.admin1Code};
    case 'KY':
      return {name: 'Kentucky', shortName: row.admin1Code};
    case 'MA':
      return {name: 'Massachusetts', shortName: row.admin1Code};
    case 'PA':
      return {name: 'Pennsylvania', shortName: row.admin1Code};
    case 'VA':
      return {name: 'Virginia', shortName: row.admin1Code};
    case 'SD':
      return {name: 'South Dakota', shortName: row.admin1Code};
    case 'NV':
      return {name: 'Nevada', shortName: row.admin1Code};
    }
  }
}

function stringify(obj) {
  return JSON.stringify(obj, (key, val) => {
    switch (key) {
    case 'parent':
    case 'sw':
    case 'se':
    case 'ne':
    case 'nw':
    // case 'geonameid':
    // case 'modificationDate':
      return val.id;
    default:
      return val;
    }
  });
}

function clone(obj) {
  return JSON.parse(stringify(obj));
}

module.exports = {
  convertAdminCode,
  stringify,
  clone
};
