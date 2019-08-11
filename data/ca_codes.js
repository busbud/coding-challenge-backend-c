// from http://download.geonames.org/export/dump/admin1CodesASCII.txt
const caCodes = {
    '01': 'AB',
    '02': 'BC',
    '03': 'MB',
    '04': 'NB',
    '13': 'NT',
    '07': 'NS',
    '14': 'NU',
    '08': 'ON',
    '09': 'PE',
    '10': 'QC',
    '11': 'SK',
    '12': 'YT',
    '05': 'NL'
}

const convert = (admin1) => {
    if (caCodes[admin1]) {
        return caCodes[admin1]
    } else {
        return admin1
    }
}

module.exports = { convert }