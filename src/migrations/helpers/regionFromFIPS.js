const regions = {
  '01': 'AB',
  '02': 'BC',
  '03': 'MB',
  '04': 'NB',
  '05': 'NL',
  '07': 'NS',
  '08': 'ON',
  '09': 'PE',
  10: 'QC',
  11: 'SK',
  12: 'YT',
  13: 'NT',
  14: 'NU'
}

module.exports = (fips) => {
  const region = regions[fips]
  if (!region) {
    return fips
  }

  return region
}
