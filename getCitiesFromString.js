module.exports = function getCitiesFromString (string) {
  return string
    .split('\n')
    .slice(1) // first line is the headers, so we drop it
    .map(line => line.split('\t')) // tab is the delimiter of our string
    .map(([
      _geo_id,
      name,
      asciiname,
      alternatenames,
      latitude,
      longitude,
      _featureClass,
      _fcode,
      countryCode,
      countryCode2,
      adminCode1,
      _adminCode2,
      _adminCode3,
      _adminCode4,
      _population,
      _elevation,
      _dem,
      _tz,
      _modified_at
    ]) => ({
      name,
      asciiname,
      alternatenames,
      latitude,
      longitude,
      countryCode,
      adminCode1
    }))
    .filter(city => // filter to valid our data
      city.name && city.asciiname
    )
}