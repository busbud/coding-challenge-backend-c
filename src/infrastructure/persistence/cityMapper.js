const toCanadianState = stateNumber => {
  const states = ["AB", "BC", "MB", "NB", "NL", "NS", "NT", "NU", "ON", "PE", "QC", "SK", "YT"];
  return states[stateNumber];
};

module.exports = {
  toObj: fields => {
    return {
      name: fields[1],
      nameAscii: fields[2],
      location: {
        longitude: parseFloat(fields[5]),
        latitude: parseFloat(fields[4])
      },
      state: isNaN(fields[10]) ? fields[10] : toCanadianState(fields[10]),
      country: fields[8],
      population: parseInt(fields[14], 10)
    };
  }
};
