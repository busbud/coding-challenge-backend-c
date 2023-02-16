const fipsCodesCanada: { [key: string]: string } = {
  "01": "AB",
  "02": "BC",
  "03": "MB",
  "04": "NB",
  "05": "NL",
  "07": "NS",
  "08": "ON",
  "09": "PE",
  "10": "QC",
  "11": "SK",
  "12": "NT",
  "13": "NU",
  "14": "YT"
};

export const checkForFIPSCodes = (stateAdminCode: string, country: string) => {
  if (country === 'Canada' && stateAdminCode) {
    if (fipsCodesCanada[stateAdminCode]) {
      console.log(fipsCodesCanada[stateAdminCode])
      return fipsCodesCanada[stateAdminCode];
    }
  }
  return stateAdminCode;
};
