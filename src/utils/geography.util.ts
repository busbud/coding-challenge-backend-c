import { ICityRawData } from '../interfaces/interfaces';

type CanadaProvinceCodeMap = {
  [key in number]: string;
};

type CountryCodeMap = {
  [key in string]: string;
};

export const CanadaProvincesAlphabetCode: CanadaProvinceCodeMap = {
  1: 'AB',
  2: 'BC',
  3: 'MB',
  4: 'NB',
  5: 'NL',
  7: 'NS',
  8: 'ON',
  9: 'PE',
  10: 'QC',
  11: 'SK',
  12: 'YT',
  13: 'NT',
  14: 'NU',
};

export const CountryCodesMap: CountryCodeMap = {
  US: 'USA',
  CA: 'Canada',
};

export function getCityDetailString(cityRawData: ICityRawData): string {
  const provinceCode = cityRawData.country === 'CA' ? CanadaProvincesAlphabetCode[Number(cityRawData.admin1)] : cityRawData.admin1
  return `${cityRawData.ascii}, ${provinceCode}, ${CountryCodesMap[cityRawData.country]}`
}
