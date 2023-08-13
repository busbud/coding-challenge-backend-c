const CANADIAN_STATES: Record<string, string> = {
  '01': 'AB',
  '02': 'BC',
  '03': 'MB',
  '04': 'NB',
  '05': 'NL',
  '07': 'NS',
  '08': 'ON',
  '09': 'PE',
  '10': 'QC',
  '11': 'SK',
  '12': 'YT',
  '13': 'NT',
  '14': 'NU',
};

export function adminCodeToStateCode(
  countryCode: string,
  adminCode: string
): string {
  if (countryCode === 'CA') {
    return CANADIAN_STATES[adminCode];
  }

  return adminCode;
}
