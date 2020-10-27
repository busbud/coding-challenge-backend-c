/**
 * Create a city mock.
 *
 * @return {Object} A dummy city object.
 */
const createCityMock = () => ({
  id: '6058560',
  name: 'London',
  ascii: 'London',
  alt_name:
    'London,Londonas,Londono,YXU,leondeon,lndn,lndn, antaryw,londoni,lun dui,lun dun,lwndwn,rondon,–õ–æ–Ω–¥–æ–Ω,◊ú◊ï◊†◊ì◊ï◊ü,ŸÑŸÜÿØŸÜ,ŸÑŸÜÿØŸÜÿå ÿßŸÜÿ™ÿßÿ±€åŸà,ŸÑŸÜÿØŸÜÿå ÿßŸàŸÜŸπÿßÿ±€åŸà,·Éö·Éù·Éú·Éì·Éù·Éú·Éò,„É≠„É≥„Éâ„É≥,‰º¶Êï¶,Îü∞Îçò',
  lat: '42.98339',
  long: '-81.23304',
  feat_class: 'P',
  feat_code: 'PPL',
  country: 'CA',
  cc2: '',
  admin1: '8',
  admin2: '',
  admin3: '',
  admin4: '',
  population: 346765,
  elevation: '',
  dem: 252,
  tz: 'America/Toronto',
  modified_at: '19/08/2012',
});

/**
 * Export the mocks.
 */
module.exports = {
  createCityMock,
};
