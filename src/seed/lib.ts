import type { Attributes, CSVLocation } from './types';

const attributes: Attributes[] = [
  'id',
  'name',
  'lat',
  'long',
  'country',
  'admin1',
  'population',
];

// Initially, the column indexes are hardcoded for clarity.
// As the seed progresses, updateIndexedLocations dynamically updates these indexes.
const locationIndexes = {
  id: 0,
  name: 1,
  lat: 4,
  long: 5,
  country: 8,
  admin1: 10,
  population: 14,
};

// I compiled this mapping manually through Google searches.
// TODO: Is there a mapping that exists online instead?
const provinces: Record<string, string> = {
  '01': 'AB', // Alberta
  '02': 'BC', // British Columbia
  '03': 'MB', // Manitoba
  '04': 'NB', // New Brunswick
  '05': 'NL', // Newfoundland and Labrador
  '07': 'NS', // Nova Scotia
  '08': 'ON', // Ontario
  '09': 'PE', // Prince Edward Island
  '10': 'QC', // Quebec
  '11': 'SK', // Saskatchewan
  '12': 'YT', // Yukon
  '13': 'NT', // Northwest Territories
  '14': 'NU', // Nunavut
};

const createLocation = (col: string[]): CSVLocation => {
  const location = {
    id: col[locationIndexes.id],
    name: col[locationIndexes.name],
    lat: parseFloat(col[locationIndexes.lat]),
    long: parseFloat(col[locationIndexes.long]),
    country: col[locationIndexes.country],
    state: col[locationIndexes.admin1],
    population: parseInt(col[locationIndexes.population]),
  };

  if (isNaN(location.lat) || isNaN(location.long) || isNaN(location.population))
    throw new Error('Invalid data format for lat, long, or population');

  if (location.state in provinces) location.state = provinces[location.state];

  return location;
};

const updateIndexedLocations = (col: string[]) => {
  for (const attr of attributes) {
    const index = col.indexOf(attr);

    if (index === -1)
      throw new Error(`CSV is missing expected column: ${attr}`);

    locationIndexes[attr] = index;
  }
};

export { createLocation, updateIndexedLocations };
