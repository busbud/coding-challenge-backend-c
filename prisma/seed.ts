import fs from 'fs';
import { City, PrismaClient } from '@prisma/client';
import { adminCodeToStateCode } from '../src/helpers/cities';

const headers = [
  'id',
  'name',
  'ascii',
  'alt_name',
  'lat',
  'long',
  'feat_class',
  'feat_code',
  'country',
  'cc2',
  'admin1',
  'admin2',
  'admin3',
  'admin4',
  'population',
  'elevation',
  'dem',
  'tz',
  'modified_at',
] as const;

export interface RawCity extends Record<(typeof headers)[number], string> {}

const prisma = new PrismaClient();
async function seed() {
  await prisma.city.deleteMany({});

  const lines = fs
    .readFileSync('data/cities_canada-usa.tsv')
    .toString()
    .split('\n');

  const skipLines = 1;

  const rawCities = lines.reduce((acc, rawLine, index) => {
    if (index + 1 <= skipLines) {
      return acc;
    }

    const line = rawLine.trim();
    if (!line) {
      return acc;
    }

    const cells = line.split('\t');

    const row = cells.reduce((obj, cell, index) => {
      const header = headers[index];
      obj[header] = cell;
      return obj;
    }, {} as RawCity);

    acc.push(row as RawCity);
    return acc;
  }, [] as RawCity[]);

  const cities: City[] = rawCities.map((raw) => {
    const {
      id: rawId,
      name,
      ascii: asciiName,
      alt_name: rawAlternateNames,
      lat: rawLatitude,
      long: rawLongitude,
      country: countryCode,
      admin1: adminCode,
      population: rawPopulation,
    } = raw;

    const id = parseInt(rawId);
    const latitude = parseFloat(rawLatitude);
    const longitude = parseFloat(rawLongitude);
    const alternateNames = rawAlternateNames?.split(',');
    const stateCode = adminCodeToStateCode(countryCode, adminCode);
    const population = parseInt(rawPopulation);

    return {
      id,
      name,
      asciiName,
      alternateNames,
      latitude,
      longitude,
      countryCode,
      stateCode,
      population,
    };
  });

  await prisma.city.createMany({
    data: cities,
  });

  console.log(`${cities.length} Cities imported successfully`);

  return;
}

seed()
  .then(async () => {
    await prisma.$disconnect();
  })
  .catch(async (e) => {
    console.error(e);
    await prisma.$disconnect();
    process.exit(1);
  });
