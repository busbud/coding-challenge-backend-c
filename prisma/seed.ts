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

function readFileLines(file: string, separator: string): string[] {
  const lines = fs.readFileSync(file).toString().split(separator);

  return lines;
}

function readRows<T>(
  lines: string[],
  separator: string,
  skipLines: number,
  buildRow: (cells: string[]) => T
): T[] {
  const rows = lines.reduce((acc, line, index) => {
    if (index + 1 <= skipLines) {
      return acc;
    }

    const trimmedLine = line.trim();
    if (!trimmedLine) {
      return acc;
    }

    const cells = trimmedLine.split(separator);
    const row = buildRow(cells);

    acc.push(row);

    return acc;
  }, [] as T[]);

  return rows;
}

function buildRawCity(cells: string[]): RawCity {
  const rawCity = cells.reduce((obj, cell, index) => {
    const header = headers[index];
    obj[header] = cell;
    return obj;
  }, {} as RawCity);

  return rawCity;
}

function rawCityToCity(rawCity: RawCity): City {
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
  } = rawCity;

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
}

const prisma = new PrismaClient();

async function seed() {
  await prisma.city.deleteMany({});

  const lines = readFileLines('data/cities_canada-usa.tsv', '\n');

  const rawCities = readRows(lines, '\t', 1, buildRawCity);

  const cities: City[] = rawCities.map(rawCityToCity);

  await prisma.city.createMany({
    data: cities,
  });

  console.log(`${cities.length} Cities imported successfully`);
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
