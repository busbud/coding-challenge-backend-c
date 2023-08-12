/* eslint-disable @typescript-eslint/no-unused-vars */
import { Prisma, PrismaClient } from '@prisma/client';
import { City, CitySuggestion } from '../../domain/models';

type QueryResult = Pick<
  City,
  'name' | 'countryCode' | 'stateCode' | 'latitude' | 'longitude'
> & {
  score: number;
};

const LARGE_CITIES_POPULATION = 5000;
const ACCEPTED_COUNTRY_CODES = ['CA', 'US'];
const LIMIT = 10;

export async function getCitiesSuggestions(
  prisma: PrismaClient,
  name: string
): Promise<CitySuggestion[]> {
  const rawCities = await prisma.$queryRaw<QueryResult[]>`
    SELECT c."name", c."countryCode", c."stateCode", c."latitude", c."longitude"
      , similarity("name", ${name}) AS "score"
    FROM "City" c
    WHERE "name" % ${name} AND "population" > ${LARGE_CITIES_POPULATION} 
    AND "countryCode" in (${Prisma.join(ACCEPTED_COUNTRY_CODES)})
    ORDER by "score" desc
    LIMIT ${LIMIT};`;

  const cities = rawCities.map((city) => {
    const { name, stateCode, countryCode, latitude, longitude, score } = city;

    return {
      name: `${name}, ${stateCode}, ${countryCode}`,
      latitude,
      longitude,
      score,
    };
  });

  return cities;
}
