import { Prisma, PrismaClient } from '@prisma/client';
import { City, CitySuggestion } from '../../../domain/models';

type Params = {
  name: string;
  minimumPopulation: number;
  countryCodes: string[];
  limit: number;
};

type QueryResult = Pick<
  City,
  'name' | 'countryCode' | 'stateCode' | 'latitude' | 'longitude'
> & {
  score: number;
};

export async function findManyCitiesSuggestions(
  prisma: PrismaClient,
  params: Params
): Promise<CitySuggestion[]> {
  const { name, minimumPopulation, countryCodes, limit } = params;

  const rawCities = await prisma.$queryRaw<QueryResult[]>`
    SELECT c."name", c."countryCode", c."stateCode", c."latitude", c."longitude"
      , similarity("name", ${name}) AS "score"
    FROM "City" c
    WHERE "name" % ${name} AND "population" >= ${minimumPopulation} 
    AND "countryCode" in (${Prisma.join(countryCodes)})
    ORDER by "score" desc
    LIMIT ${limit};`;

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
