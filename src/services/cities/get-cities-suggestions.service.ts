import { PrismaClient } from '@prisma/client';
import env from '../../config/env';
import { CitySuggestion } from '../../domain/models';
import { findManyCitiesSuggestions } from '../../infra/db/cities/find-many-cities-suggestions';

const LIMIT = 10;

export async function getCitiesSuggestions(
  prisma: PrismaClient,
  name: string
): Promise<CitySuggestion[]> {
  const cities = await findManyCitiesSuggestions(prisma, {
    name,
    minimumPopulation: env.cities.largeCitiesMinimumPopulation,
    countryCodes: env.cities.acceptedCountryCodes,
    limit: LIMIT,
  });

  return cities;
}
