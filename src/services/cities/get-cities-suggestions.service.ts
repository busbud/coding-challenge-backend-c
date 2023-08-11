/* eslint-disable @typescript-eslint/no-unused-vars */
import { City, PrismaClient } from '@prisma/client';

type CitySuggestion = Pick<City, 'id'>;

export async function getCitiesSuggestions(
  prisma: PrismaClient
): Promise<CitySuggestion[]> {
  return [] as CitySuggestion[];
}
