import db from 'service/database';

export interface City {
  id?: number,
  ascii?: string,
  name?: string,
  country?: string,
  admin1?: string,
  lat?: number,
  long?: number,
  population?: number,
}

export default {
  async searchCities(q: string) {
    const cities = await db('city').as('city')
      .innerJoin('country', 'country.iso', 'city.country')
      .innerJoin('state', (join) => join.on('state.country', 'city.country').andOn('state.admin1', 'city.admin1'))
      .select('city.name', 'city.lat', 'city.long', 'city.population', 'country.country', 'state.name as state')
      .whereRaw('LOWER(ascii) LIKE ?', [`${q}%`]);
    return cities;
  },
};
