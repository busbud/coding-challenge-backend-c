import hash from 'object-hash';
import { Location } from 'service/suggestion';

export default {
  suggestCities: {
    key: (q: string, location?: Location) => `suggestCities:${hash({ q, location })}`,
    time: 300,
  },
};
