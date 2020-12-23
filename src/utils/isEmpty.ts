import { ILocation } from '../modals/location';

export default function isEmpty(response: ILocation) {
  for (const prop in response) return false;
  return true;
}
