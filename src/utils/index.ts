import DistanceHelper from './distance.util';

export function notValidNumber(val: string): boolean {
  return val && isNaN(Number.parseFloat(val));
}

export const helpers = {
  DistanceHelper,
};
