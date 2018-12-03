/**
 * generic distance sort with priority support
 * The basic idea is to compute a number from the given object and a reference point and sort taking in account the priority of each level
 * @param spec Array<<T>{id: string, extractor: (obj) => T, reference: T, distance: (a: T, b:T) => number, type: 'ASC' | 'DSC'}> the object to sort
 * @param predicates Array<any> the object to sort
 */

export const genericSortByDistance = curry((spec, predicates) => {
  return predicates.sort((a, b) => {
    const aValues = spec.map(feat => feat.extractor(a));
    const bValues = spec.map(feat => feat.extractor(b));
  });
});
