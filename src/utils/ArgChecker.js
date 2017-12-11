export default class ArgChecker {
  check(query) {
    if (
      query &&
      (query.q ||
        (query.q && query.latitude && query.longitude) ||
        (query.latitude && query.longitude))
    ) {
      return true;
    }

    return false;
  }
}
