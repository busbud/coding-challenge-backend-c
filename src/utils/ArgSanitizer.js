const utf8 = require('utf8');

export default class ArgSanitizer {
  sanitize(query) {
    if (query && query.q) {
      query.q = utf8.decode(query.q);
    }

    if (query && query.latitude) {
      query.latitude = parseFloat(query.latitude);
    }

    if (query && query.longitude) {
      query.longitude = parseFloat(query.longitude);
    }
  }
}
