const utf8 = require('utf8');

export default class ArgSanitizer {
  sanitize(query) {
    if (query && query.q) {
      query.q = utf8.decode(query.q);
    }
  }
}
