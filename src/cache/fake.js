/* eslint-disable class-methods-use-this */


/**
 * Very simplistic Fake cache backend to be used when running the server locally
 * where a cache backend like Redis might not be installed.
 */
export default class FakeCache {

  get(...args) {
    const callback = args.slice(-1).pop();
    callback(null, null);
  }

  set() {}
}
