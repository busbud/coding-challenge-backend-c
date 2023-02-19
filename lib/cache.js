/**
 * Meant to mimick a redis cache  - this is where you would save the tries
 */


module.exports = {
  memory: {},

  init(data) {
    this.memory = data;
  },
  get(val) {
    return this.memory[val] || false;
  },
  set (key, val) {
    this.memory[key] = val;
  }
}