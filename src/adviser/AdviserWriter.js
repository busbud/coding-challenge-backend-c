const assert = require('assert');
const { Writable } = require('stream');

class AdviserWriter extends Writable {
  constructor({ config, score, userEntry }) {
    assert(config, 'expected config');
    assert(score, 'expected score');
    assert(userEntry, 'expected userEntry');
    super({ objectMode: true });

    this.config = config.suggestion;
    this.score = score;
    this.userEntry = {
      name: userEntry.q,
      latitude: userEntry.latitude,
      longitude: userEntry.longitude,
    };
    this.topResult = [];
  }

  _write(data, encoding, callback) {
    try {
      const score = this.score.getScore(this.userEntry, data);
      if (score < this.config.thresholdMatch) {
        callback();
        return;
      }
      const result = {
        name: data.name,
        latitude: data.lat,
        longitude: data.long,
        score: score,
      };
      console.log(score)

      if (this.topResult.length >= this.config.maxNumber) {
        this._putResult(result);
      } else {
        this._addResult(result);
      }
      callback();
    } catch (err) {
      callback(err);
    }
  }

  _addResult(result) {
    if (this.topResult.length === 0) {
      this.topResult.push(result);
      return;
    }
    for (let i = 0; i < this.topResult.length - 1; i++) {
      if (this.topResult[i].score < result.score) {
        this.topResult.splice(i - 1, 0, result);
        return;
      }
    }
    this.topResult.push(result);
  }

  _putResult(result) {
    if (this.topResult[this.topResult.length - 1].score < result.score) {
      for (let i = 0; i < this.topResult.length; i++) {
        if (this.topResult[i].score < result.score) {
          this.topResult[i] = result;
          break;
        }
      }
    }
  }

  getResult() {
    return this.topResult;
  }
}

module.exports = {
  AdviserWriter,
};