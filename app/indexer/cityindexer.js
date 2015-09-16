class CityIndexer {
  constructor(client) {
    this.client = client;
    this.data = [];
    this.promises = [];
  }

  index(id, data) {
    this.data.push({index: {
      _index: 'cities',
      _type: 'city',
      _id: id
    }});
    this.data.push(data);

    if (this.data.length >= 8000) {
      this.flush();
    }
  }

  flush() {
    const promise = new Promise((resolve, reject) => {
      this.client.bulk({body: this.data}).then(() => {resolve();}, (error) => {
        reject(error);
      });
      this.data = [];
    });

    this.promises.push(promise);

    return promise;
  }

  flushAll() {
    this.flush();

    return Promise.all(this.promises);
  }
};

export default CityIndexer;
