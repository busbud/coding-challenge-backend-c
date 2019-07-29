const db = require('../');

// This is a mock of the es.Client instance
function getClient(createCode = 200, existsCode = 200) {
  return {
    params: {},
    search: function (...args) {
      this.searchCalled = true;
      this.params.search = args;
      return {statusCode: 200}
    },
    bulk: function (...args) {
      this.bulkCalled = true;
      this.params.bulk = args;
      return {statusCode: 200}
    },
    ping: function () {},
    indices: {
      existsCalled: false,
      createCalled: false,
      mappingCalled: false,
      params: {
        create: [],
        exists: []
      },
      create: function (...args){
        this.params.create = args;
        this.createCalled = true;
        return {statusCode: createCode};
      },
      exists: function async (...args){
        this.params.exists = args
        this.existsCalled = true;
        return {statusCode: existsCode};
      },
      putMapping: function (...args) {
        this.params.mapping = args;
        this.mappingCalled = true;
        return {statusCode: 200}
      }
    }
  };
}

describe('db module', () => {
  describe('initIndex()', () => {
    test('should create index', async () => {
      let client = getClient(200, 404);

      const index = 'foo';

      try {
        await db.initIndex(index, client);
      }
      catch(err) {
        console.error(err);
      }

      expect(client.indices.existsCalled).toBeTruthy();
      expect(client.indices.createCalled).toBeTruthy();
      expect(client.indices.params.exists[0]).toEqual({index});
      expect(client.indices.params.create[0]).toEqual({index});
    });

    test('index should exist', async () => {
      let client = getClient(404, 200);

      const index = 'foo';

      try {
        await db.initIndex(index, client);
      }
      catch(err) {
        console.error(err);
      }

      expect(client.indices.existsCalled).toBeTruthy();
      expect(client.indices.createCalled).toBeFalsy();
      expect(client.indices.params.exists[0]).toEqual({index});
    });
  });

  describe('putMapping()', () => {
    test('should create mapping', async () => {
      const client = getClient();
      const mapping = {a: 'a'};
      const index = 'foo';
      const type = 'footype';

      try {
        await db.putMapping(mapping, index, type, client);
      }
      catch(err) {
        console.error(err);
      }

      expect(client.indices.mappingCalled).toBeTruthy();
      expect(client.indices.params.mapping[0].type).toEqual('footype');
      expect(client.indices.params.mapping[0].index).toEqual('foo');
      expect(client.indices.params.mapping[0].body).toEqual({ properties: { a: 'a' } });
    });
  });

  describe('bulk()', () => {
    test('should call bulk method', async () => {
      const body = {a: '1'};
      const index = 'foo';
      const client = getClient();

      await db.bulkUpload(body, index, client);

      expect(client.bulkCalled).toBeTruthy();
      expect(client.params.bulk[0]).toEqual({ index: 'foo', body: { a: '1' } });
    });
  });

  describe('search()', () => {
    test('should call search method', async () => {
      const client = getClient();
      const index = 'foo';
      const query = {a: '1'};


      await db.search(client, index, query);

      expect(client.searchCalled).toBeTruthy();
      expect(client.params.search[0]).toEqual({ index: 'foo', body: { a: '1' } });
    });
  });
});