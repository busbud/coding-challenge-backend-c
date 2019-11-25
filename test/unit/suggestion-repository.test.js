/* eslint-disable prefer-destructuring */
/* eslint-env mocha */

// Libraries
const Sinon = require('sinon');
const Chai = require('chai');
const ChaiAsPromised = require('chai-as-promised');

const SuggestionsRepository = require('../../dao/repositories/SuggestionsRepository');

Chai.use(ChaiAsPromised);
const assert = Chai.assert;
const sandbox = Sinon.createSandbox();

describe('SuggestionsRepository', () => {
  afterEach(async () => {
    sandbox.restore();
  });

  describe('getSuggestions', () => {
    const search = { q: 'londo' };
    it('should match sql query', async () => {
      const result = await SuggestionsRepository.getSuggestions(search, { returnSQL: true });
      assert.equal(result, `select json_agg(row_to_json(suggestions)) as suggestions from (select concat_ws(', ', ascii, admin1, country) as name, latitude, longitude from busbud m where m.name ~* '${search.q}' or m.alt_name ~* '${search.q}' and m.population > 5000) suggestions`);
    });
  });
});
