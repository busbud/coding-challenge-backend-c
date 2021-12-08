var assert = require('chai').assert;
var services = require('../src/city.services');

describe('City services', function () {
    it('get all cities',  async () => {
        // check with first load
        const cities = await services.getAllCities();
        assert.equal(5339, cities.length)
        // check the second time
        const res = await services.getAllCities();
        assert.equal(5339, res.length);
    });
});
