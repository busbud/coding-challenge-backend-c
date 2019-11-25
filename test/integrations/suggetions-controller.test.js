/* eslint-disable import/order */
/* eslint-disable prefer-destructuring */
/* eslint-env mocha */

// Libraries
const chai = require('chai');

const chaiHttp = require('chai-http');

const app = require('../../app');

const assert = chai.assert;
chai.use(chaiHttp);


describe('SuggestionsController', () => {
  describe('GET /suggestions', () => {
    it('should return 400 if q param is not provided', (done) => {
      chai.request(app)
        .get('/suggestions')
        .end((err, res) => {
          const { code, status, detail } = res.body.error;
          assert.equal(code, 'C-1010');
          assert.equal(status, '400');
          assert.equal(detail, 'Mandatory request parameter is missing: \'q\'');
        });

      done();
    });
  });
});
