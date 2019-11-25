/* eslint-disable no-undef */
/* eslint-disable prefer-destructuring */
process.env.NODE_ENV = 'test';

const httpStatus = require('http-status-codes');
const chai = require('chai');
const chaiHttp = require('chai-http');

const app = require('../../app');

const assert = chai.assert;

chai.use(chaiHttp);

const path = '/';

describe('/get /', () => {
  it('health check endpoint should return 200', (done) => {
    // given an endpoint for health check purposes
    // when we call the health check endpoint
    chai.request(app)
      .get(path)
      .end((err, res) => {
      // then we expect a 200 http response and the response body data message 'Health check ok!'
        assert.equal(res.status, httpStatus.OK);
        assert.equal('Health check ok!', res.body.data);
      });

    done();
  });
});
