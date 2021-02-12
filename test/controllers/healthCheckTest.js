const path = require('path');
const request = require('supertest');
const assert = require('chai').assert;
const app = require(path.resolve('src/index'));
const appData = require(path.resolve('package.json'));

describe('Health Check Controller', () => {

	it('should check the current state API', async () => {
		const response = await request(app).get('/api/healthCheck');
		assert.propertyVal(response.body, 'result', 'I am alive!');
		assert.propertyVal(response.body, 'version', appData.version);
		assert.ok(response.statusCode === 200);
	});

	it('should receive 404 to nonexistent route', async () => {
		const response = await request(app).get('/nonexistent');
		assert.ok(response.statusCode === 404);
	});

});