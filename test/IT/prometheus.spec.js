const request = require('supertest');

const App = require('../../app');

const { actuatorListener } = new App();

describe('GET /actuator/prometheus', () => {
    let response;

    beforeAll(async () => {
        response = await request(actuatorListener)
            .get('/actuator/prometheus')
            .send();
    });

    it('returns a 200', () => {
        expect(response.statusCode).toEqual(200);
    });

    it('returns prometheus metrics in body', () => {
        expect(response.body).toMatchSnapshot();
    });
});
