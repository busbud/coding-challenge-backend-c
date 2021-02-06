const request = require('supertest');

const App = require('../../app');

const { actuatorListener } = new App();

describe('GET /actuator/health', () => {
    let response;

    beforeAll(async () => {
        response = await request(actuatorListener)
            .get('/actuator/health')
            .send();
    });

    it('returns a 200', () => {
        expect(response.statusCode).toEqual(200);
    });

    it('returns UP message in body', () => {
        expect(response.body).toMatchSnapshot();
    });
});
