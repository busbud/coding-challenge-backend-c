import { Server } from 'http';
import supertest, { SuperTest, Test } from 'supertest';
import app from 'app';

let server: Server;
let request: SuperTest<Test>;
let response: any;

beforeAll(() => {
  server = app.listen();
  request = supertest(server);
});

afterAll(() => {
  server.close();
});

describe('GET /doc', () => {
  test('Returns a 200', async () => {
    response = await request.get('/doc');
    expect(response.status).toBe(200);
  });
  test('Returns HTML content', async () => {
    expect(response.type).toBe('text/html');
  });
});

describe('GET /doc/swagger.json', () => {
  test('Returns a 200', async () => {
    response = await request.get('/doc/swagger.json');
    expect(response.status).toBe(200);
  });
  test('Returns JSON content', async () => {
    expect(response.type).toBe('application/json');
  });
});
