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
  test('Response status is 200', async () => {
    response = await request.get('/doc');
    expect(response.status).toBe(200);
  });
  test('Response type is text/html', async () => {
    expect(response.type).toBe('text/html');
  });
});

describe('GET /doc/swagger.json', () => {
  test('Response status is 200', async () => {
    response = await request.get('/doc/swagger.json');
    expect(response.status).toBe(200);
  });
  test('Response type is application/json', async () => {
    expect(response.type).toBe('application/json');
  });
});
