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

describe('GET /', () => {
  test('Response status is 200', async () => {
    response = await request.get('/');
    expect(response.status).toBe(200);
  });
  test('Response type is text/plain', async () => {
    expect(response.type).toBe('text/plain');
  });
  test('Response text is OK', async () => {
    expect(response.text).toBe('OK');
  });
});
