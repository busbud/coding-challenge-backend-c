import request from 'supertest';
import { app } from '../app';

declare global {
  namespace NodeJS {
    interface Global {
      locations(): Promise<string[]>;
    }
  }
}

global.locations = async () => {
  const q = 'London';
  const lat = 42.98339;

  const response = await request(app)
    .get('/suggestion')
    .send({
      q,
      lat,
    })
    .expect(400);

  const cookie = response.get('Set-Cookie');

  return cookie;
};
