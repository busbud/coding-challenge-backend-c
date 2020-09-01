import app from '../../src/App';
import { expect } from 'chai';
import supertest, { Response } from 'supertest';

const request = supertest(app);

describe('Redirect in the root path', () => {

    it('returns url to be redirect', (done) => {
      request.get('/')
      .expect(302)
      .end((err: any, res: Response) => {
        const {location} = res.headers;
        expect(location).to.be.equals('https://jonasalessi.github.io/codechallenge-busbud-demo/');
        done();
      });
    });
    
});