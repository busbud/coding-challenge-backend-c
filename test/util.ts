import request from 'supertest';
import { INestApplication } from '@nestjs/common';
import { defer } from 'rxjs';
import { delay, retryWhen, tap } from 'rxjs/operators';

export function waitUntilHealthy(
  app: INestApplication,
): Promise<request.Response> {
  return defer(() => request(app.getHttpServer()).get('/health'))
    .pipe(
      tap((res) => {
        if (res.body?.status !== 'ok') {
          throw res.body.error;
        }
      }),
      retryWhen((errors) => errors.pipe(delay(100))),
    )
    .toPromise();
}
