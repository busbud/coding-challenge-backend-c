import request from 'supertest';
import { INestApplication } from '@nestjs/common';
import { defer } from 'rxjs';
import { delay, retryWhen, tap } from 'rxjs/operators';
import { TestScheduler } from 'rxjs/testing';
import { RunHelpers } from 'rxjs/internal/testing/TestScheduler';
import ProvidesCallback = jest.ProvidesCallback;

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
      retryWhen((errors) => errors.pipe(delay(300))),
    )
    .toPromise();
}

export function rxjsTest(fn: (helpers: RunHelpers) => any): ProvidesCallback {
  const testScheduler = new TestScheduler((actual, expected) => {
    console.log(actual, expected);
    expect(actual).toEqual(expected);
  });
  return () => {
    testScheduler.run(fn);
  };
}
