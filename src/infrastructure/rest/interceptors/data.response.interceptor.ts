import {
  Injectable,
  NestInterceptor,
  ExecutionContext,
  CallHandler,
} from '@nestjs/common';
import { Observable } from 'rxjs';
import { map } from 'rxjs/operators';
import { APP_INTERCEPTOR } from '@nestjs/core';
import { ICustomResponseData } from '../filters/http-exception.filter';

@Injectable()
export class DataResponseInterceptor implements NestInterceptor {
  intercept(
    context: ExecutionContext,
    next: CallHandler,
  ): Observable<ICustomResponseData> {
    const [request, response] = context.getArgs();
    return next.handle().pipe(
      map(
        data =>
          ({
            http_code: <number>response.statusCode,
            timestamp: <string>new Date().toISOString(),
            path: <string>request.url,
            http_method: <string>request.method,
            data: data ? data : {} ,
          } as ICustomResponseData),
      ),
    );
  }
}

export const DataResponseInterceptorProvider = {
  provide: APP_INTERCEPTOR,
  useClass: DataResponseInterceptor,
};
