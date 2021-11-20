import { AppSharedService } from '@domain/shared/services/app.shared.service';
import { CustomLoggerSharedService } from '@domain/shared/services/custom.logger.shared.service';
import {
  Injectable,
  NestInterceptor,
  ExecutionContext,
  CallHandler,
} from '@nestjs/common';
import { APP_INTERCEPTOR } from '@nestjs/core';
import { Observable } from 'rxjs';
import { tap } from 'rxjs/operators';

@Injectable()
export class LoggingInterceptor implements NestInterceptor {
  constructor(
    private readonly customLoggerSharedService: CustomLoggerSharedService,
    private readonly appSharedService: AppSharedService,
  ) {}
  intercept(context: ExecutionContext, next: CallHandler): Observable<any> {
    const [request, response] = context.getArgs();
    const now = Date.now();
    return next
      .handle()
      .pipe(
        tap(() =>
          this.customLoggerSharedService.log(
            `${this.appSharedService.getAppLoggingData(
              <string>request.url,
              <string>request.method,
              <string>request.ip,
              <number>response.statusCode,
            )} +${Date.now() - now}ms`,
            'LoggingInterceptor',
          ),
        ),
      );
  }
}

export const LoggingInterceptorProvider = {
  provide: APP_INTERCEPTOR,
  useClass: LoggingInterceptor,
};
