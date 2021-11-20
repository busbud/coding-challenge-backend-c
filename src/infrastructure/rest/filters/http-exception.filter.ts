import {
  ExceptionFilter,
  Catch,
  ArgumentsHost,
  HttpException,
} from '@nestjs/common';
import { Request, Response } from 'express';
import { APP_FILTER } from '@nestjs/core';
import { EnvironmentSharedService } from '@domain/shared/environment/environment.shared.service';
import { CustomLoggerSharedService } from '@domain/shared/services/custom.logger.shared.service';
import { AppSharedService } from '@domain/shared/services/app.shared.service';
export interface ICustomResponseData {
  http_code: number;
  timestamp: string;
  path: string;
  http_method: string;
  error?: any;
  data?: any;
  stack?: any;
}
@Catch(HttpException)
export class HttpExceptionFilter implements ExceptionFilter {
  constructor(
    private readonly environmentSharedService: EnvironmentSharedService,
    private readonly customLoggerSharedService: CustomLoggerSharedService,
    private readonly appSharedService: AppSharedService,
  ) {}
  async catch(exception: HttpException, host: ArgumentsHost) {
    const ctx = host.switchToHttp();
    const response = ctx.getResponse<Response>();
    const request = ctx.getRequest<Request>();
    const status = exception.getStatus();
    const exceptionResponse = <any>exception.getResponse();
    if (exceptionResponse['statusCode']) delete exceptionResponse['statusCode'];
    const responseData = {
      http_code: status,
      timestamp: new Date().toISOString(),
      path: request.url,
      http_method: request.method,
      error: exceptionResponse,
      stack:
       this.environmentSharedService.getEnv('NODE_ENV') === 'development' ? exception.stack : undefined,
    } as ICustomResponseData;
    const { stack, ...loggingErrorData } = responseData;
    this.customLoggerSharedService.log(
      this.appSharedService.getAppLoggingData(
        responseData.path,
        responseData.http_method,
        request.ip,
        responseData.http_code,
      ),
      'HttpExceptionFilter',
    );
    this.customLoggerSharedService.error(
      JSON.stringify(loggingErrorData),
      stack,
      'HttpExceptionFilter',
    );
    response.status(status).json(responseData);
  }
}

export const HttpExceptionFilterProvider = {
  provide: APP_FILTER,
  useClass: HttpExceptionFilter,
};
