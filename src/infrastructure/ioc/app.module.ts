import { Module } from '@nestjs/common';
import { HttpExceptionFilterProvider } from '../rest/filters/http-exception.filter';
import { DataResponseInterceptorProvider } from '../rest/interceptors/data.response.interceptor';
import { SuggestionsModule } from './suggestions.module';
import { LoggingInterceptorProvider } from '../rest/interceptors/logging.interceptor';
import { AppController } from '@presentation/controllers/app.controller';
import { EnvironmentSharedModule } from '@domain/shared/environment/enviroment.shared.module';
import { CustomLoggerSharedService } from '@domain/shared/services/custom.logger.shared.service';
import { AppSharedService } from '@domain/shared/services/app.shared.service';
import { UtilsSharedService } from '@domain/shared/services/utils.shared.service';
@Module({
  imports: [
    EnvironmentSharedModule,
    SuggestionsModule,
    CustomLoggerSharedService,
  ],
  controllers: [AppController],
  providers: [
    AppSharedService,
    CustomLoggerSharedService,
    UtilsSharedService,
    LoggingInterceptorProvider,
    HttpExceptionFilterProvider,
    DataResponseInterceptorProvider,
  ],
})
export class AppModule {}
