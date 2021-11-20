import {  Injectable } from '@nestjs/common';
import { AppLoggingDto } from '@presentation/dtos/app.dto';
import { EnvironmentSharedService } from '../environment/environment.shared.service';
import { UtilsSharedService } from './utils.shared.service';

@Injectable()
export class AppSharedService {
  constructor(
    private environmentSharedService: EnvironmentSharedService,
    private readonly utilsSharedService: UtilsSharedService,
  ) {}
  rootPathMsg() {
    return {
      status: true,
      data: `root path`,
      api_version: this.environmentSharedService.getEnv('API_VERSION'),
      environment: this.environmentSharedService.getEnv('NODE_ENV'),
    };
  }
  getAppLoggingData(
    path: string,
    http_method: string,
    requester_ip: string,
    http_code,
  ): string {
    const loggingData = {
      server: this.utilsSharedService.currentIpAddress(),
      method: http_method,
      statusCode: http_code,
      routePath: path,
      requestFrom: requester_ip,
      date: this.utilsSharedService.current_date(),
    } as AppLoggingDto;
    return JSON.stringify(loggingData);
  }
}
