import { Module } from '@nestjs/common';
import { EnvironmentSharedService } from './environment.shared.service';

@Module({
  providers: [{ provide: EnvironmentSharedService, useClass: EnvironmentSharedService }],
  exports: [EnvironmentSharedService],
})
export class EnvironmentSharedModule {}
