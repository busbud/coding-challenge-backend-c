import { NestFactory } from '@nestjs/core';
import { AppModule } from './app.module';
import { LogLevel, ValidationPipe } from '@nestjs/common';

async function bootstrap() {
  const app = await NestFactory.create(AppModule, {
    logger: process.env.LOG_LEVELS
      ? (process.env.LOG_LEVELS.split(',') as LogLevel[])
      : ['log', 'debug', 'error', 'warn'],
  });
  app.useGlobalPipes(new ValidationPipe({ transform: true }));
  await app.listen(2345);
}
bootstrap();
