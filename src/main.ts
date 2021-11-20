import { NestFactory } from '@nestjs/core';
import { DocumentBuilder, SwaggerModule } from '@nestjs/swagger';
import { AppModule } from '@infrastructure/ioc/app.module';
import * as helmet from 'helmet';
import { EnvironmentSharedService } from '@domain/shared/environment/environment.shared.service';
import { CustomLoggerSharedService } from '@domain/shared/services/custom.logger.shared.service';

async function bootstrap() {
	const app = await NestFactory.create(AppModule);
	app.enableCors();
	app.use(helmet());
	const environmentSharedService: EnvironmentSharedService = app.get(EnvironmentSharedService);
	const options = new DocumentBuilder()
		.setTitle('Busbud API Test - André Drumond das Chagas - Brazil')
		.addTag(environmentSharedService.getEnv('API_VERSION'))
		.setDescription('Linkedin => https://www.linkedin.com/in/andre-drumond/')
		.setVersion(environmentSharedService.getEnv('API_VERSION'))
		.build();
	const document = SwaggerModule.createDocument(app, options);

	SwaggerModule.setup(environmentSharedService.getEnv('API_DOC_ENDPOINT'), app, document);
	const logger: CustomLoggerSharedService = app.get(CustomLoggerSharedService);

	await app.listen(environmentSharedService.getEnv('SERVER_PORT'), () => {
		logger.log(`API running in '${environmentSharedService.getEnv('NODE_ENV')}' environment`, 'AppConfiguration');
		logger.log(`API running on port ${environmentSharedService.getEnv('SERVER_PORT')}`, 'AppConfiguration');
	});
}
bootstrap();
