import { Injectable } from '@nestjs/common';
import * as dotenv from 'dotenv';
import { EnvironmentVariableNotFound } from '@domain/exceptions/internal.exception';
@Injectable()
export class EnvironmentSharedService {
	private static _loadedEnvs: string | number | { [key: string]: string };
	constructor() {
		if (!EnvironmentSharedService._loadedEnvs) {
			const { parsed } = dotenv.config({
				path: `env-files/.config.${(process.env.NODE_ENV || 'development') as string}.env`
			});
			EnvironmentSharedService._loadedEnvs = parsed;
		}
	}
	getEnv(envName: string) {
		if (!EnvironmentSharedService._loadedEnvs[envName]) throw new EnvironmentVariableNotFound(envName);
		return EnvironmentSharedService._loadedEnvs[envName];
	}
}
