import { Container } from 'inversify'
import pino, { Logger } from 'pino'
import 'reflect-metadata'

import { TYPES } from './utils/Types'
import { SuggestionsController } from './controllers/SuggestionsController'
import { SuggestionsService } from './services/SuggestionsService'
import { IDatabaseRepository } from './repositories/IDatabaseRepository'
import { FirebaseRepository } from './repositories/FirebaseRepository'

if (!process.env.npm_package_name || !process.env.npm_package_version) {
    /* eslint camelcase: 0 */
    // eslint-disable-next-line @typescript-eslint/no-require-imports, @typescript-eslint/no-var-requires
    const p = require('../package.json')
    process.env.npm_package_name = p.name
    process.env.npm_package_version = p.version
}

const kernel = new Container()

//bind utils
const loggerEnabled = process.env.NODE_ENV !== 'test'
const logger = pino({ name: process.env.NODE_ENV, level: 'debug', enabled: loggerEnabled })
kernel.bind<Logger>(TYPES.Logger).toConstantValue(logger)

//bind controllers
kernel
    .bind<SuggestionsController>(TYPES.Controller)
    .to(SuggestionsController)
    .whenTargetNamed(TYPES.SuggestionsController)

//bind services
kernel
    .bind<SuggestionsService>(TYPES.Service)
    .to(SuggestionsService)
    .inSingletonScope()
    .whenTargetNamed(TYPES.SuggestionsService)

kernel
    .bind<FirebaseRepository>(TYPES.IDatabaseRepository)
    .to(FirebaseRepository)
    .inSingletonScope()
    .whenTargetNamed(TYPES.FirebaseRepository)

export { kernel }
