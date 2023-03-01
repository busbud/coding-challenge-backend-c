import { Container } from 'inversify'
import pino, { Logger } from 'pino'
import 'reflect-metadata'
import { Firestore } from '@google-cloud/firestore'

import { TYPES } from './utils/Types'
import { SuggestionsController } from './controllers/SuggestionsController'
import { SuggestionsService } from './services/SuggestionsService'
import { FirestoreRepository } from './repositories/FirestoreRepository'

if (!process.env.npm_package_name || !process.env.npm_package_version) {
    /* eslint camelcase: 0 */
    // eslint-disable-next-line @typescript-eslint/no-require-imports, @typescript-eslint/no-var-requires
    const p = require('../package.json')
    process.env.npm_package_name = p.name
    process.env.npm_package_version = p.version
}

//process.env.GOOGLE_APPLICATION_CREDENTIALS = '../googleApplicationCred.json'
process.env.GOOGLE_APPLICATION_CREDENTIALS = 'AIzaSyCsP138Gimko8NIcci5USqzG81sV_svYmw'

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

// bind DB repositories
const firestoreDb = new Firestore({
    projectId: 'abiding-cistern-379308',
    keyFilename: 'googleApplicationCred.json'
})

kernel.bind<Firestore>(TYPES.Firestore).toConstantValue(firestoreDb)

kernel
    .bind<FirestoreRepository>(TYPES.IDatabaseRepository)
    .to(FirestoreRepository)
    .inSingletonScope()
    .whenTargetNamed(TYPES.FirestoreRepository)

export { kernel }
