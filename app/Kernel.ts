import { Container } from 'inversify'
import pino, { Logger } from 'pino'
import 'reflect-metadata'
import { Firestore } from '@google-cloud/firestore'
import { RedisClientType, createClient } from 'redis'
import cacheManager, { CacheManagerOptions } from '@type-cacheable/core'

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
    projectId: process.env.FIRESTORE_PROJECT_ID,
    keyFilename: 'googleApplicationCred.json'
})

kernel.bind<Firestore>(TYPES.Firestore).toConstantValue(firestoreDb)
kernel
    .bind<FirestoreRepository>(TYPES.IDatabaseRepository)
    .to(FirestoreRepository)
    .inSingletonScope()
    .whenTargetNamed(TYPES.FirestoreRepository)

const redisClient: RedisClientType = createClient({
    password: process.env.REDIS_PASSWORD,
    socket: {
        host: process.env.REDIS_HOST,
        port: +process.env.REDIS_PORT
    }
})
cacheManager.setOptions({
    excludeContext: false,
    ttlSeconds: 0
} as CacheManagerOptions)

kernel.bind<RedisClientType>(TYPES.Redis).toConstantValue(redisClient)

export { kernel }
