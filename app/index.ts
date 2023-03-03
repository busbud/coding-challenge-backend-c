import { Logger } from 'pino'
import { App } from './App'
import { kernel } from './Kernel'
import { TYPES } from './utils/Types'

const logger = kernel.get<Logger>(TYPES.Logger)

const app = new App(80)

function uncaughtException(err: Error): void {
    logger.error(err.message, {
        tags: ['uncaught', 'exception'],
        stack: err.stack
    })
    process.exit(1)
}
process.on('uncaughtException', uncaughtException)
process.on('SIGINT', () => {
    app.stop().then(() => process.exit(0))
})

app.start().catch(uncaughtException)
