import { Next, plugins, Response, Server } from 'restify'
import { Logger } from 'pino'
import { TYPES } from './utils/Types'
import { kernel } from './Kernel'
import { Request } from './utils/Request'
import { ServiceUnavailableError } from 'restify-errors'
import { InversifyRestifyServer } from 'inversify-restify-utils'
import { v4 as uuidv4 } from 'uuid'

/* eslint camelcase: 0, no-async-promise-executor: 0, consistent-return:0 */
export class App {
    private readonly logger: Logger
    private readonly port: number
    private server: Server
    private readonly shutdownWaitTimeout: number
    private isShuttingDown = false
    private readonly serverInitRequestId: string

    constructor(port = 80) {
        this.port = port
        this.logger = kernel.get<Logger>(TYPES.Logger)
        this.serverInitRequestId = uuidv4()
    }

    public getServer(): Server {
        return this.server
    }

    private showAppInfo(appId: string): void {
        this.server.get('/', (req: Request, res: Response, next: Next) => {
            if (this.isShuttingDown) {
                next(new ServiceUnavailableError('Server is shutting down'))
            } else {
                const memoryUsage: any = process.memoryUsage()
                memoryUsage.heap_total = memoryUsage.heapTotal
                delete memoryUsage.heapTotal
                memoryUsage.heap_used = memoryUsage.heapUsed
                delete memoryUsage.heapUsed

                res.json(200, {
                    app_id: appId,
                    app_version: `v${process.env.npm_package_version}`,
                    uptime: process.uptime(),
                    environment: process.env.NODE_ENV,
                    node_version: process.version,
                    platform: process.platform,
                    memory_usage: memoryUsage,
                    cpu_usage: process.cpuUsage()
                })
                next()
            }
        })
    }

    public stop(): Promise<void> {
        this.isShuttingDown = true
        if (this.server) {
            return new Promise<void>((resolve, reject) => {
                setTimeout(async () => {
                    try {
                        await this.server.close(() => {
                            this.logger.info({
                                msg: 'Suggestions server shut down successfully',
                                serverRunId: this.serverInitRequestId,
                                tags: ['start']
                            })
                        })
                        return resolve()
                    } catch (e) {
                        return reject(e)
                    }
                }, this.shutdownWaitTimeout)
            })
        } else {
            return Promise.resolve()
        }
    }

    public start(): Promise<App> {
        return new Promise<App>(async (resolve, reject) => {
            try {
                // initialize the server
                const appId = String(process.env.npm_package_name)

                const serverBuilder = new InversifyRestifyServer(kernel)

                serverBuilder.setConfig((server: Server) => {
                    // Load middlewares
                    server.use(plugins.queryParser())
                    server.use(plugins.bodyParser())
                    server.pre(plugins.pre.sanitizePath()) // Clean trailing slashes on routes

                    // Add CORS helper
                    server.use((req: Request, res: Response, next: Next) => {
                        res.header('Access-Control-Allow-Origin', '*')
                        next()
                    })
                })
                this.server = serverBuilder.build()

                // Display application information
                this.showAppInfo(appId)

                // initialize the server
                this.server.listen(this.port, () => {
                    this.logger.info({
                        msg: `Suggestions server listening at ${this.server.url}`,
                        serverRunId: this.serverInitRequestId,
                        tags: ['start']
                    })
                    return resolve(this)
                })
            } catch (e) {
                return reject(e)
            }
        })
    }
}
