import { Logger } from 'pino'
import Ajv from 'ajv'
import ajvKeywords from 'ajv-keywords'
import { Request as RestifyRequest } from 'restify'

export type Request = RestifyRequest & { logger: Logger }

const ajv = new Ajv({ allErrors: true, coerceTypes: true })
ajvKeywords(ajv, ['transform'])

export { ajv }
