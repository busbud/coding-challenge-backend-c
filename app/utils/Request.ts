import { Logger } from 'pino'
import Ajv from 'ajv'
import { Request as RestifyRequest } from 'restify'

export type Request = RestifyRequest & { logger: Logger }

export const ajv = new Ajv({ allErrors: true, coerceTypes: true })
