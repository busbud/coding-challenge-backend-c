import { HttpException, HttpStatus } from "@nestjs/common";
import { ExceptionCodes } from "./codes";

export class InternalErrorException extends HttpException {
    constructor(message?: any) {
        super({ exception_code: ExceptionCodes.GENERIC_INTERNAL, message }, HttpStatus.BAD_REQUEST)
    }
}

export class EnvironmentVariableNotFound extends HttpException {
    constructor(envName: string) {
        super({ exception_code: ExceptionCodes.ENVIRONMENT_VARIABLE_NOT_FOUND, message : `environment var '${envName}' not found/not loaded.` }, HttpStatus.INTERNAL_SERVER_ERROR)
    }
}
export class MissingFieldOrInvalidTypeException extends HttpException {
    constructor(message?: any) {
        super({ exception_code: ExceptionCodes.MISSING_FILED_OR_INVALID_TYPE, message }, HttpStatus.BAD_REQUEST)
    }
}