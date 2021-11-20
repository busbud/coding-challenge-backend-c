import { HttpException, HttpStatus } from '@nestjs/common';
import { ExceptionCodes } from './codes';

export class FullCoodinatesAreMandatoryException extends HttpException {
  constructor(message?: any) {
    super({ exception_code: ExceptionCodes.FULL_COORDINATES_ARE_MANDATORY_EXCEPTION ,message: message ? message : `if latitude or longitude was sent, both of these query strings must be sent`  }, HttpStatus.BAD_REQUEST);
  }
}

