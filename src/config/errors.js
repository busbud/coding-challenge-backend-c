'use strict';

/**
 * Error codes and messages
 * @type {{errorCodes: {APPLICATION_WAS_NOT_VALIDATED: {code: number, errorMessage: string}, AUTHENTICATION_IMPOSSIBLE: {code: number, errorMessage: string}, BAD_AUTHENTICATION_DATA: {code: number, errorMessage: string}, BAD_PARAMETER_FORMAT: {code: number, errorMessage: string}, ENDPOINT_RETIRED: {code: number, errorMessage: string}, EXTERNAL_ERROR: {code: number, errorMessage: string}, INTERNAL_ERROR: {code: number, errorMessage: string}, INVALID_OR_EXPIRED_TOKEN: {code: number, errorMessage: string}, MISSING_CONDITIONAL_PARAMETER: {code: number, errorMessage: string}, MISSING_MANDATORY_PARAMETER: {code: number, errorMessage: string}, OPERATION_ALREADY_ONGOING: {code: number, errorMessage: string}, OVER_CAPACITY: {code: number, errorMessage: string}, RESOURCE_EXISTS: {code: number, errorMessage: string}, TIMEOUT: {code: number, errorMessage: string}, UNEXPECTED_ERROR: {code: number, errorMessage: string}, UNKNOWN_RESOURCE: {code: number, errorMessage: string}}, generics: {MODULE_NOT_FOUND: string}}}
 */
module.exports = {
  errorCodes: {
    APPLICATION_WAS_NOT_VALIDATED: {
      code: 116,
      errorMessage: 'Application was not validated yet'
    },
    AUTHENTICATION_IMPOSSIBLE: {
      code: 100,
      errorMessage: 'Could not authenticate you'
    },
    BAD_AUTHENTICATION_DATA: {
      code: 101,
      errorMessage: 'Bad authentication data'
    },
    BAD_PARAMETER_FORMAT: {
      code: 102,
      errorMessage: 'Bad parameter format'
    },
    ENDPOINT_RETIRED: {
      code: 103,
      errorMessage: 'This endpoint has been retired and should not be used'
    },
    EXTERNAL_ERROR: {
      code: 104,
      errorMessage: 'External error'
    },
    INTERNAL_ERROR: {
      code: 105,
      errorMessage: 'Internal Error'
    },
    INVALID_OR_EXPIRED_TOKEN: {
      code: 106,
      errorMessage: 'Invalid or expired token'
    },
    MISSING_CONDITIONAL_PARAMETER: {
      code: 108,
      errorMessage: 'Missing conditional parameter'
    },
    MISSING_MANDATORY_PARAMETER: {
      code: 109,
      errorMessage: 'Missing mandatory parameter'
    },
    OPERATION_ALREADY_ONGOING: {
      code: 110,
      errorMessage: 'Operation already ongoing'
    },
    OVER_CAPACITY: {
      code: 111,
      errorMessage: 'Over capacity'
    },
    RESOURCE_EXISTS: {
      code: 115,
      errorMessage: 'RESOURCE EXISTS'
    },
    TIMEOUT: {
      code: 112,
      errorMessage: 'TIMEOUT'
    },
    UNEXPECTED_ERROR: {
      code: 113,
      errorMessage: 'Unexpected Error'
    },
    UNKNOWN_RESOURCE: {
      code: 114,
      errorMessage: 'Unknown RESOURCE'
    }
  },
  generics: {
    MODULE_NOT_FOUND: 'MODULE_NOT_FOUND'
  }
};
