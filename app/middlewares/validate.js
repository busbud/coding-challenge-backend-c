/**
 * To validate and normalize params based on the defined schemas
 * 
 * @author Mohammad Fares <faressoft.com@gmail.com>
 */

/**
 * Normalize params
 * 
 * @param {Object} req
 */
function normalize(req) {

  // Foreach schema's param
  for (var paramName in req.service.params) {
        
    // Is normalization option defined
    if (di.is.propertyDefined(req.service.params[paramName], 'normalize')) {

      // Is array
      if (di.is.array(req.service.params[paramName]).normalize) {

        // Foreach normalize method
        for (var i in req.service.params[paramName].normalize) {

          // If the param is passed
          if (req.params[paramName]) {
            req.params[paramName] = di.normalizers[req.service.params[paramName].normalize[i]](req.params[paramName]);
          }

        }

      } else {

        // If the param is passed
        if (req.params[paramName]) {
          req.params[paramName] = di.normalizers[req.service.params[paramName].normalize](req.params[paramName]);
        }

      }

    }
      
  }

  di.validate.normalize(req.params, req.service.params);
  
}

/**
 * Validate the schema validators if exists
 * 
 * @param  {Object}         req
 * @return {Boolean|Object} return true if valid, otherwise false or {paramName: false, ....}
 */
function schemaValidator(req) {

  // Check if the (Schema Validators) are not specefied
  if (!di.is.propertyDefined(req.service, 'validators')) {
    return true;
  }

  // Foreach schema validator
  for (var i in req.service.validators) {
  
    // The used req params to call the validator with
    var reqParams = [];
    var schemaValidator = req.service.validators[i];
    var args = [];
    var isValid = false;
    
    // Foreach defined argument
    for (var j in schemaValidator.arguments) {
      
      // Is req param
      if (di.is.object(schemaValidator.arguments[j]) &&
          di.is.propertyDefined(schemaValidator.arguments[j], 'paramName')) {

        reqParams.push(schemaValidator.arguments[j].paramName);
        args.push(req.params[schemaValidator.arguments[j].paramName]);

      } else {

        args.push(schemaValidator.arguments[j]);

      }
    
    }

    // Add the service schema to the args
    args.push(di.deepcopy(req.service));

    // Call the validator with the arguments
    isValid = di.is[schemaValidator.validator].apply(null, args);

    // IS not valid
    if (isValid !== true) {

      // Return the params [{paramName: false}]
      if (di.is.boolean(isValid)) {

        var invalidParams = {};

        for (var i in reqParams) {
          invalidParams[reqParams[i]] = false;
        }

        return invalidParams;

      }

      return isValid;

    }
  
  }

  return true;
  
}

/**
 * To validate the input according the service's schema
 * 
 * @param {Object} req
 * @param {Object} res
 * @param {Object} next
 */
module.exports = function(req, res, next) {

  var invalidSchemaParams = {};

  // Check if the parameters are valid according to the schema (Param Validators)
  if (di.validate.isValid(req.params, req.service.params) && !req.params.client) {

    normalize(req);

    invalidSchemaParams = schemaValidator(req);

    if (invalidSchemaParams === true) {
      return next();
    }

  }

  // Response wrapper
  res = new di.Response(res);

  res.setStatus(false);

  if (di.is.string(invalidSchemaParams)) {

    res.error(invalidSchemaParams, {
      fields: di.deepmerge(invalidSchemaParams, di.validate.getFieldsStatus(req.params, req.service.params))
    }, 400);

  } else {

    res.error(di.text.get('ENTER_REQUIED'), {
      fields: di.deepmerge(invalidSchemaParams, di.validate.getFieldsStatus(req.params, req.service.params))
    }, 400);

  }

};
