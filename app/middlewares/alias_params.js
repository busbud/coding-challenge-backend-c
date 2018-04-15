/**
 * To Rename the req.params and the req.schemaParams based on the service's schema params' aliases
 * 
 * @author Mohammad Fares <faressoft.com@gmail.com>
 */

/**
 * To Rename the params based on the service's schema params' aliases 
 * 
 * @param {Object} req
 * @param {Object} res
 * @param {Object} next
 */
module.exports = function(req, res, next) {

  // Foreach schema's param
  for (var paramName in req.service.params) {
        
    // Is alias option defined
    if (di.is.propertyDefined(req.service.params[paramName], 'alias')) {

      // If the param is passed
      if (req.params[paramName]) {
        req.params[req.service.params[paramName]['alias']] = req.params[paramName];
        delete req.params[paramName];
      }

      // If the param exists in the schemaParams
      if (req.schemaParams[paramName]) {
        req.schemaParams[req.service.params[paramName]['alias']] = req.schemaParams[paramName];
        delete req.schemaParams[paramName];
      }

    }
      
  }

  next();

};
