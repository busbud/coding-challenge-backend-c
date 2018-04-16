/**
 * Join body, files into params
 * 
 * @param {Object} req
 * @param {Object} res
 * @param {Object} next
 */
module.exports = function(req, res, next) {

  // Merge body parameters with request params
  if (di.is.propertyDefined(req, 'body')) {
    req.params = di.merge(req.params, req.body);
  }

  // Merge query parameters with request params
  if (di.is.propertyDefined(req, 'query')) {
    req.params = di.merge(req.params, req.query);
  }

  // Is service
  if (req.service) {

    var schema = req.service.params;
    
    // For each param
    for (var param in schema) {

      // Is file or image
      if (di.is.propertyDefined(schema[param], 'type') &&
         (schema[param].type == 'file' || schema[param].type == 'image')) {
        // Remove it from req.params if exists
        delete req.params[param];
      }

    }

  }

  // Merge files parameters with request params
  if (di.is.propertyDefined(req, 'files')) {
    req.params = di.merge(req.params, req.files);
  }

  next();
  
};
