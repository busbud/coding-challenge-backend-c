/**
 * Set the default values for unpassed params according to the schema
 * 
 * @author Mohammad Fares <faressoft.com@gmail.com>
 */

/**
 * Set the default values for unpassed params according to the schema
 * 
 * @param {Object} req
 * @param {Object} res
 * @param {Object} next
 */
module.exports = function(req, res, next) {

  var schema = req.service.params;
  var notSetNotRequiredFields = di.validate.getNotSetNotRequiredFields(req.params, schema);

  for (var i in notSetNotRequiredFields) {

    var name = notSetNotRequiredFields[i];

    if (di.is.propertyDefined(schema[name], 'default')) {
      req.params[name] = schema[name].default;
    }

  }
  
  next();

};
