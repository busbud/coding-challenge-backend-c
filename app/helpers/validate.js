/**
 * Object Validation via A Validation Schema
 * 
 * @author Mohammad Fares <faressoft.com@gmail.com>
 */

/**
 * To add validator function
 * 
 * @param {String}   name
 * @param {Function} callback
 */
module.exports.addValidator = function(name, callback) {

  di.is.not[name] = function() {
    return !callback.apply(di.is, di.lodash.values(arguments));
  };

  di.is[name] = function() {
    return callback.apply(di.is, di.lodash.values(arguments));
  };

};

/**
 * Check for a field if it valid according to a specific schema
 *
 * @return {Boolean}
 */
var checkField = function(fieldSchema, value) {

  // Validate files & images
  if (di.is.propertyDefined(fieldSchema, 'type') &&
      (fieldSchema.type == 'image' || fieldSchema.type == 'file')) {

    // Required single file not multiple
    if ((!di.is.propertyDefined(fieldSchema, 'max-count') ||
        fieldSchema['max-count'] == 1) && di.is.array(value)) {
      return false;
    }

    // To store file's paths to be verified
    var paths = [];

    return true;

  }

  // Check type
  if (di.is.propertyDefined(fieldSchema, 'type') &&
      di.is.not[fieldSchema['type']](value)) {
    return false;
  }

  // Is
  if (di.is.propertyDefined(fieldSchema, 'is') &&
      di.is.string(fieldSchema.is) &&
      di.is.not[fieldSchema.is](value)) {
    return false;
  }

  // Is (array of conditions)
  if (di.is.propertyDefined(fieldSchema, 'is') &&
      di.is.array(fieldSchema.is)) {

    var valid = true;

    fieldSchema.is.forEach(function(condition) {

      if (di.is.propertyDefined(di.is, condition) && di.is.not[condition](value)) {
        valid = false;
        return false;
      }

    });

    if (!valid) {
      return false;
    }

  }

  // Is (object: functionName:[parameters,...])
  if (di.is.propertyDefined(fieldSchema, 'is') &&
      di.is.not.array(fieldSchema.is) &&
      di.is.object(fieldSchema.is)) {

    for (var i in fieldSchema.is) {

      var param = fieldSchema.is[i];
      var len = fieldSchema.is[i].length;

      if (di.is.equal(len, 1) && di.is.not[i](value, param[0])) {
        return false;
      }

      if (di.is.equal(len, 2) && di.is.not[i](value, param[0], param[1])) {
        return false;
      }

      if (di.is.equal(len, 3) && di.is.not[i](value, param[0], param[1], param[3])) {
        return false;
      }

    }

  }

  return true;

};

module.exports.validate = function(data, schema, normalize) {

  if (di.is.undefined(normalize) || !normalize) {
    data = di.deepcopy(data);
  }

  var fields = {};
  var status = {};
  var notSetNotRequiredFields = [];

  for (name in schema) {

    var fieldSchema = schema[name];
    var valid = true;

    // Required, Not Exist
    if (di.is.propertyDefined(fieldSchema, 'required') &&
        di.is.truthy(fieldSchema.required) &&
        (di.is.undefined(data[name]) || di.is.empty(data[name]))) {
      valid = false;
    }

    // Not Required, Not Exist
    if ((di.is.not.propertyDefined(data, name) || di.is.empty(data[name])) &&
        di.is.truthy(valid)) {

      notSetNotRequiredFields.push(name);
      valid = true;

      if (di.is.propertyDefined(fieldSchema, 'type') &&
          di.is.equal(fieldSchema['type'], 'array')) {
        data[name] = [];
      }

    } else if (valid) {

      // If it is type is number, convert the value to a number
      if (di.is.propertyDefined(fieldSchema, 'type') &&
          di.is.equal(fieldSchema['type'], 'number')) {
        data[name] = parseFloat(data[name]);
      }
        
      // If it is type is array and the value is valid, convert the value to a array
      if (di.is.propertyDefined(fieldSchema, 'type') &&
          di.is.equal(fieldSchema['type'], 'array') &&
          di.is.string(data[name])) {
        data[name] = data[name].replace(/, /g, ',').split(',');
      }
        
      // If it is type is boolean, convert the value to a boolean
      if (di.is.propertyDefined(fieldSchema, 'type') &&
          di.is.equal(fieldSchema['type'], 'boolean')) {
        if (data[name] == 'true' || data[name] == '1') {
          data[name] = true;
        } else if (data[name] == 'false' || data[name] == '0') {
          data[name] = false;
        }
      }
        
      valid = checkField(fieldSchema, data[name]);
      
    }

    if (valid && di.is.not.undefined(data[name])) {
      fields[name] = data[name];
    }

    status[name] = valid;

  }

  return {status: status, fields: fields, notSetNotRequiredFields: notSetNotRequiredFields};

};

module.exports.normalize = function(data, schema) {

  module.exports.validate(data, schema, true);

};

module.exports.getValidFields = function(data, schema) {

  return module.exports.validate(data, schema).fields;

};

module.exports.getFieldsStatus = function(data, schema) {

  return module.exports.validate(data, schema).status;

};

module.exports.getNotSetNotRequiredFields = function(data, schema) {

  return module.exports.validate(data, schema).notSetNotRequiredFields;

};

module.exports.isValid = function(data, schema) {
  
  // Deep copy the data
  data = di.deepcopy(data);

  var valid = true;
  var status = module.exports.validate(data, schema).status;

  for (var i in status) {
    if (!status[i]) {
      valid = false;
      break;
    }
  }

  return valid;

};
