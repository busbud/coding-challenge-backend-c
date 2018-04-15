/**
 * Custom Validators
 *
 * The defined validators can be used in two different ways:
 * 
 * - Param validator:
 *   # Validates a single value
 *   # Take one param (value)
 *   # Return true if valid, otherwise false or an error message
 *
 * - Schema validator:
 *   # Take any number of params (can be specefied within the schema)
 *   # Return true if valid, otherwise false or an error message
 *   # Defined within the request schema
 *   # The validate middleware will call the validator with with the defined params
 *   #   and the service schema as last argument
 *   #
 *   # Example
 *   # ------------
 *   # validators: [
 *   #   {
 *   #     validator: 'dateRange',
 *   #     arguments: [{paramName:'startDate'}, {paramName:'endDate'}, 222]
 *   #   }
 *   # ]
 * 
 * @author Mohammad Fares <faressoft.com@gmail.com>
 */

/**
 * Check if the value is a valid mysql date (yyyy-mm-dd)
 * 
 * @param  {String}  value
 * @return {Boolean}
 */
var mySQLDate = function(value) {

  return di.moment(value, 'YYYY-MM-DD HH:mm:ss', true).isValid() ||
         di.moment(value, 'YYYY-MM-DD', true).isValid();

};

/**
 * Add validators to validate module
 */
module.exports.sycnLoad = function() {

  di.validate.addValidator('mySQLDate', mySQLDate);
  
};
