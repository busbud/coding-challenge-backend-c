/**
 * Change The Case for Objects Keys
 * 
 * @author Mohammad Fares <faressoft.com@gmail.com>
 */

/**
 * Change the case of an object keys
 * 
 * @param  {Object} data
 * @param  {String} type one of (camel, pascal, title, snake, lower, upper, constant)
 * @return {Object}
 */
function objectChangeCase(data, type) {

  var result = {};
  var caseChangeMethod = di.changeCase[type];

  for (var i in data) {
    result[caseChangeMethod(i)] = data[i];
  }

  return result;
  
}

/**
 * Change the case of an object keys
 * 
 * @param  {Object|Array} data
 * @param  {String}       type one of (camel, pascal, title, snake, lower, upper, constant)
 * @return {Object|Array}
 */
module.exports = function(data, type) {


  if (di.is.array(data)) {

    var result = [];

    // Foreach element in the array
    for (var i in data) {
      result.push(objectChangeCase(data[i], type));
    }

    return result;

  }

  return objectChangeCase(data, type);

};
