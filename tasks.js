/**
 * Run tasks
 * 
 * @author Mohammad Fares <faressoft.com@gmail.com>
 */

/**
 * Run tasks
 * 
 * @param  {Array} serviceTasks array of tasks methods
 * @param  {Object} req
 * @param  {Object} res
 */
module.exports = function(serviceTasks, req, res) {

  var tasks = [];

  for (var i in serviceTasks) {
    tasks.push(serviceTasks[i](req, res));
  }

  di.async.waterfall(tasks, function(error, result) {

    if (error) {
      return res.error(error);
    }

    di.router.executeAfter(req, result).then(function(result) {
    
      res.send(result);
    
    }).catch(function(error) {
    
      res.error(error);
      
    });

  });

};
