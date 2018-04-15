/**
 * A Wrapper Around Express Res
 * 
 * @author Mohammad Fares <faressoft.com@gmail.com>
 */

/**
 * A wrapper for response data
 * 
 * @param  {Object} res
 * @param  {Object} response
 */
module.exports = function(res, response) {

  var self = this;
  
  self.res = res;
  
  // Clone
  if (typeof response == 'undefined') {

    response = {
      meta: {
        status: true,
        message: ''
      },
      data: {}
    };
    
  }

  self.get = function() {
    return response;
  };

  self.get = res.get;
  self.end = res.end;
  self.write = res.write;

  self.set = function() {
    res.set.apply(res, arguments);
  };

  self.sendData = function(data) {
    res.send(data);
  };

  self.sendFile = function(path, options, callback) {

    if (typeof options === 'undefined') {
      options = {};
    }

    if (typeof callback === 'undefined') {
      callback = null;
    }

    res.sendFile(path, options, callback);

  };

  self.send = function(data) {

    if (typeof data !== 'undefined') {
      response.data = data;
    }

    if (res.only_meta) {
      delete response.data;
    }

    res.send(response);

  };

  self.error = function(message, data, statusCode) {

    if (typeof statusCode == 'undefined') {
      statusCode = 200;
    }

    if (typeof data == 'undefined') {
      data = {};
    }

    self.setMessage(message);
    self.setStatus(false);
    self.setData(data);
    res.status(statusCode);
    res.send(response);

  };

  self.status = function(statusCode) {
    res.status(statusCode);
  }

  self.setStatus = function(status) {
    response.meta.status = status;
  };

  self.getStatus = function(status) {
    return response.meta.status;
  };

  self.setMessage = function(message) {
    response.meta.message = message;
  };

  self.getMessage = function(message) {
    return response.meta.message;
  };

  self.setData = function(data) {
    response.data = data;
  };

  self.getData = function(data) {
    return response.data;
  };

  self.setPages = function(pages) {
    response.meta.pages = pages;
  };

  self.getPages = function(pages) {
    return response.meta.pages;
  };

  self.setExtraMeta = function(key, value) {
    response.meta[key] = value;
  };

  self.getMeta = function() {
    return response.meta;
  };

};
