// @class master

var logger = require('../helpers/logger'),
    cluster = require('cluster'),
    App = require('../application'),
    __bind = function(fn, me){ return function(){ return fn.apply(me, arguments); }; },
    DEBUG_MODE = process.env.DEBUG || false;

module.exports = (function() {
  'use strict';

  // @constructor
  // @param {string} name
  // @param {number} cpus
  function Master(name, cpus) {
    var self = this;
    this.log = logger('cluster:' + name + ':master');
    this._workers = [];

    if(cpus){
      for (var i = 0; i < cpus; i++) {
        var worker = cluster.fork();
        this._workers.push(worker);
      }
    }

  }

  //@return {Array} workers list
  Master.prototype.workers = function() {
    return this._workers;
  }

  return Master;

})();