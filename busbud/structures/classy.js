

/********************
 * Import Modules
 *******************/

var _ = require("lodash");




/********************
 * Classy Class class with some extra class, nothing fancy to see here
 *******************/

// This object is pretty much an empty object with "Backbone.extend", to help for easier prototype chaining
var Classy = function() {};

Classy.extend = function (prototypeProperties, staticProperties) {
  // Parent is the "class" on which the extend has been called
  // Child is the new "class" which is to be instanced later on
  var parent = this;
  var child;

  // Check if constructor exists in hash to use it as the constructor for child
  // Otherwise use the parent's constructor
  if (prototypeProperties && _.has(prototypeProperties, "constructor")) {
    child = prototypeProperties.constructor;
  } else {
    // To be noted in the constructor "this" will point to an instance of child
    child = function () { return parent.apply(this, arguments); };
  }

  // Assign parent's properties into child; it's to pass "static" functions
  // Therefore a child can use extend to create a grandchild
  _.assign(child, parent);
  if (staticProperties) {
    _.assign(child, staticProperties);
  }

  // Prototype chaining without calling parent's constructor; CoffeeScript and Backbone use the same trick
  var ctor = function () {
    this.constructor = child;
  };
  ctor.prototype = parent.prototype;
  child.prototype = new ctor; // Child gets all from parent

  // Add the properties given to extend
  if (prototypeProperties) {
    _.assign(child.prototype, prototypeProperties);
  }

  // Easy access to parent from child if needed
  child.__super__ = parent.prototype;
  child.prototype._super_ = parent.prototype;

  return child;
};




/********************
 * Export Modules
 *******************/

module.exports = Classy
