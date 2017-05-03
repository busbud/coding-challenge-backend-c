'use strict';
/**
 * Loosely based on:
 * https://github.com/keifukuda/mongoose-visibility
 * https://github.com/mblarsen/mongoose-hidden
 */

var _ = require('lodash');

module.exports = function(schema, pluginOptions) {

    var _deleteNullUndefinedProperties = function(object) {
        for (var i in object) {
            if (object[i] === null || object[i] === undefined) {
                delete object[i];
            } else if (typeof object[i] === 'object') {
                _deleteNullUndefinedProperties(object[i]);
            }
        }
    };

    var _defaultPluginOptions = {
        visibleVirtuals: [],
        hidden: ['__v'],
        rename: {}
    };

    pluginOptions = pluginOptions || {};
    pluginOptions = _.extend(_defaultPluginOptions, pluginOptions);

    var toJSONOptions = schema.get('toJSON') || {};

    schema.set('toJSON', {
        getters: toJSONOptions['getters'] || false,
        virtuals: toJSONOptions['virtuals'] || false,
        transform: function(document, object, options) {

            object._id = object._id.toString();

            _.forEach(pluginOptions.hidden, function(value) {
                if (!value) {
                    return;
                }

                if (typeof value === 'object') {
                    _.forEach(value, function(val, key) {
                        if (val === true) {
                            _.set(object, key, undefined);
                        } else if (typeof val === 'function') {
                            if (val(document)) {
                                _.set(object, key, undefined);
                            }
                        }
                    });
                } else {
                    _.set(object, value, undefined);
                }
            });

            if (!options.virtuals && !options.getters) {
                _.forEach(pluginOptions.visibleVirtuals, function(value) {
                    _.set(object, value, _.get(document, value));
                });
            }

            _.forEach(pluginOptions.rename, function(value, key) {
                _.set(object, value, _.get(object, key));
                _.set(object, key, undefined);
            });

            // Deleting all undefined (TODO: find a way to do it directly instead of assigning undefined and then deleting)
            _deleteNullUndefinedProperties(object);

            return object;
        }
    });
};
