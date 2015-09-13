var hasOwn = require('prime/object/hasOwn');

function fromPath(source, parts) {
	"use strict";

	if (typeof parts == 'string') parts = parts.split('.');
	for (var i = 0, l = parts.length; i < l; i++) {
		if (hasOwn(source, parts[i])) {
			source = source[parts[i]];
		} else {
			return null;
		}
	}
	return source;
}

module.exports = fromPath;