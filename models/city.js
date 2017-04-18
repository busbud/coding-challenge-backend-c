'use strict'

const mongoose = require('mongoose');

require('mongoose-double')(mongoose);

var SchemaTypes = mongoose.Schema.Types;

const CitySchema = new mongoose.Schema({
	_id			: SchemaTypes.ObjectId,
    id			: Number,
	name 		: String,
	ascii		: String,
	alt_name	: String,
	lat			: SchemaTypes.Double,
	long		: SchemaTypes.Double,
	feat_class	: String,
	feat_code	: String,
	country		: String,
	cc2			: String,
	admin1		: String,
	admin2		: String,
	admin3		: String,
	admin4		: String,
	population	: Number,
	elevation	: Number,
	dem			: Number,
	tz			: String,
	modified_at	: Date
});

module.exports = mongoose.model('City', CitySchema, 'cities');
