var mongoose = require('mongoose');

var locationSchema = new mongoose.Schema({
    id: Number,
    name: String,
    ascii: String,	
    alt_name:	
    lat: Number,
    longitude: Number,
    feat_class: String,	
    feat_code: String,
    country: String,	
    cc2: String,
    admin1: String,	
    admin2: String,	
    admin3: String,	
    admin4: String,	
    population:	Number,
    elevation:	Number,
    dem: Number,	
    tz:	String,
    modified_at: Date 
});
/*
  Model name, scheme for instantiating documents, and the collection it belongs to
*/
module.exports = mongoose.model('Locations', locationSchema, 'locations');
