var mongoose = require('mongoose');
var Schema = mongoose.Schema;

// mongoose Schema for City
var citySchema = new Schema({
  id: {type: String, required: true, unique: true},
  name: {type: String, index: true},
  ascii: {type: String, required: true, index: true},
  alt_name: String,
  lat: {type: Number, required: true},
  long: {type: Number, required: true},
  country: {type: String, required: true},
  state: String,
  updated_at: Date,
  created_at: Date
});

citySchema.pre('save', function(next){
  var currentDate = new Date();
  this.updated_at = currentDate;
  if(!this.created_at){
    this.created_at = currentDate;
  }
  next();
})

var City = mongoose.model('City', citySchema);

module.exports = City;
