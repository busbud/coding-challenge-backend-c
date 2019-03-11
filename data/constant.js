const mongoose = require('mongoose');
const Schema = mongoose.Schema;
const ConstantSchema = new Schema({
  name: {
    type: String,
    required: true,
    unique: true,
    index: true
  },
  value: String
});
const ConstantModel = mongoose.model('Constant', ConstantSchema);
module.exports = ConstantModel;
