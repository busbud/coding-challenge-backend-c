const mongoose = require('mongoose');

mongoose.connect(process.env.MONGODB_URI || 'mongodb://127.0.0.1:27018/Suggestions',
  { useNewUrlParser: true });

module.exports = {mongoose};
