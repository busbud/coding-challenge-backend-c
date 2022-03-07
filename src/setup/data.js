const { default: mongoose } = require('mongoose');
const tsv_json = require('node-tsv-json');
const Location = require('../models/location.model');
const data_file = 'data/cities_canada-usa.tsv';
require('dotenv').config();

var mongo_uri = process.env.MONGO_URI;

exports.import_data_to_db = async () => {
  await mongoose
    .connect(mongo_uri, {
      keepAlive: 1,
    })
    .then(() => console.log('mongoDB connected...'));

  const json = tsv_json(
    {
      input: data_file,
      output: 'output.json',
      parseRows: false,
    },
    async (err, result) => {
      if (err) {
        console.error(err);
        mongoose.connection.close();
      } else {
        try {
          const res = await Location.insertMany(result);
          if (res) {
            mongoose.connection.close();
          }
        } catch (error) {
          console.error(error);
          mongoose.connection.close();
        }
      }
    }
  );
};
