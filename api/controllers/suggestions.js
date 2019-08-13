const fs = require('fs');
const csv = require('csv-streamify');
const JSONStream = require('JSONStream');
const sort = require('sort-stream');
const FilterCitiesStream = require('../lib/Filter-cities-stream');
const { query, validationResult } = require('express-validator');

exports.validate = () => {
  return [
    query('q').exists().withMessage('q is Required and must be only alphabetical chars')
      .matches(/^[a-z ]+$/i).withMessage('Name can only contain letters and spaces')

  ];
};

exports.cities_get_filtered = (req, res, next) => {
  const errors = validationResult(req);

  if (!errors.isEmpty()) {
    res.status(422).json({ errors: errors.array() });
    return;
  }

  const filter = new FilterCitiesStream({
    fields: ['name', 'latitude', 'longitude', 'country', 'score'],
    match: req.query['q'],
    latitude: req.query['latitude'],
    longitude: req.query['longitude']
  });


  const readStream = fs.createReadStream(process.env.DATA_FILE);
  const parser = csv({ objectMode: true, columns: true, delimiter: '\t' }, function(err, result) {
    if (err) throw err;
  });

  res.header('Content-Type', 'application/json');

  res.on('data', function(data) {
    // console.log(data);
  });

  let dataCount = 0;


  readStream.on('open', function() {
    readStream.pipe(parser)
      .pipe(filter).on('data', (data) => {
        // console.log(`[${data}]`);
        if (data) dataCount++;
      }).on('end', () => {
        if (dataCount === 0) {
          res.status(404);
          res.send(JSON.stringify({
            suggestions: []
          }));
          /*    res.send(JSON.stringify({
                                message:"No Results",
                                query: req.query,
                                status: 404
                            })); */
        }
      })
      .pipe(sort(function(a, b) {
        return a['score'] < b['score'];
      }))
      .pipe(JSONStream.stringify('{ "suggestions":[\n', '\n,\n', '\n]}\n'))
      .pipe(res).on('error', (err) => {
        if (err.code !== 'ERR_STREAM_WRITE_AFTER_END') {
          throw (err);
        }
      });
  });

  readStream.on('error', function(err) {
    console.log(err);
    throw (err);
  });
};
