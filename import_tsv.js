var mongo = require('mongodb'),
    monk = require('monk'),
    db = monk('localhost:27017/cities'),
    citiesDb = db.get('cities'),
    csv = require('csv-streamify'),
    parser = csv({delimiter: '\t', columns: true, objectMode: true}, callback);

// Keeping track of the state
var doneReading = false,
    pendingCount = 0,
    addedCount = 0;

// Reading the TSV file
parser.on('readable', function () {
  var city = parser.read();
  // Only index the city if it has a large enough population count
  if (city.population > 5000) {
    pendingCount++;
    addedCount++;
    citiesDb.insert({
      name: city.name,
      alt_name: city.alt_name,
      lat: city.lat,
      long: city.long,
      country: city.country,
      province: city.admin1
    }, function(err, doc) {
      // Handles error and exiting the script once every qualified city is in the db
      pendingCount--;
      if (err) return gracefulError(err);
      if (doneReading && pendingCount === 0) {
        console.log('All done, ' + addedCount + ' cities added.');
        db.close();
      }
    });
  }
});

// Setting the flag that we're done reading the TSV file
parser.on('end', function() {
  doneReading = true;
});

function callback(err, doc) {
  if (err) return gracefulError(err);
}

function gracefuleError(err) {
  if (err) throw err;
}

// Allows calling the script with `cat file.tsv | node script`
process.stdin
.pipe(parser);
