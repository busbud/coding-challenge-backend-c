/*
  Populates the geoname table in db. 

  Table should exist already, and should be executed from project root directory.
*/
if (process.env.NODE_ENV === "development") require('dotenv').load();

var fs = require('fs');
var path = require('path');
var pg = require('pg');

pg.defaults.poolIdleTimeout = 150000; // 2.5 minutes

var insert_sql = fs.readFileSync(path.join('data', 'sql', 'geoname_insert.sql')).toString();
var create_sql = fs.readFileSync(path.join('data', 'sql', 'geoname_schema.sql')).toString();

pg.connect(process.env.DATABASE_URL, function(err, client, done) {
  if (err) {
    return console.error('error fetching client from pool', err);
  }

  client.query(create_sql, function(err, result) {
    done();
    if(err) {
      return console.error('error running query', err);
    }
    console.log("Geoname table created");

    pg.connect(process.env.DATABASE_URL, function(err, client, done) {
      var count = 0;
      if (err) {
        return console.error('error fetching client from pool', err);
      }

      fs.readFile(path.join('data', 'cities_canada-usa.tsv'), function(err, data) {
        if (err) {
          return console.error(err);
        }
        data = data.toString();

        // Skip first line
        var index = data.indexOf('\n');
        data = data.substring(index + 1);

        while (data.indexOf('\n') > -1) {
          var line_index = data.indexOf('\n');
          var line = data.substring(0, line_index);
          var values = [];
          // Grab each tab-separated value and add it to array
          while (line.indexOf('\t') > -1) {
              var index = line.indexOf('\t');
              var value = line.substring(0, index);
              values.push(value);
              line = line.substring(index + 1);
          }
          values.push(line); // Grab last value

          // Turn empty strings in to null
          values = values.map(function(value) {
            if (!value.length) return null;
            return value;
          })

          console.log("Added:", values[0], values[1], "to Queue");
          client.query(insert_sql, values, function(err, result) {
            done();
            if(err) {
              return console.error('error running query', err);
            }
            console.log("Entry #", count, "added successfully");
            count++;
          });

          data = data.substring(line_index + 1);
        }
      });
    });
  });
});