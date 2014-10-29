/****************************************************************************
 * TO-DO : Design an API endpoint that provides auto-complete suggestions for 
 * large cities. The suggestions should be restricted to cities in the 
 * USA and Canada with a population above 5000 people.
 * Author : Keith Beaudoin
 * Email : keith.beaudoin@gmail.com
 * Version : 1.0
 * Test url = https://glacial-tundra-7317.herokuapp.com/suggestions?q=Londo&latitude=43.70011&longitude=-79.4163
 ****************************************************************************/
var utils = require('./common/utils').Utils,
    express = require('express'),
    flow = require('flow'),
    mongo = require('mongodb'),
    port = process.env.PORT || 2345,
    app = express(),
    server = app.listen(port),
    caCodes = {"1": "AB",
               "2": "BC",
               "3": "MB",
               "4": "NB",
               "5": "NF",
               "7": "NS",
               "8": "ON",
               "9": "PE",
               "10": "QC",
               "11": "SK",
               "12": "YT",
               "13": "NT",
               "14": "NU"};

//export the app
module.exports = app;

//search
app.get('/suggestions', function (req, res) {
  if (req.query.q) {
    flow.exec(
      // Open the database.
      function () {
        this.db = new mongo.Db("heroku_app31052532", new mongo.Server("ds047940.mongolab.com",47940, {}), {safe:true});
        this.db.open(this);
      },
      function () {
        this.db.authenticate("kbbusbud", "tghnnibq", this);
      },
      // Get the collection
      function (err) {
        this.db.collection('cities', this);
      },
      // get data
      function (err, collection) {
        var filters =  {"ascii":new RegExp(utils.removeAccents(req.query.q)+".*","i"),
                        "population":{$gte:5000}, 
                        $or:[{"country":"US"},{"country":"CA"}]
                       };
        
        if (req.query.longitude && req.query.latitude) {
          filters["loc"] = {$near: {$geometry : { type: "Point", coordinates:[parseFloat(req.query.longitude),parseFloat(req.query.latitude)]}}};
        }

        collection.find(filters).toArray(this);
      },
      function (err,data) {
        var data = data || [];

        //format the data
        var results = [];
        data.forEach(function(rec){

          // set the score for each result
          var score = 0;
          if (req.query.longitude && req.query.latitude) {

            //compare difference with the name, lat and lng
            namediff = utils.distance(req.query.q,rec.name);
            latdiff = utils.distance(req.query.latitude.toString(),rec.lat.toString());
            lngdiff = utils.distance(req.query.longitude.toString(),rec.long.toString());

            //average of the difference
            score = (namediff + latdiff + lngdiff) / 3;
          } else {
            score = utils.distance(req.query.q,rec.name)
          }

          results.push({"name":rec.ascii +", "+ (caCodes[rec.admin1] || rec.admin1)  +", " + (rec.country == "CA"?"Canada":"USA"),"latitude":rec.lat,"longitude":rec.long,"score":score.toFixed(2)})
        });

        if (results.length > 0) {
          //order the final result by score desc
          res.end(JSON.stringify({suggestions:results.sort(utils.compare_score_desc)}));
        } else {
          res.writeHead(404, {
            "Content-Type": "text/plain"
          });
          res.write(JSON.stringify({suggestions:[]}));
          res.end();
        }
       }
    );
  } else {
    res.end();
  }
});

//convert the lat long fields to geojson point
app.get('/converttogeo', function (req, res) {
    flow.exec(
      // Open the database.
      function () {
        this.db = new mongo.Db("heroku_app31052532", new mongo.Server("ds047940.mongolab.com",47940, {}), {safe:true});
        this.db.open(this);
      },
      function () {
        this.db.authenticate("kbbusbud", "tghnnibq", this);
      },
      // Get the collection
      function (err) {
        this.db.collection('cities', this);
      },
      // Get all records in the collection
      function (err, collection) {
        this.cities = collection;
        this.cities.find().toArray(this)
      },
      // update each record, add a geojson object that represents the location of the city
      function(err,data) {
        if (!err) {
          var collection = this.cities;
          data.forEach(function(record) {
            collection.update({_id:record._id},{$set:{"loc" : { type: "Point", coordinates: [ record.long, record.lat ] }}});
          });
        }
      }
    );
    res.end();
});
