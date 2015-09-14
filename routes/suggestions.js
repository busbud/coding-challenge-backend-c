var algoliasearch       = require('algoliasearch');
var algoliasearchHelper = require('algoliasearch-helper');
var express             = require('express');
var jsonFormatter       = require('./jsonFormatter.js');

/**
 * Variables to improve accuracy
 */
const
    geoPrecision = 50,  //50K radius from geolocation is pretty precise
    hitsPerPage = 20;   //pass h.

/**
 * setting up the app
 */
const app = express();

/**
 * setting up algolia's helper for easier querying
 */
var algoliaclient = algoliasearch('P9V07AU042', '04d81a86a58d0632abba6885ef99a81a');
var algoliahelper = algoliasearchHelper(algoliaclient,'suggestions', {
    attributesToRetrieve: ['id','name','ascii', 'country', 'admin1', 'lat', 'long'],
    hitsPerPage: hitsPerPage, // can be modified from url if needed
    attributesToHighlight: 'none'
});

/**
 * the suggestions are being processing here
 * @param req
 * @param res
 */
module.exports = app.get('/suggestions', function(req, res) {
    var q           = req.query.q;          //client's city requested
    var h           = req.query.h;          //hits per page requested
    var lat         = req.query.latitude;   //latitude of geolocation
    var lon         = req.query.longitude;  //longitude of geolocation
    var g           = req.query.g;        //geolocation precision

    //TODO: implement security for query

    if (!q) {
        return res.status(400).send('Bad query.');
    }

    if(h>0) {
        algoliahelper.setQueryParameter('hitsPerPage',h);
    }

    if(g<=0 || !g) {
        g = geoPrecision;
    }

    console.log("Query Parameters:");
    console.log(lat);
    console.log(lon);
    console.log(h);
    console.log(g);

    performSearch(q, searchCallback);

    function performSearch(q, searchCallback) {
        algoliahelper.setQuery(q).search();
        algoliahelper.once('result', function (content) {
            searchCallback(content.hits);
        });
    }

    function searchCallback(content) {
        res.setHeader('Content-Type','application/json; charset=utf-8');
        if (content.length>0) {
            res.status(200);
        } else {
            res.status(404);
        }
        console.log("\nAbout to process Algolia's results: ");
        console.log(content);
        res.end(jsonFormatter(q,lat,lon,content,g));
    }
});