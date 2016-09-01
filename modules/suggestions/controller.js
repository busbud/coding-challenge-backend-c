'use strict';

var City = require('../../models/city');

var suggestionsController = class SuggestionsController {

    get(req, res) {

        res.status(200);

        var queryParameter = req.query.q.toString();

        var query = City.find({});

        query
            .where({ '$or' : [{
                name : new RegExp(queryParameter, 'i')
                }, {
                alt_name : new RegExp(queryParameter, 'i')
            }]})
            .exec(function(error, citiesSuggest) {



            // Return a 500 response with the error object
            if(error)
               return res.status(500).send({type:'error', error: error});

            // If the `name` criteria fetch no records
            // Set the response header to 404
            if(citiesSuggest.length == 0)
                res.status(404);

            // Send the results
            return res.json({
                suggestions : citiesSuggest
            });
        })
    }
};

module.exports = new suggestionsController();