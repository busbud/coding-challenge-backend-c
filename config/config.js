module.exports = {


    googleAPIkey : '',
    dataFileLocation : "./data/cities_canada-usa.tsv",
    mongoUrl : 'mongodb://coding-challenge:busbud@ds019766.mlab.com:19766/heroku_z7p8f5ck',

    suggestions : {

        // the max results returned for suggestions
        maxResults : 5,

        // the default radius to use for suggestions
        radius     : 50000
    }


}