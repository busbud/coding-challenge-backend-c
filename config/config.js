


// There is no differences between environments for the config
// For this challenge, we took the same variables so the config file is not separated by environment
// But we could have something like this. (but not usefull for now)
// - config
//    - default.js      // the basic config file
//    - dev.js          // you can override all basic config information just for your dev env
//    - staging.js      // you can override all basic config information just for your staging env
//    - production.js   // you can override all basic config information just for your production env
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