const redisClient = require("../cache").redisClient;
const removeAccents = require("remove-accents");
const getScoreQuery = require("./score").getScoreQuery;
const mongo = require("../mongo").mongo;
const REDIS_EXPIRE = 60 * 60 * 24;

/**
 * Since there are transformations on the user input for the match on
 * location, make sure that the redis key is composed of the transformed
 * input to avoid having different keys with same content. Eg. NewY and New Y.
 *
 * @param { string } q
 * @returns { string }
 */
const getRedisKey = reqParams => {  
  let key = 'suggestions';
  const search =  getRegEx(reqParams.q).toString();
  key = key.concat("-", search);
  if (reqParams.long !== undefined && reqParams.lat !== undefined) {    
    key = key.concat('-', reqParams.long, '-', reqParams.lat);
  }  
  return key;
};

/**
 * Escape regex characters, remove accents, replace hawaiian quote by
 * standard quote
 *
 * @todo Try string.normalize(form)
 *
 * @param {string} q
 * @return {RegExp}
 */
const getRegEx = q => {
  q = q || "";
  q = q.replace(/[-\/\\^$*+?.()|[\]{}]/g, "\\$&");
  q = q.replace(/‘/g, "'");
  return new RegExp(removeAccents(q.replace(/\s/, "")), "i");
};

/**
 * Fetch suggestions from mongo based location name input
 *
 * @param {string} q
 */
const fetchSuggestions = reqParams => {
  
  const query = [];

  if( reqParams.long && reqParams.lat) {
    query.push({      
      $geoNear: {
        query: {
          searchField: { $regex: getRegEx(reqParams.q) }          
        },        
        near: { 
           type: "Point",
           coordinates: [ Number(reqParams.long) , Number(reqParams.lat)]
         },         
         distanceField: "distance",         
         spherical: true,         

      }
    },)
  } else {    
    query.push({ $match: { searchField: { $regex: getRegEx(reqParams.q) } } },)
  }  
  
  query.push({
    $project: {        
      _id: 0,                        
      score: getScoreQuery(reqParams),        
      name: "$disambiguateName",
      longitude: "$long",
      latitude: "$lat"
    },
  })
    
  query.push({$sort: {score: -1}})
  query.push({$limit: 5})  
  console.log(query);
  return mongo()
    .collection("locations")
    .aggregate(query)
    .toArray();
};

/**
 * Fetch suggestions from the db or use cache if key exist.
 *
 * @param {string} q
 */
const fetchSuggestionsCached = reqParams => {
  const redisKey = getRedisKey(reqParams);  
  return new Promise((resolve, reject) => {
    redisClient().get(redisKey, (err, reply) => {        
        if(err) {
          return reject(err);
        }
        if (reply) {         
          console.log('CACHED', redisKey) ;
          return resolve(JSON.parse(reply));
        } else {
          fetchSuggestions(reqParams).then(documents => {
            redisClient().set(
              redisKey,
              JSON.stringify(documents),
              "EX",
              REDIS_EXPIRE
            );
            resolve(documents);
          }).catch(err => {
            reject(err);            
          })          
        }
      });    
  });
};

/**
 * 
 * @param {object} row 
 */
const roundScore = (row) => {  
  row.score = Math.round(row.score * 10) / 10;
  return row;
}

/**
 * Get suggestions order by score.
 *
 * @param {object} reqParams
 */
const getSuggestions = async reqParams => {  
  const documents = await fetchSuggestionsCached(sanitizeRequestParams(reqParams));
  return documents.map(roundScore)
};

/**
 * Round long and la for cache and make sure mongo db doesn't use geoNear for 
 * invalid geo input.
 * 
 * @param {object} reqParams 
 */
const sanitizeRequestParams = function(reqParams) {  
  if (typeof reqParams.q !== "string") {
    throw new Error("Param q is required");
  }
  
  const output = {}
  output.q = reqParams.q;
  
  const long = isNaN(reqParams.long) ?  false : Math.round(parseFloat(reqParams.long));
  const lat = isNaN(reqParams.lat) ?  false : Math.round(parseFloat(reqParams.lat));
  
  if (long !== false && lat !== false) {
    output.long = long;
    output.lat = lat;
  }
  
  return output;
}

module.exports = {  
  getRegEx,
  getRedisKey,
  fetchSuggestions,
  fetchSuggestionsCached,  
  getSuggestions,
  sanitizeRequestParams,
};
