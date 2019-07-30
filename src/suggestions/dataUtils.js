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
const getRedisKey = q => {
  return getRegEx(q)
    .toString()
    .concat("-", "suggestions");
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
const fetchSuggestions = requestParams => {
  
  const query = [];

  if( requestParams.long && requestParams.lat) {
    query.push({      
      $geoNear: {
        query: {
          searchField: { $regex: getRegEx(requestParams.q) }          
        },        
        near: { 
           type: "Point",
           coordinates: [ Number(requestParams.long) , Number(requestParams.lat)]
         },         
         distanceField: "distance",         
         spherical: true,         

      }
    },)
  } else {    
    query.push({ $match: { searchField: { $regex: getRegEx(requestParams.q) } } },)
  }  

  query.push({
    $project: {        
      _id: 0,                        
      score: getScoreQuery(requestParams),        
      name: "$disambiguateName",
      longitude: "$long",
      latitude: "$lat"
    },
  })
    
  query.push({$sort: {score: -1}})
  query.push({$limit: 5})
  
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
const fetchSuggestionsCached = requestParams => {
  const redisKey = getRedisKey(requestParams.q);  
  return new Promise((resolve, reject) => {
    
      redisClient().get(redisKey, (err, reply) => {        
        if(err) {
          return reject(err);
        }
        if (reply) {
          return resolve(JSON.parse(reply));
        } else {
          fetchSuggestions(requestParams).then(documents => {
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
  if (typeof reqParams.q !== "string") {
    throw new Error("Param q is required");
  }
  const documents = await fetchSuggestionsCached(reqParams);
  return documents.map(roundScore)
};

module.exports = {  
  getRegEx,
  getRedisKey,
  fetchSuggestions,
  fetchSuggestionsCached,  
  getSuggestions
};
