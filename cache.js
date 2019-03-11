const redis = require('redis');
const redisURL = process.env.REDIS_URL || 'localhost';
const redisPort = process.env.REDIS_PORT || 6379;
const client = redis.createClient();

client.on('connect', () => console.log('Successfully connected to the redis database'));
client.on('error', (err) => console.error('Unable to connect to redis: ' + err));

module.exports =  (req, res, next) => {
        // generate the request key from our project and the requesting URL
        let requestKey = "__expressCityRequest__" + req.originalUrl || req.url;

        client.get(requestKey, function(err, reply){
            if (err) { res.status(500).send("Unable to successfully process result"); next(); }
            else{
                if(reply){ // We have data in the Redis Cache
                    console.log("Reading data from Redis cache");
                    let records = JSON.parse(reply);
                    if (records.length === 0)res.status(404); // Reasonable assume that if the cache is an empty array that it was due to an invalid result
                    if ('suggestions' in records){
                        if (records['suggestions'].length === 0)res.status(404);
                    }
                    res.json(records);
                }else{ // We do not have data, we pass the request and store to our cache
                    res.sendResponse = res.json;
                    // override the json method, and then reuse previous implementation to send data to client
                    res.json = (body) => {
                        client.set(requestKey, JSON.stringify(body));
                        res.sendResponse(body);
                    };
                    next();
                }
            }
        });
    };

