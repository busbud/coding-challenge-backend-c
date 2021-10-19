import { RedisClient, createClient } from "redis"
import { classToPlain } from "class-transformer"
import { Suggestions } from "models/dtos/Suggestions"
import { ISuggestions } from "models/interfaces/ISuggestions"
import { promisify } from 'util'

const CAHCE_PREFIX = "busbud:searchresults:"
const createCacheClient = (): RedisClient => {
    //url: "redis://:p4b8b55258aa17aeb78d7180095bd50541ac8527751abf88ac7f154ff4b2c9096@ec2-44-194-253-34.compute-1.amazonaws.com:27229"
    const redisClient: RedisClient = createClient({ 
        url: process.env.REDIS_URL 
    })
    
    return redisClient
}

export abstract class CacheServices {

    public static async StoreSuggestions(searchString: string, results: ISuggestions) {

        try {
            const cache: RedisClient = createCacheClient()
            cache.set(`${CAHCE_PREFIX}${searchString}`, JSON.stringify(classToPlain(results)))
        }
        catch {

        }
        
    }

    public static async SearchSuggestions(searchString: string) {

        try {
            const cache: RedisClient = createCacheClient()
            const getAsync = promisify(cache.get).bind(cache);
            var cachedSearch: Suggestions = JSON.parse(await getAsync(`${CAHCE_PREFIX}${searchString}`)) as Suggestions || new Suggestions()
            return cachedSearch
        }
        catch {
            return new Suggestions()
        }
        
    }


}
