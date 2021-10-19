import { RedisClient, createClient } from "redis"
import { Suggestions } from "models/dtos/Suggestions"
import { ISuggestions } from "models/interfaces/ISuggestions"
import { promisify } from 'util'

const CAHCE_PREFIX = "busbud:searchresults:"
const createCacheClient = (): RedisClient => {
    const redisClient: RedisClient = createClient({ 
        url: process.env.REDIS_URL 
    })
    
    return redisClient
}

export abstract class CacheServices {

    // Store the last search result in cache
    public static async StoreSuggestions(searchString: string, results: ISuggestions) {

        try {
            const cache: RedisClient = createCacheClient()
            cache.set(`${CAHCE_PREFIX}${searchString}`, JSON.stringify(results))
        }
        catch(error) {
            throw error
        }
        
    }
    
    //Searche for cached suggestions 
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
