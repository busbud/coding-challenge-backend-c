import { Suggestion } from "models/dtos/Suggestion"
import { Suggestions } from "models/dtos/Suggestions"
import { City } from "models/entities/City"
import { ISuggestion } from "models/interfaces/ISuggestion"
import { ISuggestions } from "models/interfaces/ISuggestions"
import { CityRepository } from "repositories/CityRepository"
import { getCustomRepository } from "typeorm"
import { CacheServices } from "./CacheServices"

export abstract class SuggestionsServices {
    
    public static async RetrieveSuggestions(cityQuery: string, longitude: string, latitude: string): Promise<ISuggestions> {
                
        try {

            if(!cityQuery || cityQuery.length < 3) {
                return new Suggestions()
            }

            var cacheResult: ISuggestions = await CacheServices.SearchSuggestions(cityQuery)

            if(cacheResult.suggestions.length == 0) {
                const cityRepository = getCustomRepository(CityRepository)
                const cities: Array<City> = await cityRepository.searchCitiesByName(cityQuery)
                const suggestions = new Suggestions().FromEntityCities(cities, longitude, latitude, cityQuery)
                CacheServices.StoreSuggestions(cityQuery, suggestions)
                
                return suggestions
            }
            else {
                cacheResult.suggestions = cacheResult.suggestions.map((city: Suggestion): ISuggestion => { 
                    Object.setPrototypeOf(city, Suggestion.prototype)
                    city.ReHidrateFromCache(longitude, latitude) 
                    return city
                } )
                cacheResult.suggestions = cacheResult.suggestions.sort((a,b) => b.score - a.score)
            }
            
            return cacheResult

        }
        catch(error) {
            console.error(error)
            throw error
        }
    }
}