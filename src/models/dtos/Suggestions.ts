import { City } from "models/entities/City"
import { ISuggestion } from "../interfaces/ISuggestion"
import { ISuggestions } from "../interfaces/ISuggestions"
import { Suggestion } from "./Suggestion"

export class Suggestions implements ISuggestions {
    suggestions: ISuggestion[];

    constructor() {
        this.suggestions = new Array<Suggestion>();
    }

    public FromEntityCities(cities: Array<City>, longitude: string, latitude: string, cityQuery: string): ISuggestions {
        this.suggestions = cities.map((city: City) => new Suggestion().FromEntityCity(city, longitude, latitude, cityQuery))
        this.suggestions = this.suggestions.sort((a,b) => b.score - a.score)
        return this
    }

}