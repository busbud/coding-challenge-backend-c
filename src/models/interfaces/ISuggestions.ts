import { City } from "models/entities/City";
import { ISuggestion } from "./ISuggestion"

export interface ISuggestions {
    suggestions: Array<ISuggestion>

    FromEntityCities: (cities: Array<City>, longitude: string, latitude: string, cityQuery: string) => ISuggestions
}