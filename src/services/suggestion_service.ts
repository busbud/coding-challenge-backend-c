import {ICityRawData} from "../interfaces/raw_cities";


interface IGetCitySuggestion {
    name: string
    latitude: string
    longitude: string
    score: number
}

interface IGetSuggestionsResponse {
    suggestions: Array<IGetCitySuggestion> }


export class CitiesSuggestionService {
    #citiesData: ICityRawData[];
  constructor(cities: ICityRawData[]) {
    this.#citiesData = cities;
  }

  getSuggestions(searchString: string, minPopulation=5000, countries= ["US", "CA"]): IGetCitySuggestion[] {
      const citiesWithSimilarityScore: [ICityRawData, number][] = this.getCities(searchString, minPopulation, countries);
      return citiesWithSimilarityScore.map((cityScore): IGetCitySuggestion => {
          return {
              name: cityScore[0].name,
              latitude: cityScore[0].latitude,
              longitude: cityScore[0].longitude,
              score: cityScore[1]
          }
      });
  }

  getSuggestionsWithCoordinates(searchString: string, latitude: string, longitude: string, minPopulation=5000, countries= ["US", "CA"]): IGetCitySuggestion[] {
    const citySuggestions = this.getSuggestions(searchString, minPopulation, countries);
    return this.scoreByDistance(citySuggestions, latitude, longitude);
  }

    private getCities(searchString: string, minPopulation: number, countries: string[]) {
        return [];
    }

    private scoreByDistance(citySuggestions: IGetCitySuggestion[], latitude: string, longitude: string) {
        return [];
    }
}

