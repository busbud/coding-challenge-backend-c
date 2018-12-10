import AllCities from "../domain/cities/AllCities";
import Cities from "../domain/cities/Cities";
import City from "../domain/cities/City";

export interface AutoCompleteQuery {
  name: string;
  longitude?: number;
  latitude?: number;
}

export interface SuggestionView {
  name: string;
  latitude: string;
  longitude: string;
  score: number;
}

export interface AutoCompleteView {
  suggestions: SuggestionView[];
}

export default class AutoCompleteResultForCities {
  private allCities: AllCities;

  constructor(allCities: AllCities) {
    this.allCities = allCities;
  }

  public async proceed(query: AutoCompleteQuery): Promise<AutoCompleteView> {
    const cities = await this.allCities.inUSAAndCanadaWithMoreThan5000People();

    const suggestions = cities
      .thatAutocompleteWith(query.name, query.longitude, query.latitude)
      .map((city: City): SuggestionView => cityToSuggestionView(city));

    return { suggestions };
  }
}

const cityToSuggestionView = (city: City): SuggestionView => ({
  name:
    city.getName() +
    ", " +
    city.getCountryCode() +
    ", " +
    city.getFeatureCode(),
  latitude: city.getLatitude().toString(),
  longitude: city.getLongitude().toString(),
  score: city.getScore()
});
