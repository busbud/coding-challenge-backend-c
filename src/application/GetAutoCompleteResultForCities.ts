import AllCities from "../domain/cities/AllCities";
import Cities from "../domain/cities/Cities";
import { resolve } from "path";
import { rejects } from "assert";
import City from "../domain/cities/City";

export interface AutoCompleteQuery {
  name: string;
  longitude?: number;
  latitude?: number;
}

export interface Suggestion {
  name: string;
  latitude: string;
  longitude: string;
  score: number;
}

export interface AutoCompleteView {
  suggestions: Suggestion[];
}

export default class GetAutoCompleteResultForCities {
  private allCities: AllCities;

  constructor(allCities: AllCities) {
    this.allCities = allCities;
  }

  public async execute(query: AutoCompleteQuery): Promise<AutoCompleteView> {
    const result: Promise<AutoCompleteView> = new Promise((resolve, reject) => {
      this.allCities
        .inUSAAndCanadaWithMoreThan5000People()
        .then((cities: Cities) => {
          const suggestions: Suggestion[] = cities
            .thatAutocompleteWith(query.name, query.longitude, query.latitude)
            .map(
              (city: City): Suggestion => ({
                name:
                  city.getName() +
                  ", " +
                  city.getCountryCode() +
                  ", " +
                  city.getFeatureCode(),
                latitude: city.getLatitude().toString(),
                longitude: city.getLongitude().toString(),
                score: city.getScore()
              })
            );

          const view: AutoCompleteView = {
            suggestions
          };

          resolve(view);
        })
        .catch(error => console.log(error));
    });

    return result;
  }
}
