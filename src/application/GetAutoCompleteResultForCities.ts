import AllCities from "../domain/cities/AllCities";
import Cities from "../domain/cities/Cities";
import { resolve } from "path";
import { rejects } from "assert";

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
          const view: AutoCompleteView = {
            suggestions: [
              {
                name: "gogo",
                latitude: "titi",
                longitude: "srtuie",
                score: 20
              }
            ]
          };

          resolve(view);
        });
    });

    return result;
  }
}
