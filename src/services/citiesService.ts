import _ from "lodash";
import {
  calculateLocationAccuracyScoreByCoordinates,
  findSpecificAddressInformation,
} from "./../helper";
import {
  ACCEPTED_COUNTRIES,
  ACCEPTED_COUNTRIES_ENUM,
  CitiesDTO,
  ICitiesService,
  GetCitiesByQueryParamParam,
  GetSuggestionAccuracyByLatAndLongParam,
  ACCEPTED_MIN_POPULATION,
  LocationBreakdownResponse,
} from "./cities.definition";
import config from "../config/env";
import { tsvParse } from "d3-dsv";
import * as path from "path";
import * as fs from "fs";
import * as stringSimilarity from "string-similarity";
import { IApiService } from "../shared/apiService";

const __dirname = path.resolve(path.dirname(""));
const { googleApiKey, baseUrl } = config.googleMap;

export class CitiesService implements ICitiesService {
  private _apiService: IApiService;

  constructor(apiService: IApiService) {
    this._apiService = apiService;
  }

  /**
   * Get City Suggestions By Query Param
   */
  public async getCitiesByQueryParam(param: GetCitiesByQueryParamParam): Promise<CitiesDTO[]> {
    const { cityName, longitude, latitude } = param;
    const tsvFilePath = path.resolve(__dirname, "data/cities_canada-usa.tsv");
    try {
      const fileContent = await fs.readFileSync(tsvFilePath, { encoding: "utf-8" });

      const [...result] = await tsvParse(fileContent, (location) => {
        const paramContainsCoordinates = !_.isNil(longitude) || !_.isNil(latitude);

        if (
          !location?.name ||
          !location?.country ||
          !ACCEPTED_COUNTRIES.includes(location?.country)
        )
          return;

        const searchNameMatchCity = _.includes(
          location?.ascii?.toLowerCase(),
          cityName.toLowerCase()
        );

        if (searchNameMatchCity && Number(location?.population) > ACCEPTED_MIN_POPULATION) {
          const suggestedLocationCoordinates = {
            latitude: Number(location?.lat),
            longitude: Number(location?.long),
          };
          const locationCountry = location?.country || "";
          const locationState = _.isNaN(Number(location?.admin1)) ? location?.admin1 : null;

          return {
            name: `${location?.name}, ${locationState}, ${ACCEPTED_COUNTRIES_ENUM[locationCountry]}`,
            ...suggestedLocationCoordinates,
            score: paramContainsCoordinates
              ? this.getSuggestionAccuracyByLatAndLong({
                  suggestedLocationCoordinates,
                  longitude: Number(longitude),
                  latitude: Number(latitude),
                })
              : this.getSuggestionAccuracyByName(cityName, location?.name),
          };
        }
        return;
      });

      const formattedResult = !_.isNull(googleApiKey)
        ? await this.getDetailedAddress(result)
        : result;
      const suggestedCities = _.orderBy(formattedResult, ["score"], ["desc"]);

      return suggestedCities;
    } catch (err) {
      throw new Error("Failed to get city suggestions");
    }
  }

  /**
   *
   * Function: Gets
   * @param cities
   * @returns CitiesDTO[]
   */
  private async getDetailedAddress(cities: CitiesDTO[]): Promise<CitiesDTO[]> {
    return Promise.all(
      cities.map(async (city) => {
        const cityName = city?.name?.split(",");

        if (!_.isNaN(Number(cityName[1]))) {
          const locationbreakdown = await this.getLocationBreakdown({
            ...city,
          });

          return {
            ...city,
            name: `${cityName[0]}, ${locationbreakdown?.state}, ${cityName[2]} `,
          };
        }
        return city;
      })
    ).then((data) => data);
  }

  /**
   * Get breakdown of address by city, state and country using coordinates
   */
  private async getLocationBreakdown({
    latitude,
    longitude,
  }: {
    latitude: number;
    longitude: number;
  }): Promise<LocationBreakdownResponse> {
    try {
      const {
        data: { results },
      } = await this._apiService.get(
        `${baseUrl}?latlng=${latitude},${longitude}&sensor=${true}&key=${googleApiKey}`
      );

      const addressInformation = results[0].address_components;
      return {
        city: findSpecificAddressInformation(addressInformation, "locality") || "",
        state:
          findSpecificAddressInformation(addressInformation, "administrative_area_level_1") || "",
        country: findSpecificAddressInformation(addressInformation, "country") || "",
      };
    } catch (err) {
      throw new Error("Failed to get commit feeds");
    }
  }

  private getSuggestionAccuracyByName(queryName: string, suggestedName: string): number {
    return stringSimilarity.compareTwoStrings(suggestedName, queryName);
  }

  private getSuggestionAccuracyByLatAndLong(param: GetSuggestionAccuracyByLatAndLongParam): number {
    return calculateLocationAccuracyScoreByCoordinates({ ...param });
  }
}
