import fs from "fs";
import City from "../domain/cities/City";
import AllCities from "../domain/cities/AllCities";
import Cities from "../domain/cities/Cities";
import { promisify } from "util";

let cache;

export default class FileAllCities implements AllCities {
  async inUSAAndCanadaWithMoreThan5000People(): Promise<Cities> {
    if (cache) {
      return cache;
    }

    const cities = Cities.newWithoutCities();

    const fileEntry = await promisify(fs.readFile)(
      "./resources/cities_canada-usa.tsv",
      "utf8"
    );

    //process.nextTick() ?
    fileEntry
      .split("\n")
      .slice(1)
      .map(line => {
        const lineAsArray = line.trim().split("\t");

        return new City(
          lineAsArray[1],
          lineAsArray[8],
          lineAsArray[7],
          Number(lineAsArray[4]),
          Number(lineAsArray[5]),
          Number(lineAsArray[14])
        );
      })
      .filter((city: City) => city.getPopulation() > 5000)
      .forEach(city => cities.addCity(city));

    cache = cities;

    return cities;
  }
}
