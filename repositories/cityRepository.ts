import { IRepository } from "./IRepository";
import { City } from "../models/city.model";
import * as fs from "fs";

export class CityRepository implements IRepository<City> {
  getAll() {
    return fs
      .readFileSync("./data/cities_canada-usa.tsv", "utf8")
      .split("\n")
      .map(this.parseCity);
  }

  private parseCity(cityJson: string): City {
    const columnValus = cityJson.split("\t");
    if (columnValus.length !== 19)
      throw new Error("The formatting of the row in the data was wrong.");

    return {
      id: +columnValus[0],
      name: columnValus[1],
      ascii: columnValus[2],
      alt_name: columnValus[3],
      lat: +columnValus[4],
      long: +columnValus[5],
      feat_class: columnValus[6],
      feat_code: columnValus[7],
      country: columnValus[8],
      cc2: columnValus[9],
      admin1: columnValus[10],
      admin2: columnValus[11],
      admin3: columnValus[12],
      admin4: columnValus[13],
      population: +columnValus[14],
      elevation: +columnValus[15],
      dem: columnValus[16],
      tz: columnValus[17],
      modified_at: new Date(columnValus[18])
    };
  }
}
