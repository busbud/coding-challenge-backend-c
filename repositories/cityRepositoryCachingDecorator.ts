import { IRepository } from "./IRepository";
import { CityRepository } from "./cityRepository";
import { City } from "../models/city.model";

export class CityRepositoryCachingDecorator implements IRepository<City> {
  private items: City[];
  private repository: IRepository<City>;

  constructor(repository: CityRepository) {
    this.repository = repository;
  }

  getAll() {
    this.ensureCacheExists();

    return this.items;
  }

  private ensureCacheExists() {
    this.items || (this.items = this.repository.getAll());
  }
}
