import { EntityRepository, Repository } from "typeorm"
import { Country } from "models/entities/Country"

@EntityRepository(Country)
export class CountryRepository extends Repository<Country> {

}