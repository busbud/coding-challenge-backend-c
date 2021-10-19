import { EntityRepository, MoreThanOrEqual, Repository, ILike } from "typeorm"
import { City } from "models/entities/City"

@EntityRepository(City)
export class CityRepository extends Repository<City> {

    public async searchCitiesByName(citiesName: string): Promise<City[]> {

        try {

            return await this.find({
                relations: ["country","province"],
                where: {
                    name: ILike(`%${citiesName}%`),
                    population: MoreThanOrEqual(5000)
                }
            });

        }
        catch(error) {
            console.error(error)
            throw error
        }

    }

    public async getCitiesCount(): Promise<number> {

        try {
            return await this.count()
        }
        catch(error) {
            console.error(error)
            throw error
        }

    }

}