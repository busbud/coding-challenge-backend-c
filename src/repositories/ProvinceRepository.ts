import { EntityRepository, Repository } from "typeorm"
import { Province } from "models/entities/Province"

@EntityRepository(Province)
export class ProvinceRepository extends Repository<Province> {

}