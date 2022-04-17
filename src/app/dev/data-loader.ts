import 'reflect-metadata';
import TsvReader from '../../infra/tsv/reader';
import path from 'path';
import CityDTO from '../../domain/model/value-object/city-dto';
import { City } from '../../domain/model/entity/city';
import { IOrmClient } from '../../infra/orm/orm-client-interface';
import Container from 'typedi';
import { SequelizeClient } from '../../infra/orm/sequelize-client';

const tsvReader = new TsvReader();
const ormClient: IOrmClient = Container.get<IOrmClient>(SequelizeClient);

const records: unknown[] = tsvReader.read(path.resolve('data') + '/cities_canada-usa.tsv');

ormClient.connectDatabase();

const citiesDTO: CityDTO[] = records.map(
    (record: unknown) => new CityDTO(record)
)

for (const cityDTO of citiesDTO) {
    const city: City = City.createFromDTO(cityDTO);
    
    void city.save();
}