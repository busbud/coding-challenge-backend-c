import path from 'path';
import { Sequelize } from 'sequelize-typescript';
import config from '../../app/config';
import { IOrmClient } from './orm-client-interface';
import { Service } from 'typedi';
import { City } from '../../domain/model/entity/city';

@Service()
export class SequelizeClient implements IOrmClient {
    private sequelize: Sequelize;

    public connectDatabase(): void {
        this.sequelize = new Sequelize(config.database.url, {
            modelPaths: [path.resolve('domain/model/entity')],
        });

        this.loadModels();
        
        this.sequelize.authenticate().catch((error: Error) => {
            throw new Error('Unable to connect to the database: ' + error.message);
        });
    }

    private loadModels() {
        this.sequelize.addModels([
            City
        ]);
    }
}