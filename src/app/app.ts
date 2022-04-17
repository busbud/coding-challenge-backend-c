import { IOrmClient } from '../infra/orm/orm-client-interface';
import { SequelizeClient } from '../infra/orm/sequelize-client';
import { Container } from 'typedi';
import HttpServer from '../infra/http/http-server';
import Routes from './routes';

export default class App {
    public run() {
        const ormClient: IOrmClient = Container.get<IOrmClient>(SequelizeClient);
        const httpServer: HttpServer = new HttpServer();

        ormClient.connectDatabase();
        httpServer.init();

        // load routes
        Routes(httpServer.getServer());

        httpServer.run();
    }
}
