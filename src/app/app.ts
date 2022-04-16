import HttpServer from '../infra/http/http-server';
import Routes from './routes';

export default class App {
    public run() {
        const httpServer = new HttpServer();

        httpServer.init();

        // load routes
        Routes(httpServer.getServer());

        httpServer.run();
    }
}
