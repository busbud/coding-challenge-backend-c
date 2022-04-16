import HttpServer from '../infra/http/http-server';

export default class App {
    public run() {
        const httpServer = new HttpServer();

        httpServer.init();
        // load routes
        httpServer.run();
    }
}
