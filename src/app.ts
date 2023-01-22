import http from "http";
import { ExpressServer } from "./config";
import config from "./config/env";

const hostUrl = config.environment === "development" ? "127.0.0.1" : "0.0.0.0";

const app = new ExpressServer().getInstance();

http.createServer(app).listen(config.port, hostUrl);
console.log(`Server running at http://${hostUrl}:${config.port}/suggestions`);

export default app;
