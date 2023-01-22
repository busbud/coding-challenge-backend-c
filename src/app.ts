import http from "http";
import { ExpressServer } from "./config";
import config from "./config/env";

const app = new ExpressServer().getInstance();

http.createServer(app).listen(config.port);
console.log("Server running at http://127.0.0.1:%d/suggestions", config.port);

export default app;
