import http from "http";
import { ExpressServer } from "./config";
import config from "./config/env";

const PORT = config.port;
const app = new ExpressServer().getInstance();

http.createServer(app).listen(PORT, "127.0.0.1");
console.log("Server running at http://127.0.0.1:%d/suggestions", PORT);

export default app;
