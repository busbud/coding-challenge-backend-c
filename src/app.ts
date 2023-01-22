import http from "http";
import { ExpressServer } from "./config";
import config from "./config/env";

const PORT = config.port;
const app = new ExpressServer().getInstance();

const httpServer = http.createServer(app);

httpServer.listen(PORT, "127.0.0.1", () => {
  console.log(`🚀 Server running at http://127.0.0.1:${PORT}/suggestions`);
});

export default app;
