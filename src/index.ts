import server from "./server";
import CityController from "./controller/city-controller";
import { cityRepository } from "./repository/city"
import path from "path";
import Server from "./server";
import http from "http";

const port = process.env.PORT || 2345;

// Startup
cityRepository.loadCities(path.join(process.cwd(), 'data', 'cities_canada-usa.tsv'))
    .then(() => {
      const app = new Server().init()

      http.createServer(app).listen(port, () => {
        console.log(`Server running at http://localhost:${port}`);
      })
    })
