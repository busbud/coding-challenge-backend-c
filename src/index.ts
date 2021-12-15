import server from "./server";
import CityController from "./controller/city-controller";
import {cityRepository} from "./repository/city"
import path from "path";
import Server from "./server";
import http from "http";

const port = process.env.PORT || 2345;

// Startup
const app = new Server().init()

http.createServer(app).listen(port, () => {
  console.log(`Server running at http://localhost:${port}`);

  console.log('Loading the cities ...');
  // Loading the cities ...
  cityRepository.loadCities(path.join(process.cwd(), 'data', 'cities_canada-usa.tsv'))
      .then(() => console.log('done!'))
})
