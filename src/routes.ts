import express, {Request, Response, Router} from "express";
import cityController from "./controller/city-controller";
import {City} from "./repository/city";

interface SuggestionRoute {
  q: string;
  latitude: number
  longitude: number
}

export default (): Router => {
  const r = express.Router({
    strict: true,
  })

  r.get('/suggestions', (req: Request<{}, {}, {}, SuggestionRoute>, res: Response) => {
        let query = req.query.q;
        let lat = req.query.latitude;
        let long = req.query.longitude;

        let response = cityController.getCitiesLikeName(query);

        // Sort by lat lon if they exist
        if (lat && long) {
          response = cityController.sortByDistance(response, lat, long)
        }

        // If there is no city found, return 404, otherwise return 200 and the values...
        if (response.length) {  // Found
          res.status(200).json({
            suggestions: cityController.toDto(response)
          });
        } else {                // Not found
          res.status(404).json({
            suggestions: []
          });
        }
      }
  )

  return r
}
