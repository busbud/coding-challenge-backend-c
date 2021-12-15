import express, {Request, Response, Router} from "express";
import cityController from "./controller/city-controller";
import {City} from "./repository/city";

interface SuggestionRoute {
  q: string;
  latitude: number
  longitude: number
}

/**
 * Returns the router with all the routes available for this API.
 * @returns {Router} Express router
 */
export default (): Router => {
  const r = express.Router({
    strict: true,
  })

  r.get('/suggestions', (req: Request<{}, {}, {}, SuggestionRoute>, res: Response) => {
        let query = req.query.q;
        let lat = req.query.latitude;
        let long = req.query.longitude;

        let response = cityController.findAllCitiesScores(query, lat, long);

        // If there is no city found, return 404, otherwise return 200 and the values...
        if (!response.length) {  // Not Found
          res.status(404).json({
            suggestions: []
          });
        } else {                // Found
          res.status(200).json({
            suggestions: cityController.toDto(response)
          });
        }
      }
  )

  return r
}
