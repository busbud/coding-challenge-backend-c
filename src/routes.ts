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

        const response = cityController.getCitiesLikeName(query);

        if (response.length) {
          res.status(200).json({
            suggestions: cityController.toDto(response)
          });
        } else {
          res.status(404).json({
            suggestions: []
          });
        }
      }
  )

  return r
}
