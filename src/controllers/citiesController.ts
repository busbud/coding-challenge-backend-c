import axios from "axios";
import { citySuggestionsSchema } from "./citiesController.definition";
import { Request, Response } from "express";

import { CitiesService } from "../services/citiesService";
import _ from "lodash";

const citiesService = new CitiesService(axios);

export class CitiesController {
  static async citySuggestions(req: Request, res: Response) {
    try {
      const { q, lon, lat } = req.query;
      const { error, value } = citySuggestionsSchema.validate({
        cityName: q,
        longitude: lon,
        latitude: lat,
      });

      if (error) {
        res.status(400).json({ message: `Invalid Query Parameters: ${error}` });
        return;
      } else {
        const suggestions = await citiesService.getCitiesByQueryParam({ ...value });

        if (_.isEmpty(suggestions)) return res.status(404).json({ suggestions });

        return res.status(200).json({ suggestions });
      }
    } catch (error) {
      return res.status(500).json({ message: `Failed to get suggestions: ${error}` });
    }
  }
}
