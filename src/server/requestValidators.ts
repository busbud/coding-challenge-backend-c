import {NextFunction, Request, Response} from 'express';

/** Validates GET suggestions query parameters */
export const getSuggestionRequestValidator = (req: Request, res: Response, next: NextFunction) => {
  const coordinateRegex = /^(\\+|-)?(90(\.0+)?|[0-8]?\d(\.\d+)?)$/;
  const regexExp = new RegExp(coordinateRegex);

  const q = req.query.q as string;
  const latitude = req.query.latitude as string;
  const longitude = req.query.longitude as string;

  if (!q || (q && !q.length)) {
    res.status(400)
    res.json({error: 'q must not be empty'});
    res.end();
    return;
  }

  if (!latitude && !longitude) {
    next();
  } else if (latitude && longitude && regexExp.test(latitude) && regexExp.test(longitude)) {
    next();
  } else {
    res.status(400)
    res.json({error: 'latitude and longitude must be provided and be valid'});
    res.end();
    return;
  }
};
