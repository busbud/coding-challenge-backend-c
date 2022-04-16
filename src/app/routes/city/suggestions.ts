import { Application, Request, Response } from 'express';
import { celebrate, Joi, Segments } from 'celebrate';

export default (app: Application) => {
    app.get(
        '/suggestions',
        celebrate({
            [Segments.QUERY]: Joi.object({
                q: Joi.string().optional(),
                latitude: Joi.number().optional(),
                longitude: Joi.number().optional(),
                minPopulation: Joi.number().default(5000)
            })
            // ensure positions params are populated with their peer
            .with('latitude', 'longitude')
            .with('longitude', 'latitude'),
        }),
        (request: Request, response: Response) => {
            //entity city load from json
            //5000 people limit
            
            return response.status(200).json({"salut": "toto"});
        }
    );
};
