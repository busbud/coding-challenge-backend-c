import { Application, Request, Response } from 'express';
import { celebrate, Joi, Segments } from 'celebrate';
import { City } from '../../../domain/model/entity/city';
import { Op } from 'sequelize';
import { IScorerStrategy } from '../../../domain/scorer/scorer-strategy-interface';
import { NameScorerStrategy } from '../../../domain/scorer/name-scorer-strategy';
import { CityPresenter } from '../../../domain/model/value-object/city-presenter';
import { SuggestionSearchCriteria } from '../../../domain/model/value-object/suggestion-search-criteria';
import { PositionScorerStrategy } from '../../../domain/scorer/position-scorer-strategy';
import { NameAndPositionScorerStrategy } from '../../../domain/scorer/name-and-position-scorer-strategy';

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
        async ({ query: { q, latitude, longitude, minPopulation } }: Request, response: Response) => {
            const searchCriteria: SuggestionSearchCriteria = new SuggestionSearchCriteria(String(q), Number(latitude), Number(longitude));
            const cities: City[] = await City.findAll({
                where: {
                    name: {
                        [Op.substring]: q ?? ''
                    },
                    population: {
                        [Op.gte]: minPopulation
                    }
                }
            });
            let scorerStrategy: IScorerStrategy = null;

            if (q && !latitude && !longitude) {
                scorerStrategy = new NameScorerStrategy();
            }

            if (!q && latitude && longitude) {
                scorerStrategy = new PositionScorerStrategy();
            }

            if (q && latitude && longitude) {
                scorerStrategy = new NameAndPositionScorerStrategy();
            }

            const cityPresenters: CityPresenter[] = scorerStrategy
                .executeStrategy(cities, searchCriteria)
                .sort((a, b) => (a.score < b.score) ? 1 : -1)
            ;

            return response.status(200).json(cityPresenters);
        }
    );
};
