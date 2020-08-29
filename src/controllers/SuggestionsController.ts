import { Response, Request } from "express";

export default class SuggestionsController {

    handler(req: Request, res: Response) {
        res.status(404).send({
            suggestions: []
        });
    }

}