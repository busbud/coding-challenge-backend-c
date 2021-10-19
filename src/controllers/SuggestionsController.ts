import { Request, Response, NextFunction } from "express"
import { classToPlain } from "class-transformer"
import { ISuggestions } from "models/interfaces/ISuggestions"
import { SuggestionsServices } from "services/SuggestionsServices"

export abstract class SuggestionsController {

    public static async GetSuggestions(req: Request, res: Response, next: NextFunction) {

        try {
            
            const {q, latitude, longitude}: {q: string; latitude: string; longitude:string} = req.query as any

            const suggestions: ISuggestions = await SuggestionsServices.RetrieveSuggestions(q, latitude, longitude);

            res.status((suggestions.suggestions.length > 0 ? 200 : 404))
                .send(classToPlain(suggestions))

        }
        catch(error) {

            if(process.env.NODE_ENV === "production") {
                res.status(500)
                    .send()
            }
            else {
                res.status(500)
                    .send(error)
            }
                
        }

        return
    }

}