import { Response, Request } from 'express'; 

export default class RedirectController {

    handler(req: Request, res: Response): void {
      res.redirect(`https://jonasalessi.github.io/codechallenge-busbud-demo/`);
    } 
    
}