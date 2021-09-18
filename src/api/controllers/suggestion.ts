import { Connection } from 'api/db';
import { Request, Response } from 'express';

export function makeSuggestionController(connection: Connection) {
  return new SuggestionController(connection);
}

class SuggestionController {
  connection;
  constructor(connection: Connection) {
    this.connection = connection;
  }
  async get(req: Request, res: Response) {
    try {
      const raw = this.connection.get(req.toString());
      res.status(200).send(raw);
    }
    catch (error) {
      res.status(400).send(error);
    }
  }
}
