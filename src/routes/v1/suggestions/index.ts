import { Request, Response, Router } from 'express';
import { SearchParams } from '../../../types/search';
import SearchService from '../../../services/search';

const search = new SearchService();

export default function (): Router {
  const router = Router();

  router.get('/', async (req: Request, res: Response) => {
    const params = req.query as SearchParams;
    try {
      const result = search.search(params);
      res.status(result.length ? 200 : 404).json({ suggestions: result });
    } catch (e) {
      res.status(400).json({ error: e.message });
    }
  });

  return router;
}
