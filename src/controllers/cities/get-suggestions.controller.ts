import { Request, Response } from 'express';

export async function getCitySuggestions(_req: Request, res: Response) {
  res.writeHead(404, { 'Content-Type': 'text/plain' });

  res.end(
    JSON.stringify({
      suggestions: [],
    })
  );
}
