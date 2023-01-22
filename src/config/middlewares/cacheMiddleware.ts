/* eslint-disable @typescript-eslint/no-explicit-any */
import { Request, Response, NextFunction } from "express";
import _ from "lodash";
import NodeCache from "node-cache";

const nodeCache = new NodeCache({ stdTTL: 15 });

/**  
 * Caching Middleware that:
    - checks the node cache using the url as the key to determine if query has been made before
        - if found: pulls data from the cache and returns response without hitting the controller
        - if not found: proceeds to hit the controller and:
            - stores response returned in the cache
            - returns response
 */
export const cache = (req: Request, res: Response, next: NextFunction) => {
  const key: string = req.url;
  if (nodeCache.has(key)) {
    const result = nodeCache.get(key);

    return res.status(200).json(JSON.parse(result as string));
  } else {
    (res as any).sendResponse = res.send;
    (res as any).send = (body: any) => {
      nodeCache.set(key, body);
      return (res as any).sendResponse(body);
    };
  }
  return next();
};
