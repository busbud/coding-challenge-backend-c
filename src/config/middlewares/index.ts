import { cache } from "./cacheMiddleware";
import bodyParser from "body-parser";

import { MiddlewareType } from "./middlewares.definition";

export const middleware: Array<MiddlewareType> = [
  { handler: bodyParser.json() },
  { handler: bodyParser.urlencoded({ extended: false }) },
  { handler: cache },
];
