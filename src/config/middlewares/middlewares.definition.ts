/* eslint-disable @typescript-eslint/no-explicit-any */
import { RouteType } from "../../routes/routes.definition";

export interface MiddlewareType {
  path?: string;
  handler: any;
}

export interface AppServerType {
  registerMiddleware(middleware: MiddlewareType | Array<MiddlewareType>): void;
  registerRoute(route: RouteType | Array<RouteType>): void;
  getInstance(): any;
}
