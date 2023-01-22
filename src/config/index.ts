/* eslint-disable @typescript-eslint/no-explicit-any */
import express from "express";
import { routes } from "../routes";
import { RouteType } from "../routes/routes.definition";
import { middleware } from "./middlewares";
import { AppServerType, MiddlewareType } from "./middlewares/middlewares.definition";

/**
 * Express Server
 * Provides abstraction for application server
 */
export class ExpressServer implements AppServerType {
  private readonly _app: any;

  /**
   * Constructor
   */
  constructor() {
    this._app = express();

    if (middleware.length) {
      this.registerMiddleware(middleware);
    }
    if (routes.length) {
      this.registerRoute(routes);
    }
  }

  /**
   * Register Middleware
   * @param  {MiddlewareType|Array<MiddlewareType>} middleware
   */
  registerMiddleware(middleware: MiddlewareType | Array<MiddlewareType>) {
    let middlewares: MiddlewareType[] = [];
    middlewares = middlewares.concat(middleware || []);

    middlewares.forEach((middleware: MiddlewareType) => {
      // eslint-disable-next-line @typescript-eslint/ban-types
      let handlers: Function[] = [];
      handlers = handlers.concat(middleware.handler);

      this._app.use(...handlers);
    });
  }

  /**
   * Register Route(s)
   * @param  {IRoute|Array<RouteType>} route
   */
  registerRoute(route: RouteType | Array<RouteType>): void {
    let routes: RouteType[] = [];
    routes = routes.concat(route || []);

    routes.forEach((route: RouteType) => {
      this._app[route.method](route.path, route.handler);
    });
  }

  /**
   * Get express app
   */
  getInstance(): any {
    return this._app;
  }
}
