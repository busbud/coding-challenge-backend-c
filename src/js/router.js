import React from "react";
import { Router } from "express";
import createLocation from "history/lib/createLocation";
import { RoutingContext, match } from "react-router";
import SuggestionsController from "./controllers/Suggestions";
import SuggestionService from "./services/SuggestionService";
import LocationRepo from "./repos/LocationRepo";
import routes from "./frontend/routes";
import toobusy from "toobusy-js";

function getRouter(db) {
  const locationRepo = new LocationRepo(db);
  const suggestionService = new SuggestionService(locationRepo);
  const suggestionsController = new SuggestionsController(suggestionService);

  const router = Router();

  router.use((req, res, next) => {
    /* If the server is overloaded, we should immediately stop requests rather than place too much
     * load on the cpu. */
    if (toobusy()) {
      res.status(503).json({
        "error": 503,
        "message": "The server is busy right now, please try your request again later"
      });
    }
    else {
      next();
    }
  });

  router.get("/suggestions", suggestionsController.get.bind(suggestionsController));

  /* Serve up the front-end web client. */
  router.use((req, res, next) => {
    let location = createLocation(req.url);

    match({ routes, location }, (error, redirectLocation, renderProps) => {
      if (redirectLocation) {
        res.redirect(301, redirectLocation.pathname + redirectLocation.search);
      }
      else if (error) {
        res.status(500).send(error.message);
      }
      else if (renderProps == null) {
        res.status(404).send('Not found');
      }
      else {
        const content = React.renderToString(<RoutingContext {...renderProps}/>);
        res.status(200).send(React.renderToString(
          <html>
            <head>
              <link href='//fonts.googleapis.com/css?family=Oswald:400,300,700|Open+Sans:400,300' rel='stylesheet' type='text/css' />
              <link href='/css/main.css' type='text/css' rel='stylesheet' />
            </head>
            <body>
              <div id="page" dangerouslySetInnerHTML={{__html: content}}></div>
              <script src="//maps.googleapis.com/maps/api/js?sensor=false" ></script>
              <script src="/js/client.js"></script>
            </body>
          </html>
        ))
      }
    });
  });

  return router;
}

export default getRouter;