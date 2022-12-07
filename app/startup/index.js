const getDirectories = require("./getDirectories");
const mainDirectory = "endpoints";
const cache = require("../cache");

module.exports = (app) => {
  // get all directories from `mainDirectory`
  const directories = getDirectories(mainDirectory);

  // for each directory get all the endpoints
  for (let dir of directories) {
    const endpoints = getDirectories(`${mainDirectory}/${dir}`);

    for (let endpoint of endpoints) {
      // get the endpoint config
      const {
        Method,
        Routes,
      } = require(`../../${mainDirectory}/${dir}/${endpoint}/config.json`);

      // get the endpoint handler
      const handler = require(`../../${mainDirectory}/${dir}/${endpoint}`);

      // add endpoint to express router
      if (Method.toLowerCase() === "get") {
        app[Method.toLowerCase()](
          `/${dir}/${endpoint}`,
          cache(process.env.CACHE_TTL),
          async (req, res, next) => {
            // get scopes from endpoints config
            // run endpoint handler
            handler({ req, res, next })
              .then((response) => {
                res
                  .status(response?.status || 200)
                  .send(
                    response && typeof response === "object"
                      ? response
                      : { success: true }
                  );
              })
              .catch((err) => {
                next(err);
              });
          }
        );
      } else {
        app[Method.toLowerCase()](
          `/${dir}/${endpoint}`,
          async (req, res, next) => {
            // get scopes from endpoints config
            // run endpoint handler
            handler({ req, res, next })
              .then((response) => {
                res
                  .status(response?.status || 200)
                  .send(
                    response && typeof response === "object"
                      ? response
                      : { success: true }
                  );
              })
              .catch((err) => {
                next(err);
              });
          }
        );
      }

      // add custom route
      /**
       * Optional: to get shorter routes or add params in url
       */
      if (Routes) {
        for (let route of Routes) {
          route = route.replace(/^\/|\/$/g, "");
          if (Method.toLowerCase() === "get") {
            app[Method.toLowerCase()](
              `/${route}`,
              cache(process.env.CACHE_TTL),
              async (req, res, next) => {
                // get scopes from endpoints config
                handler({ req, res, next })
                  .then((response) => {
                    res
                      .status(response?.status || 200)
                      .send(
                        response && typeof response === "object"
                          ? response
                          : { success: true }
                      );
                  })
                  .catch((err) => {
                    next(err);
                  });
              }
            );
          } else {
            app[Method.toLowerCase()](`/${route}`, async (req, res, next) => {
              // get scopes from endpoints config
              handler({ req, res, next })
                .then((response) => {
                  res
                    .status(response?.status || 200)
                    .send(
                      response && typeof response === "object"
                        ? response
                        : { success: true }
                    );
                })
                .catch((err) => {
                  next(err);
                });
            });
          }
        }
      }
    }
  }
};
