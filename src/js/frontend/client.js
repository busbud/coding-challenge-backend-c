import React from "react";
import Router from "react-router";
import routes from "./routes";
import $ from "jquery";
import createBrowserHistory from '../../../node_modules/react-router/node_modules/history/lib/createBrowserHistory';

$(document).ready(() => {
  let history = createBrowserHistory();
  React.render(<Router history={history} routes={routes} />, document.getElementById("page"));
});
