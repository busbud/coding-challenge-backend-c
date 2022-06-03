import express from "express";
import { getSuggestions } from "./src/suggestions.js";

const app = express();
const port = process.env.PORT || 2345;

const EMPTY_RESPONSE = {suggestions: []};
const NOT_FOUND = 404;

app.get("/suggestions", async (req, res) => {
  const input = req.query.q;

  if (input && input !== "") {
    const results = await getSuggestions(input);

    if (results.suggestions.length === 0) {
      res.status(NOT_FOUND).send(EMPTY_RESPONSE);  
    }

    res.send(results);    
  } else {
    res.status(NOT_FOUND).send(EMPTY_RESPONSE);
  }
});

app.listen(port, () => {
  console.log(`Server running at http://127.0.0.1:${port}/suggestions`);
});

export default app;