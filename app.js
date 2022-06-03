import express from "express";
import { getSuggestions } from "./src/suggestions.js";

const app = express();
const port = process.env.PORT || 2345;

const emptyResponse = {suggestions: []};

app.get("/suggestions", async (req, res) => {
  const input = req.query.q;

  if (input && input !== "") {
    const results = await getSuggestions(input);

    if (results.suggestions.length === 0) {
      res.status(404).send(emptyResponse);  
    }

    res.send(results);    
  } else {
    res.status(404).send(emptyResponse);
  }
});

app.listen(port, () => {
  console.log(`Server running at http://127.0.0.1:${port}/suggestions`);
});

export default app;