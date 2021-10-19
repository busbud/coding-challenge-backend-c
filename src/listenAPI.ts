import { SuggestionsController } from "./controllers/SuggestionsController";
import express from "express"

const app = express();

module.exports = () => {

    // Listens for GET /suggestions responding with SuggestionsController.GetSuggestions method
    app.get('/suggestions', SuggestionsController.GetSuggestions);

    // Starts the server
    app.listen(Number(process.env.PORT) || 2345, process.env.HOST, () => {
        console.log("Server running at http://%s:%d/suggestions", process.env.HOST, Number(process.env.PORT) || 2345)
    });
}