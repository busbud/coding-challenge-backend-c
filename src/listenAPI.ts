import { SuggestionsController } from "./controllers/SuggestionsController";
import express from "express"
///const express = require('express');
const app = express();

module.exports = () => {

    app.get('/suggestions', SuggestionsController.GetSuggestions);

    app.listen(Number(process.env.PORT) || 2345, process.env.HOST, () => {
        console.log("Server running at http://%s:%d/suggestions", process.env.HOST, Number(process.env.PORT) || 2345)
    });
}