"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.suggestionsRouter = void 0;
const express_1 = require("express");
const findCities_1 = require("./handlers/findCities");
exports.suggestionsRouter = (0, express_1.Router)();
exports.suggestionsRouter.get('/', findCities_1.findCities);
