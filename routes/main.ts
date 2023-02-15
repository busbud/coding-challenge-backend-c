import express, { Request, Response } from 'express';
const router = express.Router();

import { tsvToJsonArray } from '../lib/tsvConverter';
import { getSuggestions } from '../controllers/suggestion';



// Load cities from TSV file
const cities = tsvToJsonArray('./data/cities_canada-usa.tsv');
router.get("/", (req, res) => { res.send(cities); });




router.get('/suggestions', getSuggestions);


















module.exports = router;