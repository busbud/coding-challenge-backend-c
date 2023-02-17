import express, { Request, Response } from 'express';
const router = express.Router();

import { getSuggestions } from '../controllers/suggestion';

router.get("/", (req, res) => { res.send('Welcome'); });

router.get('/suggestions', getSuggestions);


module.exports = router;