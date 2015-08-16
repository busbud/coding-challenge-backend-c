'use strict';

import slug         from 'slug';
import cities       from '../data/cities-canada-usa';
import * as scoring from './suggestions/scoring';
import {trie}       from './suggestions/matching';
import {normalized} from './suggestions/engine';

// Scoring engine with different scorers and weights
const score = scoring.engine([
  [scoring.length, 0.2],
  [scoring.population, 0.4],
  [scoring.distance, 0.4]
]);

const engine = normalized(s => slug(s, {lower: true}));
// const suggestions = normalized(s => s.toLowerCase());

export default engine(c => c.name, trie, score, cities);
