import Benchmark     from 'benchmark';
import cities        from '../data/cities-canada-usa';
import * as matching from '../src/suggestions/matching';

const items = matching.build(c => c.full_name, cities);
const trie  = matching.trie(items);
const dumb  = matching.dumb(items);

(new Benchmark.Suite())
  .add('trie', () => {
    trie('Montréal');
  })
  .add('dumb', () => {
    dumb('Montréal');
  })
  .on('cycle', event => {
    console.log(String(event.target));
  })
  .run();
