# Busbud Coding Challenge

## Brainstorm Keywords

Trie, RadixTree, PatriciaTree, LevenshteinDistance, KDTree, Fuzziness, N-Grams, Normalization, PreProcessing, Redis.

## Searching Algorithm

* An optimized implementation for the `Trie` DataStructure (Check `app/helpers/trie.js`).
* Support for `fuzziness` and storing objects instead of just words by specifying a specific key of the objects as lookup word.
* Calculate the `EditDistance`.
* Tolerance with mssing or extra letters.
* Interesting implementation in term of compile optimization.
* Insert the cities sorted in descending order by population to enhance the prefix expaning.
* Create the `Trie` at the first request and use it for future requests.
* Normalize the keys and the search qurey:
  * Lowercase.
  * Remove any characters that are not either a letter or space.
  * Remove duplicate sapces.
  * Remove prefix and suffix white spaces.
* Configurable (Check `app/config/config.json`:
  * **fuzziness**: the maximum edit distance.
  * **prefixLength**: the number of initial letters which will not be `fuzzified` to reduce the number of examined keys.
  * **maxResults**: the max number of returned results.

## Scoring

### General Notes

* Configurable weigts (Check `app/config/config.json`).
* Give a full score for the not applied criterias.
* If one of the criterias is set to 0, it will not be applied.

### Scoring criterias: 

#### population

* Give the full population score to the highest population city, otherwise give a partial score relatively to the highest population.

#### prefixUniqueness:

* Give the full prefixUniqueness score if the key is a unique prefix among all other cities' keys prefixes.

#### lengthMatching:
* Give the full lengthMatching score to the exact length matching, otherwise give a partial score relatively to the length difference.

#### editDistance:

* Give the full editDistance score when editDistance = 0, otherwise give a partial score relatively to the editDistance.
* It doesn't apply if the fuzziness matching option is set to 0.

#### geoDistance

* Give the full geoDistance score to the nearest city, otherwise give a partial score relatively to the distance.
* It doesn't apply if the user's geo location is not passed or if we have just one suggestion.

## Cool Things

* DI module.
* More types of middlewares (before, service, controller, after, fail).
* Fully documented code, have fun reading the code :).
* Separate the validation from the logic the validation from module.
* Run a cluster of the app via PM2, configured in `app/config/config.json`.
* More unit testing cases (total 71).

## Bad Things

Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod
tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam,
quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo
consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse
cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non
proident, sunt in culpa qui officia deserunt mollit anim id est laborum.

## Notes

* I used the minimal parts of a framework that I built from around 3 years. Some parts of the code read a revamp since they have bad implementation, consistency, naming, documentation like `router.js`, `response.js`, `tasks.js`, `helpers/validate.js`, etc.

# Getting Started

## Setting up the project

In the project directory run

```
npm install
```

## Running the tests

The test suite can be run with

```
npm test
```

## Starting the application

This project runs via PM2 as cluster, you can configure
the number of cluster in the `app/config/config.json` file

```
npm start
```

# About Me

Mohammad Fares, Senior Software Engineer at Amazon.

* CV: [https://goo.gl/fu1Zcf](https://goo.gl/fu1Zcf)
* LinkedIn: [https://linkedin.com/in/faressoft](https://linkedin.com/in/faressoft)
* GitHub: [https://github.com/faressoft](https://github.com/faressoft)
* GitHubGist: [https://gist.github.com/faressoft](https://gist.github.com/faressoft)

My Best Articles:

* [Scalability Overview, Terms, and Methodologies](https://goo.gl/oxS3MG)
* [Locks, Mutex, Semaphore, Deadlock, Starvation](https://goo.gl/FT8A3P)
* [DOM Performance (Reflow & Repaint)](https://goo.gl/cfjAQr)

My Best Project

* [Mohmal.com](https://www.mohmal.com/en)
