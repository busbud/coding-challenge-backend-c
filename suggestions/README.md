# Suggestions

This directory contains the modules concerned with getting suggestions.

## To do:

If I were to spend more time on this, I would:

- Put the data into a database to make it read faster
- Look into cursors to see if that could speed up reading from the file stream
- Implement more complex score functions like
  - partial match (some characters of query)
  - evaluate match without punctuation
  - generate alternative spellings of query term in case it has a typo
- I hardcoded an if condition to check for Canadian data rows in order to convert region numbers to province abbreviations
- Take a closer look at the reverse-geolocation library to make sure it's reliable and safe

## Features:

**Configurable score weights** - in the file `config.json` you can change the numbers in the score object to make the results of those score functions weighted higher.  Numbers are relative to each other, so any number is fine.

**Configurable population limit** - the population limit was specified as "greater than 5000", but in the config file you can change that number.

**Separate model and controller** - I use those terms loosely, but the data is read in one function, scored against an array of scoring functions, and then evaluated against weights and formatted into results in another function.  So if someone were to change the data source to a database, the scoring functions and weighting function wouldn't necessarily need to change.  Additional scoring functions can be really easily added to the file `score-imports.js`

**Improving results based on historical results** - The "popularity" score counts how many times a city in the data store was returned as a top 3 result. Future queries consider that count against the total number of historical results to potentially increase the score of the new result.

## Notes:

**BIG NOTICE**: I absolutely acknowledge that reading the `.tsv` directly was a bad idea; on my computer the query takes something like 5 minutes to run.  Ultimately I couldn't even get the tests to pass because they timed out even after setting a 5 minute timeout in the package.json.  I fixed them up a bit, though, and think they would pass if I changed the data source so that the queries run faster.

I did not mitigate for high levels of traffic, because it can't even handle low levels of traffic.  If I were to implement mitigations, I would not use the file system for storage (for the cache or the data source), I might index the common columns and write the match scoring directly into the query, I might implement sessions and access control to limit the potential query sources, and I might rate-limit queries from the same source.

I initially tried to stick to ECMA Script 2009 as per the super old Node version, but I mistakenly thought Promises were from that spec after I was too far gone, so I upgraded the Node version.

I chose rocket-store as a datastore library for the cache because I had used it previously and knew it was super minimal.

I made a judgement call to enforce a minimum of 3 characters for the search term to be evaluated for exact-match and near-match.