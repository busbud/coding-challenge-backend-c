# Busbud Coding Challenge

live: https://fathomless-atoll-1734.herokuapp.com/suggestions?q=montreal

## Design Assumptions

People who use this API can be grouped into two categories: those who spelled their destination name correctly and those who didn't. To greatly simplify the computations required by this API, I made a few assumptions to help me design the scoring algorithms.

- People who misspelled the city's name either made a typo or they legitimately don't kow how to spell the city's name.
- In the case that they made a typo and still sent the query, the typo must be towards the end of the query city name.
- In the scenario that they dont know how to spell the city's name, we should try matching the query and its substrings until we find a match. However, due to how quickly our confidence decreases as we use substrings to query matches, the query substring should no longer be relevant after just a few calls.
- The location of the user will only affect the user's search interests in a certain range. For this implementation, we will assume 300 km.
- Large cities are more likely to be queried, thus their scores should receive a bias.

## Implementation Details

Given an application which rarely updates but frequently accesses the database, I have chosen to go with MongoDB. I believe that async MongoDB's lean queries are one of the faster options available for this type of application. I used a script to populate the MongoDB database, removing fields which I didn't know what to do with and adding a few to make the most out of the fast accessing speed. 

Given a query string **q** of length **n**, I convert the special characters and I attempt to find city names starting with **q** and if I can't get any, I remove the last character of **q** and recursively query until I find suggestions or until the suggestions are no longer relevant. These suggestions are then given a score based off the levenshtein distance from the original query, haversine distance from the user, and the population count. 
