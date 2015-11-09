# Busbud Coding Challenge

## Design Assumptions

People who use this API can be grouped into two categories: those who spelled their destination name correctly and those who didn't. To greatly simplify the computations required by this API, I made a few assumptions to help me design the scoring algorithms.

- People who misspelled the city's name either made a typo or they legitimately don't kow how to spell the city's name.
- In the case that they made a typo and still sent the query, the typo must be towards the end of the query city name.
- In the scenario that they dont know how to spell the city's name, we should try matching the query and its substrings until we find a match. However, due to how quickly our confidence decreases as we use substrings to query matches, the query substring should no longer be relevant after just a few calls. We will stop once all the substrings from 0 to n, where n is half of the original query string.
- The location of the user will only affect the user's search interests in a certain range. For this implementation, we will assume 300 km.
- Large cities are more likely to be queried, thus their scores should receive a bias.