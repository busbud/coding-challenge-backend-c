# Busbud Coding Challenge

Here's my shot at the Busbud coding challenge!

## Fuzzy String Matching

To implement a performant solution for this kind of problem we first need to prune our search space before we do the distance
calculation for the best candidates.

### First Step: Search Space Pruning

We first have to create an index using n-gram to prune our search space. Here an example for q=Montreal

```
Index size is 7238

After N-gram pruning: 266

After N-gram with an error limit (a maximum of 2 n-grams can be missing): 78
```

### Second Step: Scoring Function (Jaro-Winkler)

After the pruning, we should get a more manageable number of candidates, but still there's some
optimization possible (see the presentation of Seth Verrinder & Kyle Punam). We can use an approximation of
the Jaro-Winkler distance without the need of the number of transpositions, it gives us an upperbound on the score.

The optimization is based on the assumption that we can compute the number of matching characters way more efficiently
than the fullblown Jaro-Winkler distance. We can achieve that by encoding our query and index entry in a number and using
bitwise operations on them to find how many characters from the smallest string are not in the biggest one. The optimization
doesn't seem to be that important in javascript (there seems to be a lot of conversion between Number(float) -> Int -> Number(float)),
but it still improves the result on the benchmark. Also it gives a good boost when we are searching really long terms without
results.

```
After Jaro-Winkler upperbound filtering: 43 (down from 78)
```

Now we are able to use our costly distance computing on only 43 candidates, filter them with a score limit and ordered them.

### Third Step (optional): Score using location if available

Last step, but an optional one, we use a squared equirectangular approximation, because we don't need the real distance
but a total order between the values. Then we penalise a part of the total score (10%) with the distance ratio (current distance/maximum distance).

## Possible Improvements

- Add alternates names to the index
- Better handling of special characters
- Better handling of admin1 code and country in the index
- More test cases


### References

- Jaro–Winkler distance Wikipedia article https://en.wikipedia.org/wiki/Jaro%E2%80%93Winkler_distance
- What's in a Name? Fast Fuzzy String Matching - Seth Verrinder & Kyle Putnam - Midwest.io 2015 https://www.youtube.com/watch?v=s0YSKiFdj8Q