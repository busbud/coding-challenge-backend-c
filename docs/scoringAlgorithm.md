# Scoring Algorithm for suggesting cities

## Jaro–Winkler Similarity Rating

> Jaro-Winkler similarity rating is a measure of similarity between two strings that is based 
> on the Jaro distance metric. 
> 
> It takes into account both the number of matching characters and transpositions between the strings, 
> as well as the prefix similarity (i.e., the number of characters at the beginning of the strings that match exactly). 
> 
> The Jaro-Winkler similarity rating ranges from 0 to 1, with higher values indicating a higher degree of 
> similarity between the strings. 

## Haversine Distance

> Haversine distance is a formula used to calculate the distance between two points on the surface of a sphere, 
> such as the Earth.
> 
> It takes into account the curvature of the sphere and calculates the distance based on the latitude and longitude 
> coordinates of the two points. 
> 
> The formula involves converting the coordinates to radians and applying a series of trigonometric functions. 
> The result is a more accurate measurement of distance than other distance metrics that assume a flat surface. 

## The Suggestions service uses the combination of above two algorithms to provide results

### Case 1: When only keyword for search is given and neither of latitude or longitude
In this scenario, the scoring of the suggested cities is completely based on the Jaro–Winkler Similarity Rating

### Case 2: When keyword for search is given along with both latitude and longitude
In this scenario, first the Jaro–Winkler Similarity Rating is calculated between the query word and the city name.
Then, Haversine Distance is calculated between the latitude and longitude given in the query and those of a city.

The two scores are combine as per the below formula:
```
score = Jaro-Winkler Similaity Rating * 0.7 + Haversine Distance * 0.3
```

The above formula, gives more weight to the Jaro-Winkler rating (0.1) than the Haversine Distance (0.3)

### Case 3: When keyword for search is given along with only one of either latitude or longitude
In this scenario, the scoring is again completely based off of Jaro-Winkler similarity rating just as in Case 1 
ignoring the latitude or longitude given.
