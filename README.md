# Busbud Coding Challenge [![Build Status](https://travis-ci.org/nvnoskov/coding-challenge-backend-c.svg?branch=master)](https://travis-ci.org/nvnoskov/coding-challenge-backend-c)


## Comments
First of all prepare data at first start of the service and store it in memory (~ 1.5 mb)

### Stage 1: simple indexOf with coordinate distance.

Base on query we filter cities, than caclucate distance between cities and sort.

##### Tests:
```
ab -n1000 -c5 http://127.0.0.1:2345/suggestions?q=Londo&latitude=43.70011&longitude=-79.4163

Requests per second:    1220.62 [#/sec] (mean)
Time per request:       4.096 [ms] (mean)
Time per request:       0.819 [ms] (mean, across all concurrent requests)


ab -n10000 -c20 'http://127.0.0.1:2345/suggestions?q=Londo&latitude=43.70011&longitude=-79.4163'

Requests per second:    1639.61 [#/sec] (mean)
Time per request:       12.198 [ms] (mean)
Time per request:       0.610 [ms] (mean, across all concurrent requests)
```

### Stage 2: levenshtein distance with coordinate distance.
Levenshtein distance can forgive user for mistakes in queries

Trying calculate levenshtein distance to every record in memory, than caclulate coords distance, than sort.

```
ab -n1000 -c5 http://127.0.0.1:2345/suggestions?q=Londo&latitude=43.70011&longitude=-79.4163

Requests per second:    17.45 [#/sec] (mean)
Time per request:       286.577 [ms] (mean)
Time per request:       57.315 [ms] (mean, across all concurrent requests)

```
**Unacceptable performance!**


### Stage 3: indexOf + levenshtein distance with coordinate distance.

Previously we filter records by indexOf, than calculate levenshtain distance, than coords distance.

```
ab -n1000 -c5 http://127.0.0.1:2345/suggestions?q=Londo&latitude=43.70011&longitude=-79.4163

Requests per second:    1148.55 [#/sec] (mean)
Time per request:       4.353 [ms] (mean)
Time per request:       0.871 [ms] (mean, across all concurrent requests)


ab -n10000 -c20 'http://127.0.0.1:2345/suggestions?q=Londo&latitude=43.70011&longitude=-79.4163'

Requests per second:    1545.56 [#/sec] (mean)
Time per request:       12.940 [ms] (mean)
Time per request:       0.647 [ms] (mean, across all concurrent requests)

```

**This stage give us good performance with relevant results**

### Stage 4: trying improve performance.
![We have to go deeper](https://miro.medium.com/max/1024/1*cwR_ezx0jliDvVUV6yno5g.jpeg)

One of the tricks will be dividing our data to chunks based on name and search in a small piece of data, instead of a full list
I choose hash with 3 symbols length, but it can be changed just need more tests and experiments.


**Memory allocation in this case insreased from 14 mb to 27.5mb**

```
ab -n1000 -c5 'http://127.0.0.1:2345/suggestions?q=Londo&latitude=43.70011&longitude=-79.4163'

Requests per second:    2666.65 [#/sec] (mean)
Time per request:       1.875 [ms] (mean)
Time per request:       0.375 [ms] (mean, across all concurrent requests)

ab -n10000 -c20 'http://127.0.0.1:2345/suggestions?q=Londo&latitude=43.70011&longitude=-79.4163'

Requests per second:    5297.57 [#/sec] (mean)
Time per request:       3.775 [ms] (mean)
Time per request:       0.189 [ms] (mean, across all concurrent requests)

```
**Performance increased 3 times**



