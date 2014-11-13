# Approach

I formulated a plan after reading the instructions. This plan aims at
resolving the challenge using the described technologies.

## Plan:

*Geo data parsing*

1.   Understand how the data is formatted and where it is located.

2.   Build a simple data parser with output: JSON object to work with.

3.   Complete comments and unit tests for this parsing activity.
Small clean-up...


*Request query parsing*

1.  Lay down the details for the input query: edge cases, etc.

2.  Design algorithm for score.

3.  Build a simple query parser with output: JSON object to work with.
(note: probably given with req)

4.  Build a simple algorithm.

5.  Build specific test cases with the data provided to try edge cases.

6.  Complete comments and unit tests for this part. Small clean-up...


*Final*

1. Review solution.


## Notes

###Assumptions:

TSV file can change over time. For example, the population can change.

###Questions:

1. Can Geonames change fields?

Since no version number in files, I 
guess it might, unless when you query these files you specify some
version. Let us assume no modification for this exercise.

###Do not forget:

Error mgmt

##Timesheet:

*Tue Nov 11*

9:00 - 11:42 (5min pause) Parse TSV function completed.

13:09 - 16:41 (3x5min pauses) Finished parsing core 
functionalities...remains clean-up and fixing comparison.

I thought I would have finished the challenge in the morning. The
problems with string-matching on city names and the calculation of the
score took more time than I estimated.

18:45 - 20:47 (45min pause) Learn Heroku and deploy app, url: https://coding-challenge-backend-c.herokuapp.com/suggestions?q=sthilaire&latitude=45.56678&longitude=-73.19915

*Thu Nov 13*

9:00 - 12:05 (2x5min pauses) Finished refactoring into model classes. Attacking more unit tests.
12:30 - 2:15 (10min pause) Finished unit tests. Added TODO in code for further enhancements.

Total: 2h 40m + 3h 15m + 1h 15m + 2h 45m + 1h 45m + 30m = 12h 15m


##Estimation:

*Tue Nov 11 11:43*

Almost three hours for parsing the TSV and getting familiar with the
problem.

I estimate three hours to do the queryString and the algorithm logic.



*Tue Nov 11 16:42*

Finished some prototype with hackish comparison to make tests pass for
Montreal...containing a special character for the "e".

I need three to four more hours to find a better string-comparison 
solution, clean code (review overall solution), add more 
tests for maintenance.

Still in the mood of the Startup Weekend...


