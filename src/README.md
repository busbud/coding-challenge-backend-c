# Approach

I formulated a plan after reading the instructions. This plan aims at
resolving the challenge using the described technologies.

## Plan:

*Geo data parsing*

.   Understand how the data is formatted and where it is located.
.   Build a simple data parser with output: JSON object to work with.
.   Complete comments and unit tests for this parsing activity.
Small clean-up...


*Request query parsing*

.  Lay down the details for the input query: edge cases, etc.
.  Design algorithm for score.
.  Build a simple query parser with output: JSON object to work with.
(note: probably given with req)
.  Build a simple algorithm.
.  Build specific test cases with the data provided to try edge cases.
.  Complete comments and unit tests for this part. Small clean-up...


*Final*

. Review solution.


## Notes

###Assumptions:

TSV file can change over time. For example, the population can change.

###Questions:

1. Can Geonames change fields? Since no version number in files, I 
guess it might, unless when you query these files you specify some
version. Let us assume no modification for this exercise.

###Do not forget:

Error mgmt

##Timesheet:

*Tue Nov 11*

9:00 - 11:42 (pause de 5min) Parse TSV function completed.

13:09 - 16:41 (trois pauses de 5min) Finished parsing core 
functionalities...remains clean-up and fixing comparison.

I thought I would have finished the challenge in the morning. The
problems with string-matching on city names and the calculation of the
score took more time than I estimated.


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


