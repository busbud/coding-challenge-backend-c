
# Solution Explanation 
My aim at this challenge was to keep the solution as simple as possible. More specifically: 
- I chose to use express for that  reason, as I find it easier to handle routes with it.
- For the data, I used the tsv provided in the original challenge code and used event-stream to transform it to JSON. The code used for the conversion is included, but is not referenced in the final code. It could be used manually as the need to update the JSON arises. My assumption is that a database of cities does not change frequently, so I did not believe it was appropriate to create a cron job to constantly check the zip file. 
- To search for the data I used a json-query search module. I choose not to use a database  because I didn't think the data set was big enough to justify its use. 
- For the score, I initially only used the distance in km between the coordinates given and the city. As I confirmed that the name itself needed to influence the score, I created a very simple calculation to check how many letters from a city's name have been given already. A better approach would be to calculate the score based on the total number of cities in the database and the frequency that the word is searched by users.
- Taking into account high traffic, I included a redis cache that stores the URL. Because I am using the free version, I set the expiration to be 10 min only.

You can see the working solution deployed at [https://crondinini-coding-challenge.herokuapp.com/suggestions?q=montreal](https://crondinini-coding-challenge.herokuapp.com/suggestions?q=montreal).

# How to run the solution on your machine
**Assuming you already have git installed, these are the steps:** 
>> Clone repository 
```
git clone https://github.com/crondinini/coding-challenge-backend-c.git
```

>> Run npm install to download dependencies 
```
npm install
```

>> Start the server
```
npm start
```
>> Access URL 
http://127.0.0.1:2345/suggestions?q=montreal


# Original Text: 
# Busbud Coding Challenge 

## Requirements

Design an API endpoint that provides auto-complete suggestions for large cities.
The suggestions should be restricted to cities in the USA and Canada with a population above 5000 people.

- the endpoint is exposed at `/suggestions`
- the partial (or complete) search term is passed as a querystring parameter `q`
- the caller's location can optionally be supplied via querystring parameters `latitude` and `longitude` to help improve relative scores
- the endpoint returns a JSON response with an array of scored suggested matches
    - the suggestions are sorted by descending score
    - each suggestion has a score between 0 and 1 (inclusive) indicating confidence in the suggestion (1 is most confident)
    - each suggestion has a name which can be used to disambiguate between similarly named locations
    - each suggestion has a latitude and longitude
- all functional tests should pass (additional tests may be implemented as necessary).
- the final application should be [deployed to Heroku](https://devcenter.heroku.com/articles/getting-started-with-nodejs).
- feel free to add more features if you like!

#### Sample responses

These responses are meant to provide guidance. The exact values can vary based on the data source and scoring algorithm

**Near match**

    GET /suggestions?q=Londo&latitude=43.70011&longitude=-79.4163

```json
{
  "suggestions": [
    {
      "name": "London, ON, Canada",
      "latitude": "42.98339",
      "longitude": "-81.23304",
      "score": 0.9
    },
    {
      "name": "London, OH, USA",
      "latitude": "39.88645",
      "longitude": "-83.44825",
      "score": 0.5
    },
    {
      "name": "London, KY, USA",
      "latitude": "37.12898",
      "longitude": "-84.08326",
      "score": 0.5
    },
    {
      "name": "Londontowne, MD, USA",
      "latitude": "38.93345",
      "longitude": "-76.54941",
      "score": 0.3
    }
  ]
}
```

**No match**

    GET /suggestions?q=SomeRandomCityInTheMiddleOfNowhere

```json
{
  "suggestions": []
}
```


### Non-functional

- All code should be written in Javascript
- Mitigations to handle high levels of traffic should be implemented
- Challenge is submitted as pull request against this repo ([fork it](https://help.github.com/articles/fork-a-repo/) and [create a pull request](https://help.github.com/articles/creating-a-pull-request-from-a-fork/).
- Documentation and maintainability is a plus

### References

- Geonames provides city lists Canada and the USA http://download.geonames.org/export/dump/readme.txt
- http://www.nodejs.org/
- http://ejohn.org/blog/node-js-stream-playground/


## Getting Started

Begin by forking this repo and cloning your fork. GitHub has apps for [Mac](http://mac.github.com/) and
[Windows](http://windows.github.com/) that make this easier.

### Setting up a Nodejs environment

Get started by installing [nodejs](http://www.nodejs.org).

For OS X users, use [Homebrew](http://brew.sh) and `brew install nvm`

Once that's done, from the project directory, run

```
nvm use
```

### Setting up the project

In the project directory run

```
npm install
```

### Running the tests

The test suite can be run with

```
npm test
```

### Starting the application

To start a local server run

```
PORT=3456 npm start
```

which should produce output similar to

```
Server running at http://127.0.0.1:2345/suggestions
```
