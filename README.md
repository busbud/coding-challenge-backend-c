# Busbud Coding Challenge

## Requirements

Design an API endpoint that provides autocomplete suggestions for large cities.
The suggestions should be restricted to cities in the USA and Canada with a population above 5000 people.

-   the endpoint is exposed at `/suggestions`
-   the partial (or complete) search term is passed as a query string parameter `q`
-   the caller's location can optionally be supplied via query string parameters `latitude` and `longitude` to help improve relative scores
-   the endpoint returns a JSON response with an array of scored suggested matches
    -   the suggestions are sorted by descending score
    -   each suggestion has a score between 0 and 1 (inclusive) indicating confidence in the suggestion (1 is most confident)
    -   each suggestion has a name which can be used to disambiguate between similarly named locations
    -   each suggestion has a latitude and longitude
-   all functional tests should pass (additional tests may be implemented as necessary).
-   the final application should be [deployed to Heroku](https://devcenter.heroku.com/articles/getting-started-with-nodejs).
-   feel free to add more features if you like!

#### Sample responses

These responses are meant to provide guidance. The exact values can vary based on the data source and scoring algorithm.

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

-   All code should be written in Javascript, Typescript or PHP.
-   Mitigations to handle high levels of traffic should be implemented.
-   Challenge is submitted as pull request against this repo ([fork it](https://help.github.com/articles/fork-a-repo/) and [create a pull request](https://help.github.com/articles/creating-a-pull-request-from-a-fork/)).
-   Documentation and maintainability is a plus.

## Dataset

You can find the necessary dataset along with its description and documentation in the [`data`](data/) directory.

## Evaluation

We will use the following criteria to evaluate your solution:

-   Capacity to follow instructions
-   Developer Experience (how easy it is to run your solution locally, how clear your documentation is, etc)
-   Solution correctness
-   Performance
-   Tests (quality and coverage)
-   Code style and cleanliness
-   Attention to detail
-   Ability to make sensible assumptions

It is ok to ask us questions!

We know that the time for this project is limited and it is hard to create a "perfect" solution, so we will consider that along with your experience when evaluating the submission.

## Getting Started

### Prerequisites

You are going to need:

-   `Git`
-   `nvm` (or your preferred node version manager)
-   `Node.js`

### Setting up your environment

1. Begin by forking this repo and cloning your fork. GitHub has apps for [Mac](http://mac.github.com/) and
   [Windows](http://windows.github.com/) that make this easier.

2. Install [nvm](https://github.com/nvm-sh/nvm#install--update-script) or your preferred node version manager.

3. Install [Node.js](http://www.nodejs.org).

### Setting up the project

In the project directory run the `setup.sh` bash script. It may require administrator privileges to run.

### Running the tests

The test suite can be run with:

```
npm run test
```

### Starting the application

To start a local server run:

```
npm run start
```

it should produce an output similar to:

```
Server running at http://127.0.0.1:2345/suggestions
```

### Evolution Paths

The project has just been updated to leverage the most recent LTS version of node (16.14.2).

Due to current time constraints, the project could be upgraded to the industry best practices. Here's a few suggestions :

#### Switch to ES6 style import/export

I've quickly tried to switch from the `require` syntax to the ES6 `import/export` syntax, but had a few issues with the test setup and preferred focusing on delivering features

#### Add support for typescript

Leveraging the typescript type system would be a big plus for future maintainability and developer experience. If time permits, fully switching to it would be the best, but a cheaper, short-term, alternative would be to allow mix-and-matching js and ts files.

#### Update the scoring algorithms

Being strapped for time, the scoring algorithms are heavily limited and are not production-ready. However, they are isolated and would be easy to change. Also notice that the tests validating them are using relative comparison (instead of hard-coding generating scores) to ensure future changes would not break them

#### Assertions

Being unfamiliar with the provided assertion libraries, I wasn't able to leverage their full potential. Normalizing them with the libraries' best practices could be a small plus.
