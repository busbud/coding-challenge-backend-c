# Place Finder

This API suggest the name of cities with latitude, longitude and a score  that represent the confidence of suggestion.

## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes. See deployment for notes on how to deploy the project on a live system.

```
* Clone the repository from git
* To run locally though CLI : npm start
```
### Prerequisites

What things you need know to run the code and how to install them

* [Node v12.13](https://nodejs.org/)
* [GeoNames-Client](https://github.com/kinotto/geonames.js/)


## Deployment

Currently the application is deployed on [heroku](https://dashboard.heroku.com/apps), at this [link](https://badal-busbud.herokuapp.com/suggestions).

### Consuming API
Once the application is running locally, open postman or a browser and call GET end-point of this application.
```
http://localhost:2345/suggestions?q=london&longitude=-63.2505&latitude=-17.33866
```
**q** is the name of the place the client want to search.<br>
**longitude** and **latitude** are optional parameters that a user can provide to get better suggestions.
