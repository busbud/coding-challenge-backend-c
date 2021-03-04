# Busbud Coding Challenge - PHP

## Matching Mechanism
In first place the app will try to get the cities which start with q value. To make faster I created an index on Name Column.

In case no record is founded, the app will make a new fetch using soundex function which is a phonetic algorithm for indexing names after English pronunciation of sound.
For example : ttp://localhost:80/suggestions?q=qebec

## Score Algorithm

It calculates the score based on two parameters :
- A : The distance between the city and the user location if we have it.
    - I split the distance to 3 categories : short, medium and long. 
    - I used Haversine Formula to calculate the distance, then based on the distance category I can assign a score value to that city.
- B : The similarity between q and the city's name using PHP similar_text function.

Then I generate the new score : 40% of A + 60 of B.
and in order to keep the score between 0 and 1, I normalize it using xi−min(x) / max(x)−min(x).

## Validation
- q : required, at least one character.
- latitude : optional, it should be between -90 and 90.
- longitude : optional, it should be between -180 and 180.
- For any endpoint diffrent then '/suggestions', the user will get : HTTP/1.1 404 NOT FOUND error.
- For Invalid inputs, the response will be : HTTP/1.1 422 Unprocessable Entity

## Working in Local
Using Docker, you can run the application in your local :
      
    docker-compose up -d --build

You can stop the application using :

    docker-compose down

Using Adminer you connect to MySQL Server : http://localhost:8080/
    - username : root
    - password : root

Data file : data/script.sql

## Test
You can find test cases in test folder.
After change your directory to the app directory, you run it using :
      
    php vendor/bin/phpunit test/SuggestionTestCases.php

## Heroku Link

https://city-suggestion-challenge.herokuapp.com/suggestions?q=Londo&latitude=43.70011&longitude=-79.4163

## Future Improvement
- Use .env file for Global variable and Database credentials.
- Use a PHP framework like Phalcon or Lavale, so we can easily handle the validations, the filter, dependency injection...
- We can use Elasticsearch.
- Define a better Scoring Algorithm after pursuing the user search behavior.
