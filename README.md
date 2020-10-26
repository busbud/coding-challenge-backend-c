# Busbud Coding Challenge

 * For original challenge, see [original-challenge.md](original-challenge.md)
 * I mixed in a single document the writings of my personal experience completing this challenge and a tech description of the solution. Refer to [BusBud challenge writeup](https://docs.google.com/document/d/1acW0nhGTuzQXl7P3GmLfveAHAMVr8E48-Gjb7QESx1U/edit#heading=h.m18w3iroo2pt).
 
## Heroku deploy 

* Solution has been deployed to https://busbud-challenge-brunorey.herokuapp.com/
   * Can be either navigated in a browser or queried with curl, for example with the following command
   * `curl "https://busbud-challenge-brunorey.herokuapp.com/suggestions/?q=Londo&latitude=43.70011&longitude=-79.4163" | jq`

## Local deploy 

* To build & run locally using `npm`
   * `npm i`
   * `npm run test`
   * `npm run start`
 * To build & run locally using `docker`
   * `docker build . -t busbud_challenge_brunorey`
   * `docker run -p 2345:2345 busbud_challenge_brunorey`
 * For both options you can then test it by running the following `curl` command
   * `curl "http://127.0.0.1:2345/suggestions/?q=Londo&latitude=43.70011&longitude=-79.4163" | jq`
