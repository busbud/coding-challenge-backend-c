# Busbud Coding Challenge

#### Submitted By: Feras Allaou

## How to get Started

- Clone the Repo.
- run `npm install` to install all the dependencies.
- run `npm run start` to get the App working. Default port should be 3101 and it will be mentioned in the log.
- visit `localhost:3101/suggestions` to access the API.
- you can search using the `q` param. i.e `localhost:3101/suggestions?q=lon`
- you can also add coordinates to your search. i.e `localhost:3101/suggestions?q=Lond&latitude=43.70011&longitude=-79.4163`

#### How to develop

- Each service lives in a separate folder inside the **Services** folder. in our case, we have Suggestions as a service.
- You can create a **route.js** file to define your service's routes, and then define your handlers inside a separate file **Suggestions.js** in our case.
- Finally, you can just instruct your app to use those resources inside **app.js**.
- To test, simply run `npm run test`
