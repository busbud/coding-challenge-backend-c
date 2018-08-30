# Busbud Coding Challenge

[Heroku link here.](https://coding-challenge-mchughs.herokuapp.com/).

#### Sample queries
  [/suggestions returns empty array](https://coding-challenge-mchughs.herokuapp.com/suggestions)

  [empty search term returns empty array](https://coding-challenge-mchughs.herokuapp.com/suggestions?q=)

  ['New ' should have New York as the top hit](https://coding-challenge-mchughs.herokuapp.com/suggestions?q=New+)

  ['La' should have 'Laval' at or near the top](https://coding-challenge-mchughs.herokuapp.com/suggestions?q=La)

  ['New ' with latitude and longitude near New Orleans returns New Orleans with a higher score than New York](https://coding-challenge-mchughs.herokuapp.com/suggestions?q=New&latitude=29.95&longitude=-90.10)
