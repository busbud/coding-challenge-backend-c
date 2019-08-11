# Busbud Coding Challenge

### Deployment
The app the deployed at https://busbud-chao.herokuapp.com/

Example usage:
```
https://busbud-chao.herokuapp.com/suggestions?q=londo
```

```
https://busbud-chao.herokuapp.com/suggestions?q=Londo&latitude=43.70011&longitude=-79.4163
```

### Application
All cities data are stored in-memory. Cities returned have a name that starts with the querystring q provided. When latitude and longitude params are provided, string similariry and distance are used to calculated the score. When not provided, string similarity and population are used instead.

### Dependencies added
* n-readlines
* geopoint
* string-similarity
* remove-accents
* rate-limiter-flexible